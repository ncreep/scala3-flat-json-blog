package ncreep.error_generation

import scala.Tuple.*
import scala.compiletime.*
import scala.compiletime.ops.string.*
import scala.deriving.Mirror

inline def checkDuplicateFields[A](using mirror: Mirror.ProductOf[A]): Unit =
  // Not possible to assign to a `val` as type inference loses precise type information
  inline makeLabellings[mirror.MirroredElemTypes] match
    case labels => renderDuplicatesError[labels.Value]

inline def renderDuplicatesError[Labellings <: Tuple] =
  type Duplicates = FindDuplicates[Labellings]
  
  inline erasedValue[Duplicates] match
    case _: EmptyTuple => ()
    case _: (h *: t) => error(constValue[RenderError[h *: t]])

// although we can do this at the value-level, the 'error' function limits us to using only
// string literals and '+', so working at the value-level doesn't simplify much, and at the type-level
// we can use higher-level functions like `map` and `fold`
type RenderError[LabelsWithSources <: Tuple] =
  // using the custom ++ instead of + because + is bound to work only on <: String, and we can't prove that here
  "Duplicate fields found:\n" ++ 
    Fold[RenderLabelsWithSources[LabelsWithSources], "", [a, b] =>> a ++ "\n" ++ b]

type RenderLabelsWithSources[LabelsWithSources <: Tuple] = 
  Map[LabelsWithSources, RenderLabelWithSource]

type RenderLabelWithSource[LabelWithSources] <: String = LabelWithSources match
  case (label, sources) => "- [" ++ label ++ "] from [" ++ MkString[sources] ++ "]"

// "unsafe" if running on an empty tuple
type MkString[T <: Tuple] =
  Fold[Init[T], Last[T], [a, b] =>> a ++ ", " ++ b]

// A more loosely-typed version of `+`
type ++[A, B] <: String = (A, B) match
  case (a, b) => a + b