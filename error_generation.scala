package ncreep.duplicate_fields

import ncreep.common.*

import scala.compiletime.*
import scala.compiletime.ops.any.*
import scala.compiletime.ops.boolean.*
import scala.compiletime.ops.int.{+ => _, *}
import scala.compiletime.ops.string.*
import scala.deriving.Mirror

import Tuple.*

inline def checkDuplicateFields[T <: Tuple]: Unit =
  // Not possible to assign to a `val` as type inference loses precise type information
  inline makeLabellings[T] match
    case _: Typed[labels] =>
      // can't extract into a type alias, as it breaks `constValue`
      renderDuplicatesError[OnlyDuplicates[GroupLabels[FlatMap[labels, ZipLabels]]]]

inline def renderDuplicatesError[T <: Tuple] =
  inline erasedValue[T] match
    case _: NonEmptyTuple => error(constValue["Duplicate fields found:\n" ++ RenderError[T]])
    case _ => ()

// although we can do this at the value-level, the 'error' functions limits us to using only
// string literals and '+', so working at the value-level doesn't simplify much, and the type-level
// we can use higher-level functions like `map` and `fold`
type RenderError[T <: Tuple] =
  // using the custom ++ instead of + because + is bound to work only on <: String, and we can prove that here
  Fold[Map[T, RenderGroup], "", ++]

type RenderGroup[T] <: String = T match
  case (label, sources) => "- " ++ label ++ " from " ++ MkString[sources] ++ "\n"

// "unsafe" if running on an empty tuple
type MkString[T <: Tuple] =
  Fold[Init[T], Last[T], [a, b] =>> a ++ ", " ++ b]

type ZipWithConst[T <: Tuple, A] = Map[T, [t] =>> (t, A)]

type GroupLabels[T <: Tuple] <: Tuple = T match
  case EmptyTuple => EmptyTuple
  case (label, source) *: t =>
    ((label, source *: FindLabel[label, t])) *: GroupLabels[RemoveLabel[label, t]]

type Size[T] <: Int = T match
  case EmptyTuple => 0
  case x *: xs => S[Size[xs]]

type OnlyDuplicates[T <: Tuple] = Filter[T, [t] =>> Size[Second[t]] > 1]

type FindLabel[Label, T <: Tuple] =
  Filter[T, [ls] =>> HasLabel[Label, ls]] Map Second

type RemoveLabel[Label, T <: Tuple] =
  Filter[T, [ls] =>> ![HasLabel[Label, ls]]]

type Second[T] = T match
  case (a, b) => b

type HasLabel[Label, LS] <: Boolean = LS match
  case (l, s) => Label == l

class Labelling[Label <: String, ElemLabels <: Tuple]

type IsMirror[M <: Mirror] <: Mirror = M match
  case Mirror => M

class Typed[A]:
  type Value = A

transparent inline def makeLabellings[T <: Tuple]: Typed[_ <: Tuple] =
  inline erasedValue[T] match
    case EmptyTuple => Typed[EmptyTuple]
    case _: (h *: t) =>
      // the pattern-matches below are needed to force the compiler to use the most precise
      // type for the values we are summoning
      inline makeLabellings[t] match
        case _: Typed[ls] =>
          inline summonInline[Mirror.Of[h]] match
            case m: IsMirror[m] =>
              // doing this redundant assignment to satisfy the compiler:
              // https://github.com/lampepfl/dotty/issues/18010
              val x = Typed[Prepend[Labelling[m.MirroredLabel, m.MirroredElemLabels], ls]]

              x

// a workaround for *: not working in certain cases:
// https://github.com/lampepfl/dotty/issues/18011
type Prepend[X, +Y <: Tuple] <: Tuple = X match
  case X => X *: Y

type ZipLabels[L] <: Tuple = L match
  case Labelling[label, elemLabels] => ZipWithConst[elemLabels, label]

type ++[A, B] <: String = (A, B) match
  case (a, b) => a + b