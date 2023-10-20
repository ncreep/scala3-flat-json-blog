package ncreep.error_generation

import scala.Tuple.*
import scala.compiletime.*
import scala.deriving.Mirror

trait Labelling[Label <: String, ElemLabels <: Tuple]

class Typed[A]:
  type Value = A

// a workaround for *: not working in certain cases:
// https://github.com/lampepfl/dotty/issues/18011
// in this case the code compiles but loses type precision
type Prepend[X, +Y <: Tuple] <: Tuple = X match
  case X => X *: Y

transparent inline def makeLabellings[T <: Tuple]: Typed[_ <: Tuple] =
  inline erasedValue[T] match
    case EmptyTuple => Typed[EmptyTuple]
    case _: (h *: t) =>
      // the pattern-matches below are needed to force the compiler to use the most precise
      // type for the values we are summoning
      // changing the order of pattern-matching here will make the code lose its precise types
      inline makeLabellings[t] match
        case tailLabellings =>
          inline summonInline[Mirror.Of[h]] match
            case headMirror =>
              type HeadLabelling = Labelling[headMirror.MirroredLabel, headMirror.MirroredElemLabels]

              Typed[Prepend[HeadLabelling, tailLabellings.Value]]
