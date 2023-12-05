package ncreep.error_generation

import scala.deriving.Mirror

trait LabellingsFor[A]:
  type Labellings <: Tuple

object LabellingsFor:
  type Aux[A, Labellings0] = LabellingsFor[A] { type Labellings = Labellings0 }

  def make[A, Labellings0 <: Tuple]: Aux[A, Labellings0] =
    new LabellingsFor[A]:
      type Labellings = Labellings0

  given LabellingsFor.Aux[EmptyTuple, EmptyTuple] = make

  type ProductMirrorAux[A, Label, ElemLabels] = Mirror.ProductOf[A] {
    type MirroredLabel = Label
    type MirroredElemLabels = ElemLabels
  }

  /** Creates a labelling for a non-empty tuple. */
  given [H, T <: Tuple, HLabel <: String, HElemLabels <: Tuple, TLabellings <: Tuple](
      using hMirror: ProductMirrorAux[H, HLabel, HElemLabels],
      tLabellings: LabellingsFor.Aux[T, TLabellings])
      : LabellingsFor.Aux[H *: T, Labelling[HLabel, HElemLabels] *: TLabellings] =
    make

  type FieldsMirrorAux[A, Fields <: Tuple] = Mirror.ProductOf[A] {
    type MirroredElemTypes = Fields
  }

  /** Creates labellings for the fields of [[A]], assuming that [[A]] has a [[Mirror]] in scope. */
  given [A, Fields <: Tuple, Labellings <: Tuple](
      using mirror: FieldsMirrorAux[A, Fields],
      labellings: LabellingsFor.Aux[Fields, Labellings]): LabellingsFor.Aux[A, Labellings] =
    make

end LabellingsFor

// An alternative version of `checkDuplicateFields` that uses givens with
// `LabellingsFor`, instead of `makeLabellings`
inline def checkDuplicateFieldsWithGivens[A](
    using labels: LabellingsFor[A]): Unit =
  renderDuplicatesError[labels.Labellings]
