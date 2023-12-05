package ncreep.error_generation

import scala.Tuple.*
import scala.compiletime.ops.any.*
import scala.compiletime.ops.boolean.*
import scala.compiletime.ops.int.*

type FindDuplicates[Labellings <: Tuple] = 
  OnlyDuplicates[GroupByLabels[ZipAllWithSource[Labellings]]]

type ZipAllWithSource[Labellings <: Tuple] = Labellings FlatMap ZipWithSource

type GroupByLabels[Labels <: Tuple] <: Tuple = Labels match
  case EmptyTuple => EmptyTuple
  case (label, source) *: t =>
    ((label, source *: FindLabel[label, t])) *: GroupByLabels[RemoveLabel[label, t]]

type OnlyDuplicates[Labels <: Tuple] = Labels Filter ([t] =>> Size[Second[t]] > 1)

type FindLabel[Label, T <: Tuple] =
  T Filter ([ls] =>> HasLabel[Label, ls]) Map Second

type RemoveLabel[Label, T <: Tuple] =
  T Filter ([ls] =>> ![HasLabel[Label, ls]])

type HasLabel[Label, LS] <: Boolean = LS match
  case (l, s) => Label == l

type ZipWithSource[L] <: Tuple = L match
  case Labelling[label, elemLabels] => ZipWithConst[elemLabels, label]

type ZipWithConst[T <: Tuple, A] = T Map ([t] =>> (t, A))

type Second[T] = T match
  case (a, b) => b

type Size[T] <: Int = T match
  case EmptyTuple => 0
  case x *: xs => 1 + Size[xs]