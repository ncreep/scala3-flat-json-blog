package ncreep.error_generation

import scala.Tuple.*
import scala.compiletime.ops.any.*
import scala.compiletime.ops.boolean.*
import scala.compiletime.ops.int.*

type FindDuplicates[Labellings <: Tuple] = 
  OnlyDuplicates[GroupByLabels[ZipAllWithSource[Labellings]]]

type ZipAllWithSource[Labellings <: Tuple] = Labellings FlatMap ZipLabels

type GroupByLabels[Labels <: Tuple] <: Tuple = Labels match
  case EmptyTuple => EmptyTuple
  case (label, source) *: t =>
    ((label, source *: FindLabel[label, t])) *: GroupByLabels[RemoveLabel[label, t]]

type OnlyDuplicates[Labels <: Tuple] = Filter[Labels, [t] =>> Size[Second[t]] > 1]

type FindLabel[Label, T <: Tuple] =
  Filter[T, [ls] =>> HasLabel[Label, ls]] Map Second

type RemoveLabel[Label, T <: Tuple] =
  Filter[T, [ls] =>> ![HasLabel[Label, ls]]]

type HasLabel[Label, LS] <: Boolean = LS match
  case (l, s) => Label == l

type ZipLabels[L] <: Tuple = L match
  case Labelling[label, elemLabels] => ZipWithConst[elemLabels, label]

type ZipWithConst[T <: Tuple, A] = Map[T, [t] =>> (t, A)]

type Second[T] = T match
  case (a, b) => b

type Size[T] <: Int = T match
  case EmptyTuple => 0
  case x *: xs => 1 + Size[xs]

