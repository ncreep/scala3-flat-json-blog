package ncreep.common

import io.circe.*
import scala.compiletime.*

// asserting at compile-time that a given type is a tuple type
type IsTuple[T <: Tuple] <: Tuple = T match
  case T => T

type TupledDecoders[T <: Tuple] = T match
  case EmptyTuple => Decoder[EmptyTuple]
  case Decoder[h] *: t => MapCons[h, TupledDecoders[t]]

type DecoderForTuple[T <: Tuple] = T match
  case EmptyTuple => Decoder[EmptyTuple]
  case h *: t => MapCons[h, DecoderForTuple[t]]

type MapCons[H, DT] = DT match
  case Decoder[IsTuple[t]] => Decoder[H *: IsTuple[t]]

inline def tupleToJson(tuple: Tuple): List[JsonObject] =
  inline tuple match
    case EmptyTuple => Nil
    case tup: (h *: t) =>
      val encoder = summonInline[Encoder.AsObject[h]]

      val json = encoder.encodeObject(tup.head)

      json :: tupleToJson(tup.tail)

inline def decodeTuple[T <: Tuple]: DecoderForTuple[T] =
  inline erasedValue[T] match
    case _: EmptyTuple => Decoder.const(EmptyTuple)
    case _: (h *: t) =>
      val decoder = summonInline[Decoder[h]]

      mapCons(decoder, decodeTuple[t])

inline def mapCons[H, DT](h: Decoder[H], t: DT): MapCons[H, DT] =
  inline t match
    case tail: Decoder[IsTuple[t]] => h.product(tail).map(_ *: _)

def concatObjects(jsons: List[JsonObject]): Json = 
  Json.obj(jsons.flatMap(_.toList): _*)
