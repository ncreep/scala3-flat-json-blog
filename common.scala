package ncreep.common

import io.circe.*
import scala.compiletime.*

inline def tupleToJson(tuple: Tuple): List[JsonObject] =
  inline tuple match
    case EmptyTuple => Nil
    case tup: (h *: t) =>
      val encoder = summonInline[Encoder.AsObject[h]]

      val json = encoder.encodeObject(tup.head)

      json :: tupleToJson(tup.tail)

// a helper for pattern-matching
trait Is[A]

inline def decodeTuple[T <: Tuple]: Decoder[T] = 
  inline erasedValue[Is[T]] match
    case _: Is[EmptyTuple] => Decoder.const(EmptyTuple)
    case _: Is[h *: t] =>
      val decoder = summonInline[Decoder[h]]

      combineDecoders(decoder, decodeTuple[t])

def concatObjects(jsons: List[JsonObject]): Json = 
  Json.obj(jsons.flatMap(_.toList): _*)

def combineDecoders[H, T <: Tuple](dh: Decoder[H], dt: Decoder[T]): Decoder[H *: T] =
  dh.product(dt).map(_ *: _)