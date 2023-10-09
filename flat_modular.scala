package ncreep.modularity

import io.circe.*
import ncreep.common.combineDecoders
import ncreep.duplicate_fields.*
import ncreep.model.*

import scala.Tuple.*
import scala.compiletime.*
import scala.deriving.Mirror

@main def test(): Unit =
  val codec = makeCodec[User]

  val json = codec(user)

  println(json)
  println(codec.decodeJson(json))

end test

inline def makeCodec[A <: Product](
    using mirror: Mirror.ProductOf[A]): Codec[A] =

  checkDuplicateFields[mirror.MirroredElemTypes]

  val encoders = summonAll[Map[mirror.MirroredElemTypes, Encoder.AsObject]]
  val encoder = encodeTuple(encoders).contramap[A](Tuple.fromProductTyped)

  val decoders = summonAll[Map[mirror.MirroredElemTypes, Decoder]]
  val decoder = decodeTuple(decoders).map(mirror.fromTuple)

  Codec.from(decoder, encoder)

// a helper for pattern matching
case class IsDecoderMap[T <: Tuple](value: Map[T, Decoder])

inline def decodeTuple[T <: Tuple](decoders: Map[T, Decoder]): Decoder[T] =
  inline IsDecoderMap(decoders) match
    case _: IsDecoderMap[EmptyTuple] => Decoder.const(EmptyTuple)
    case ds: IsDecoderMap[h *: t] =>
      combineDecoders(ds.value.head, decodeTuple(ds.value.tail))

// a helper for pattern matching
case class IsEncoderMap[T <: Tuple](value: Map[T, Encoder.AsObject])

inline def encodeTuple[T <: Tuple](encoders: Map[T, Encoder.AsObject]): Encoder.AsObject[T] =
  inline IsEncoderMap(encoders) match
    case _: IsEncoderMap[EmptyTuple] => emptyEncoder
    case es: IsEncoderMap[h *: t] =>
      combineObjectEncoders(es.value.head, encodeTuple(es.value.tail))

def emptyEncoder: Encoder.AsObject[EmptyTuple] = Encoder.AsObject.instance(_ => JsonObject.empty)

def combineObjectEncoders[H, T <: Tuple](eh: Encoder.AsObject[H], et: Encoder.AsObject[T]): Encoder.AsObject[H *: T] =
  Encoder.AsObject.instance[H *: T]: ht =>
    val headObject = eh.encodeObject(ht.head)
    val tailObject = et.encodeObject(ht.tail)

    JsonObject.fromIterable(headObject.toIterable ++ tailObject.toIterable)
