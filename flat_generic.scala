package ncreep.flat_generic

import io.circe.*
import ncreep.common.*
import ncreep.duplicate_fields.*
import ncreep.model.*

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

  val encoder = Encoder.instance[A]: value =>
    val fields = Tuple.fromProductTyped(value)
    val jsons = tupleToJson(fields)

    concatObjects(jsons)

  val tupleDecoder = decodeTuple[mirror.MirroredElemTypes]
  val decoder = tupleDecoder.map(mirror.fromTuple)

  Codec.from(decoder, encoder)