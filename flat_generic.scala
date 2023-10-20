package ncreep.flat_generic

import io.circe.*
import ncreep.common.*
import ncreep.error_generation.*
import ncreep.model.*

import scala.deriving.Mirror

@main def test(): Unit =
  val codec = makeCodec[User]
  // val codec2 = makeCodec[User2] // does not compile

  val json = codec(user)

  println(json)
  println(codec.decodeJson(json))

end test

inline def makeCodec[A <: Product](
    using mirror: Mirror.ProductOf[A]): Codec[A] =

  checkDuplicateFields[A]

  val encoder = Encoder.instance[A]: value =>
    val fields = Tuple.fromProductTyped(value)
    val jsons = tupleToJson(fields)

    concatObjects(jsons)

  val decoder = 
    decodeTuple[mirror.MirroredElemTypes].map(mirror.fromTuple)

  Codec.from(decoder, encoder)