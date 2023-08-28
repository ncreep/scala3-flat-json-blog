package ncreep.flat_concrete

import io.circe.*
import ncreep.common.*
import ncreep.model.*

import scala.deriving.Mirror

@main def test() =
  val json = codec(user)

  println(json)
  println(codec.decodeJson(json))

end test

val codec: Codec[User] =
  val encoder = Encoder.instance[User]: value =>
    val fields = Tuple.fromProductTyped(value)
    val jsons = tupleToJson(fields)
    
    concatObjects(jsons)
    
  val mirror = summon[Mirror.Of[User]]

  val decoder =
    decodeTuple[mirror.MirroredElemTypes].map(mirror.fromTuple)

  Codec.from(decoder, encoder)
