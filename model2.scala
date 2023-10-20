package ncreep.model

import io.circe.*
import io.circe.generic.semiauto

case class User2(email: Email2, phone: Phone2, address: Address2)

case class Email2(primary: String, secondary: Option[String], lastUpdate: Long, verified: Boolean)

case class Phone2(number: String, prefix: Int, verified: Boolean)

case class Address2(country: String, city: String, lastUpdate: Date)

case class Date(value: Long)

object Email2:
  given Codec.AsObject[Email2] = semiauto.deriveCodec[Email2]

object Phone2:
  given Codec.AsObject[Phone2] = semiauto.deriveCodec[Phone2]

object Address2:
  given Codec.AsObject[Address2] = semiauto.deriveCodec[Address2]

object Date:
  given Codec.AsObject[Date] = semiauto.deriveCodec[Date]

val user2 = User2(
  Email2(primary = "bilbo@baggins.com", secondary = Some("frodo@baggins.com"), lastUpdate = 21, verified = true),
  Phone2(number = "555-555-00", prefix = 88, verified = false),
  Address2(country = "Shire", city = "Hobbiton", lastUpdate = Date(55))
)
