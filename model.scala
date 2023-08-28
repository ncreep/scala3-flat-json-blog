package ncreep.model

import io.circe.*
import io.circe.generic.semiauto

case class User(email: Email, phone: Phone, address: Address)

case class Email(primary: String, secondary: Option[String])

case class Phone(number: String, prefix: Int)

case class Address(country: String, city: String)

object Email:
  given Codec.AsObject[Email] = semiauto.deriveCodec[Email]

object Phone:
  given Codec.AsObject[Phone] = semiauto.deriveCodec[Phone]

object Address:
  given Codec.AsObject[Address] = semiauto.deriveCodec[Address]

val user = User(
  Email(primary = "bilbo@baggins.com", secondary = Some("frodo@baggins.com")),
  Phone(number = "555-555-00", prefix = 88),
  Address(country = "Shire", city = "Hobbiton")
)
