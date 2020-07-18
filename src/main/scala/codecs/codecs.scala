package codecs

import io.circe
import io.circe.Decoder.Result
import io.circe.HCursor

trait EncoderInstances {

  /** An encoder for the `Unit` value */
  implicit val unitEncoder: circe.Encoder[Unit] = circe.Encoder.encodeUnit

  /** An encoder for `Int` values */
  implicit val intEncoder: circe.Encoder[Int] = circe.Encoder.encodeInt

  /** An encoder for `String` values */
  implicit val stringEncoder: circe.Encoder[String] = circe.Encoder.encodeString

  /** An encoder for `Boolean` values */
  implicit val booleanEncoder: circe.Encoder[Boolean] = circe.Encoder.encodeBoolean

  /**
   * Encodes a list of values of type `A` into a JSON array containing
   * the list elements encoded with the given `encoder`
   */
  //  implicit def listEncoder[A](implicit encoder: Encoder[A]): Encoder[List[A]] =
  //    Encoder.fromFunction(as => Json.Arr(as.map(encoder.encode)))
  implicit def listEncoder[A](implicit encoder: circe.Encoder[A]): circe.Encoder.AsArray[List[A]] =
    circe.Encoder.encodeList(encoder)

}

/**
 * A specialization of `Encoder` that returns JSON objects only
 */
trait ObjectEncoder[A] extends circe.Encoder[A] {
  // Refines the encoding result to `Json.Obj`
  //  override def apply(a: A): circe.Json = ???

  /**
   * Combines `this` encoder with `that` encoder.
   * Returns an encoder producing a JSON object containing both
   * fields of `this` encoder and fields of `that` encoder.
   */
  def zip[B](that: ObjectEncoder[B]): ObjectEncoder[(A, B)] =
    ObjectEncoder.fromFunction { case (a, b) =>
      circe.Json.fromJsonObject(
        circe.JsonObject.fromMap(
          apply(a).asObject.get.toMap ++ that.apply(b).asObject.get.toMap
        )
      )
    }
}

object ObjectEncoder {

  /**
   * Convenient method for creating an instance of object encoder from a function `f`
   */
  def fromFunction[A](f: A => circe.Json): ObjectEncoder[A] = (a: A) => f(a)

  /**
   * An encoder for values of type `A` that produces a JSON object with one field
   * named according to the supplied `name` and containing the encoded value.
   */
  def field[A](name: String)(implicit encoder: circe.Encoder[A]): ObjectEncoder[A] =
  //    ObjectEncoder.fromFunction(a => Json.Obj(Map(name -> encoder.apply(a))))
    ObjectEncoder.fromFunction(a =>
      circe.Json.fromJsonObject(
        circe.JsonObject.singleton(name, encoder.apply(a))
      )
    )

}

trait ObjectDecoder[A] extends circe.Decoder[A] {

  def zip[B](objectDecoder: ObjectDecoder[B]): ObjectDecoder[(A, B)] =
    ObjectDecoder.fromFunction(cursor => {
      for {
        fieldA <- apply(cursor)
        fieldB <- objectDecoder.apply(cursor)
      } yield (fieldA, fieldB)
    })

}

object ObjectDecoder {

  def fromFunction[A](f: HCursor => Result[A]): ObjectDecoder[A] = (c: HCursor) => f(c)

  def field[A](name: String)(implicit decoder: circe.Decoder[A]): ObjectDecoder[A] = ObjectDecoder.fromFunction(_.downField(name).as[A])

}

trait DecoderInstances {

  /** A decoder for the `Unit` value */
  implicit val unitDecoder: circe.Decoder[Unit] = circe.Decoder.decodeUnit

  /** A decoder for `Int` values. Hint: use the `isValidInt` method of `BigDecimal`. */
  implicit val intDecoder: circe.Decoder[Int] = circe.Decoder.decodeInt

  /** A decoder for `String` values */
  implicit val stringDecoder: circe.Decoder[String] = circe.Decoder.decodeString

  /** A decoder for `Boolean` values */
  implicit val booleanDecoder: circe.Decoder[Boolean] = circe.Decoder.decodeBoolean

  /**
   * A decoder for JSON arrays. It decodes each item of the array
   * using the given `decoder`. The resulting decoder succeeds only
   * if all the JSON array items are successfully decoded.
   */
  implicit def listDecoder[A](implicit decoder: circe.Decoder[A]): circe.Decoder[List[A]] =
    circe.Decoder.decodeList

  /**
   * A decoder for JSON objects. It decodes the value of a field of
   * the supplied `name` using the given `decoder`.
   */
  def field[A](name: String)(implicit decoder: circe.Decoder[A]): circe.Decoder[A] =
    circe.Decoder.instance(cursor => decoder.tryDecode(cursor.downField(name)))

}

case class Person(name: String, age: Int)

object Person extends PersonCodecs

trait PersonCodecs {

  /** The encoder for `Person` */
  implicit val personEncoder: circe.Encoder[Person] =
    ObjectEncoder.field[String]("name")
      .zip(ObjectEncoder.field[Int]("age"))
      .contramap(person => (person.name, person.age))

  /** The corresponding decoder for `Person` */
  implicit val personDecoder: circe.Decoder[Person] =
    ObjectDecoder.fromFunction(_.downField("name").as[String])
      .zip(_.downField("age").as[Int])
      .map { tuple: (String, Int) =>
        tuple match {
          case (name, age) => Person(name, age)
        }
      } // No error handling

}

case class Contacts(people: List[Person])

object Contacts extends ContactsCodecs

trait ContactsCodecs {

  // TODO Define the encoder and the decoder for `Contacts`
  // The JSON representation of a value of type `Contacts` should be
  // a JSON object with a single field named “people” containing an
  // array of values of type `Person` (reuse the `Person` codecs)

  implicit val contactsEncoder: circe.Encoder[Contacts] =
    ObjectEncoder.field[List[Person]]("people")
      .contramap(_.people)

  //TODO: use custom ObjectDecoder
  implicit val contactsDecoder: circe.Decoder[Contacts] =
    ObjectDecoder.fromFunction(_.downField("people").as[List[Person]])
      .map { people: List[Person] =>
        people match {
          case _ => Contacts(people)
        }
      }

}

// In case you want to try your code, here is a simple `Main`
// that can be used as a starting point. Otherwise, you can use
// the REPL (use the `console` sbt task).
object Main {

  import io.circe.parser.decode

  def main(args: Array[String]): Unit = {
    println(decode[Int]("42"))
    println(decode[Boolean]("true"))
    println(decode[List[Int]](""" [ 1, 2, 3 ] """))
    println(decode[Person](""" { "name": "Alice", "age": 42 } """))
    println(decode[List[Person]](""" [ { "name": "Alice", "age": 42 }, { "name": "Bob", "age": 24 } ] """))
    println(decode[Contacts](""" { "people": [ { "name": "Alice", "age": 42 }, { "name": "Bob", "age": 24 } ] } """))
  }

}
