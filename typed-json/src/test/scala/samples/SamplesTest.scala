package samples

import munit.FunSuite

import frawa.typedjson.TypedJson
import frawa.typedjson.parser.jawn.JawnParser
import frawa.typedjson.validation.TypeMismatch
import frawa.typedjson.pointer.Pointer

class SamplesTest extends FunSuite {

  test("always valid without a schema") {
    implicit val p = new JawnParser()

    val typedJson = TypedJson.create()
    val json      = """{"foo":"bar"}"""
    val result    = typedJson.validate(json)
    assertEquals(result.map(_.valid), Right(true))
  }

  test("use schema to validate several values") {
    implicit val p = new JawnParser()

    val schemaJson = """{"type": "string"}"""
    val typedJson  = TypedJson.create(schemaJson).toOption.get

    val validJson = """"foo""""
    assertEquals(typedJson.validate(validJson).map(_.valid), Right(true))
    val invalidJson = """13"""
    assertEquals(typedJson.validate(invalidJson).map(_.valid), Right(false))
  }

  test("obtain validation errors") {
    implicit val p = new JawnParser()

    val schemaJson = """{"type": "string"}"""
    val typedJson  = TypedJson.create(schemaJson).toOption.get

    val invalidJson = """true"""
    val validation  = typedJson.validate(invalidJson)
    assertEquals(validation.map(_.valid), Right(false))
    assertEquals(validation.map(_.output.errors), Right(Seq(TypedJson.Error(Pointer.empty, TypeMismatch("string")))))
  }

}