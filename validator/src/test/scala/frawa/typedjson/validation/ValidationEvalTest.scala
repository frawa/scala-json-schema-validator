/*
 * Copyright 2021 Frank Wagner
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package frawa.typedjson.validation

import frawa.typedjson
import frawa.typedjson.meta.MetaSchemas
import frawa.typedjson.parser._
import frawa.typedjson.testutil.TestSchemas._
import frawa.typedjson.testutil.TestUtil.{assertResult, withSchema}
import frawa.typedjson.processor._
import munit.FunSuite

class ValidationEvalTest extends FunSuite {
  implicit val zioParser: ZioParser = new ZioParser()

  private def assertValidate(text: String)(
      schema: SchemaValue,
      lazyResolver: Option[LoadedSchemasResolver.LazyResolver] = None
  )(
      f: Result[ValidationResult] => Unit
  ) = {
    implicit val lr = lazyResolver
    assertResult(ValidationEval())(schema, text)(f)
  }

  private def assertErrors(result: Result[ValidationResult], expected: Seq[WithPointer[Observation]]): Unit = {
    assertEquals(result.results.flatMap(_.errors), expected)
  }

  test("null") {
    withSchema(nullSchema) { schema =>
      assertValidate("""null""")(schema) { result =>
        assertErrors(result, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate("""13""")(schema) { result =>
        assertErrors(result, Seq(WithPointer(TypeMismatch("null"))))
        assertEquals(result.valid, false)
      }
    }
  }

  test("boolean") {
    withSchema(boolSchema) { schema =>
      assertValidate("""true""")(schema) { result =>
        assertErrors(result, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate("""13""")(schema) { result =>
        assertErrors(result, Seq(WithPointer(TypeMismatch("boolean"))))
        assertEquals(result.valid, false)
      }
    }
  }

  test("true schema") {
    withSchema(trueSchema) { schema =>
      assertValidate("""null""")(schema) { result =>
        assertErrors(result, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate("""13""")(schema) { result =>
        assertErrors(result, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate("""{}""")(schema) { result =>
        assertErrors(result, Seq())
        assertEquals(result.valid, true)
      }
    }
  }

  test("false schema") {
    withSchema(falseSchema) { schema =>
      assertValidate("""null""")(schema) { result =>
        assertErrors(
          result,
          Seq(
            typedjson.processor.WithPointer(
              result = FalseSchemaReason(),
              pointer = Pointer(
                segments = Nil
              )
            )
          )
        )
        assertEquals(result.valid, false)
      }
      assertValidate("""13""")(schema) { result =>
        assertErrors(
          result,
          Seq(
            typedjson.processor.WithPointer(
              result = FalseSchemaReason(),
              pointer = Pointer(
                segments = Nil
              )
            )
          )
        )
        assertEquals(result.valid, false)
      }
      assertValidate("""{}""")(schema) { result =>
        assertErrors(
          result,
          Seq(
            typedjson.processor.WithPointer(
              result = FalseSchemaReason(),
              pointer = Pointer(
                segments = Nil
              )
            )
          )
        )
        assertEquals(result.valid, false)
      }
    }
  }

  test("not false") {
    withSchema(notFalseSchema) { schema =>
      assertValidate("""null""")(schema) { result =>
        assertErrors(result, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate("""13""")(schema) { result =>
        assertErrors(result, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate("""{}""")(schema) { result =>
        assertErrors(result, Seq())
        assertEquals(result.valid, true)
      }
    }
  }

  test("empty schema") {
    withSchema(emtpySchema) { schema =>
      assertValidate("""null""")(schema) { result =>
        assertErrors(result, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate("""13""")(schema) { result =>
        assertErrors(result, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate("""{}""")(schema) { result =>
        assertErrors(result, Seq())
        assertEquals(result.valid, true)
      }
    }
  }

  test("not empty") {
    withSchema("""{"not": {}}""") { schema =>
      assertValidate("""null""")(schema) { result =>
        assertErrors(
          result,
          Seq(
            typedjson.processor.WithPointer(
              result = NotInvalid(),
              pointer = Pointer(
                segments = Nil
              )
            )
          )
        )
        assertEquals(result.valid, false)
      }
      assertValidate("""13""")(schema) { result =>
        assertErrors(
          result,
          Seq(
            typedjson.processor.WithPointer(
              result = NotInvalid(),
              pointer = Pointer(
                segments = Nil
              )
            )
          )
        )
        assertEquals(result.valid, false)
      }
      assertValidate("""{}""")(schema) { result =>
        assertErrors(
          result,
          Seq(
            typedjson.processor.WithPointer(
              result = NotInvalid(),
              pointer = Pointer(
                segments = Nil
              )
            )
          )
        )
        assertEquals(result.valid, false)
      }
    }
  }

  test("string") {
    withSchema(stringSchema) { schema =>
      assertValidate(""""hello"""")(schema) { result =>
        assertErrors(result, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate("""13""")(schema) { result =>
        assertErrors(result, Seq(WithPointer(TypeMismatch("string"))))
        assertEquals(result.valid, false)
      }
    }
  }

  test("number") {
    withSchema(numberSchema) { schema =>
      assertValidate("""13""")(schema) { result =>
        assertErrors(result, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate("""null""")(schema) { result =>
        assertErrors(result, Seq(WithPointer(TypeMismatch("number"))))
        assertEquals(result.valid, false)
      }
    }
  }

  test("array") {
    withSchema(numberArraySchema) { schema =>
      assertValidate("""[13]""")(schema) { result =>
        assertErrors(result, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate("""null""")(schema) { result =>
        assertErrors(result, Seq(WithPointer(TypeMismatch("array"))))
        assertEquals(result.valid, false)
      }
    }
  }

  test("array item") {
    withSchema(numberArraySchema) { schema =>
      assertValidate("""[true]""")(schema) { result =>
        assertErrors(result, Seq(typedjson.processor.WithPointer(TypeMismatch("number"), Pointer(0))))
        assertEquals(result.valid, false)
      }
      assertValidate("""[13]""")(schema) { result =>
        assertErrors(result, Seq())
        assertEquals(result.valid, true)
      }
    }
  }

  test("object") {
    withSchema(totoObjectSchema) { schema =>
      assertValidate("""{
                       |"toto": 13,
                       |"titi": "hello"
                       |}
                       |"""".stripMargin)(schema) { result =>
        assertErrors(result, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate("""null""")(schema) { result =>
        assertErrors(
          result,
          Seq(
            WithPointer(
              TypeMismatch("object")
            )
          )
        )
        assertEquals(result.valid, false)
      }
    }
  }

  test("object property type") {
    withSchema(totoObjectSchema) { schema =>
      assertValidate("""{
                       |"toto": 13,
                       |"titi": true
                       |}
                       |""".stripMargin)(schema) { result =>
        assertErrors(result, Seq(typedjson.processor.WithPointer(TypeMismatch("string"), Pointer.empty / "titi")))
        assertEquals(result.valid, false)
      }
    }
  }

  test("object missing property") {
    withSchema(totoObjectSchema) { schema =>
      assertValidate("""{
                       |"toto": 13
                       |}
                       |""".stripMargin)(schema) { result =>
        assertErrors(result, Seq())
        assertEquals(result.valid, true)
      }
    }
  }

  test("object missing required property") {
    withSchema("""{
                 |"type": "object",
                 |"properties": {
                 |  "toto": { "type": "number" },
                 |  "titi": { "type": "string" }
                 |},
                 |"required": ["titi"]
                 |}
                 |""".stripMargin) { schema =>
      assertValidate("""{
                       |"toto": 13
                       |}
                       |""".stripMargin)(schema) { result =>
        assertErrors(
          result,
          Seq(
            WithPointer(
              result = MissingRequiredProperties(Seq("titi"))
            )
          )
        )
      }
    }
  }

  test("allOf") {
    withSchema(allOfSchema) { schema =>
      assertValidate("""1313""".stripMargin)(schema) { result =>
        assertErrors(result, Seq())
        assertEquals(result.valid, true)
      }
    }
  }

  test("impossible allOf") {
    withSchema("""{
                 |"allOf": [
                 |  { "type": "number" },
                 |  { "type": "string" }
                 |]
                 |}
                 |""".stripMargin) { schema =>
      assertValidate("""1313""".stripMargin)(schema) { result =>
        assertErrors(result, Seq(WithPointer(TypeMismatch("string"))))
        assertEquals(result.valid, false)
      }
    }
  }

  test("anyOf") {
    withSchema(anyOfSchema) { schema =>
      assertValidate("""1313""".stripMargin)(schema) { result =>
        assertErrors(result, Seq())
        assertEquals(result.valid, true)
      }
    }
  }

  test("failed anyOf") {
    withSchema(anyOfSchema) { schema =>
      assertValidate("""true""".stripMargin)(schema) { result =>
        assertErrors(
          result,
          Seq(
            WithPointer(TypeMismatch("number")),
            WithPointer(TypeMismatch("string"))
          )
        )
        assertEquals(result.valid, false)
      }
    }
  }

  test("oneOf") {
    withSchema(oneOfSchema) { schema =>
      assertValidate("""1313""".stripMargin)(schema) { result =>
        assertErrors(result, Seq())
        assertEquals(result.valid, true)
      }
    }
  }

  test("failed oneOf: none") {
    withSchema("""{
                 |"oneOf": [
                 |  { "type": "string" },
                 |  { "type": "boolean" }
                 |]
                 |}
                 |""".stripMargin) { schema =>
      assertValidate("""1313""".stripMargin)(schema) { result =>
        assertErrors(
          result,
          Seq(
            WithPointer(TypeMismatch("string")),
            WithPointer(TypeMismatch("boolean"))
          )
        )
        assertEquals(result.valid, false)
      }
    }
  }

  test("failed oneOf: two") {
    withSchema("""{
                 |"oneOf": [
                 |  { "type": "number" },
                 |  { "type": "number" }
                 |]
                 |}
                 |""".stripMargin) { schema =>
      assertValidate("""1313""".stripMargin)(schema) { result =>
        assertErrors(
          result,
          Seq(
            WithPointer(NotOneOf(2))
          )
        )
        assertEquals(result.valid, false)

      }
    }
  }

  test("not") {
    withSchema("""{
                 |"not": { "type": "number" }
                 |}
                 |""".stripMargin) { schema =>
      assertValidate("""true""".stripMargin)(schema) { result =>
        assertErrors(result, Seq())
        assertEquals(result.valid, true)
      }
    }
  }

  test("failed not") {
    withSchema("""{
                 |"not": { "type": "number" }
                 |}
                 |""".stripMargin) { schema =>
      assertValidate("""1313""".stripMargin)(schema) { result =>
        assertErrors(result, Seq(WithPointer(NotInvalid())))
        assertEquals(result.valid, false)
      }
    }
  }

  test("if/then/else") {
    withSchema(ifThenElseSchema) { schema =>
      assertValidate("""1313""".stripMargin)(schema) { result =>
        assertErrors(result, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate(""""string"""".stripMargin)(schema) { result =>
        assertErrors(result, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate("""null""".stripMargin)(schema) { result =>
        assertErrors(result, Seq(WithPointer(TypeMismatch("string"))))
        assertEquals(result.valid, false)
      }
    }
  }

  test("then/if/else") {
    withSchema("""{
                 |"then": { "type": "number" },
                 |"if": { "type": "number" },
                 |"else": { "type": "string" }
                 |}
                 |""".stripMargin) { schema =>
      assertValidate("""1313""".stripMargin)(schema) { result =>
        assertErrors(result, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate(""""string"""".stripMargin)(schema) { result =>
        assertErrors(result, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate("""null""".stripMargin)(schema) { result =>
        assertErrors(result, Seq(WithPointer(TypeMismatch("string"))))
        assertEquals(result.valid, false)
      }
    }
  }

  test("if/else") {
    withSchema("""{
                 |"if": { "type": "number" },
                 |"else": { "type": "string" }
                 |}
                 |""".stripMargin) { schema =>
      assertValidate("""1313""".stripMargin)(schema) { result =>
        assertErrors(result, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate(""""string"""".stripMargin)(schema) { result =>
        assertErrors(result, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate("""null""".stripMargin)(schema) { result =>
        assertErrors(result, Seq(WithPointer(TypeMismatch("string"))))
        assertEquals(result.valid, false)
      }
    }
  }

  test("if/then") {
    withSchema("""{
                 |"if": { "type": "number" },
                 |"then": { "type": "number" }
                 |}
                 |""".stripMargin) { schema =>
      assertValidate("""1313""".stripMargin)(schema) { result =>
        assertErrors(result, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate(""""string"""".stripMargin)(schema) { result =>
        assertErrors(result, Seq())
        assertEquals(result.valid, true)
      }
    }
  }

  test("then/else") {
    withSchema("""{
                 |"then": { "type": "number" },
                 |"else": { "type": "string" }
                 |}
                 |""".stripMargin) { schema =>
      assertValidate("""1313""".stripMargin)(schema) { result =>
        assertErrors(result, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate(""""string"""".stripMargin)(schema) { result =>
        assertErrors(result, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate("""null""".stripMargin)(schema) { result =>
        assertErrors(result, Seq())
        assertEquals(result.valid, true)
      }
    }
  }

  test("null or string") {
    withSchema(nullOrStringSchema) { schema =>
      assertValidate("""null""")(schema) { result =>
        assertErrors(result, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate(""""hello"""")(schema) { result =>
        assertErrors(result, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate("""13""")(schema) { result =>
        assertErrors(
          result,
          Seq(
            WithPointer(TypeMismatch("null")),
            WithPointer(TypeMismatch("string"))
          )
        )
        assertEquals(result.valid, false)
      }
    }
  }

  test("enum") {
    withSchema(enumSchema) { schema =>
      assertValidate(""""foo"""")(schema) { result =>
        assertErrors(result, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate(""""bar"""")(schema) { result =>
        assertErrors(result, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate(""""hello"""")(schema) { result =>
        assertErrors(
          result,
          Seq(
            WithPointer(
              NotInEnum(
                Seq(
                  StringValue("foo"),
                  StringValue("bar")
                )
              )
            )
          )
        )
        assertEquals(result.valid, false)
      }
    }
  }

  test("const") {
    withSchema(constSchema) { schema =>
      assertValidate(""""first"""")(schema) { result =>
        assertErrors(result, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate("""{}""".stripMargin)(schema) { result =>
        assertErrors(
          result,
          Seq(
            typedjson.processor.WithPointer(
              result = TypeMismatch(
                expected = "string"
              ),
              pointer = Pointer(
                segments = Nil
              )
            ),
            typedjson.processor.WithPointer(
              result = NotInEnum(
                values = List(
                  StringValue(
                    value = "first"
                  )
                )
              ),
              pointer = Pointer(
                segments = Nil
              )
            )
          )
        )
      }
      assertValidate(""""second"""")(schema) { result =>
        assertErrors(
          result,
          Seq(
            typedjson.processor.WithPointer(
              result = NotInEnum(
                values = List(
                  StringValue(
                    value = "first"
                  )
                )
              ),
              pointer = Pointer(
                segments = Nil
              )
            )
          )
        )
      }
    }
  }

  test("$id/$ref/$def") {
    withSchema(idRefDefsSchema) { schema =>
      assertValidate("""[1313]""".stripMargin)(schema) { result =>
        assertErrors(result, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate(""""string"""".stripMargin)(schema) { result =>
        assertErrors(
          result,
          Seq(
            typedjson.processor.WithPointer(
              result = TypeMismatch(
                expected = "array"
              ),
              pointer = Pointer(
                segments = Nil
              )
            )
          )
        )
        assertEquals(result.valid, false)
      }
    }
  }

  test("$ref in properties") {
    withSchema(refInPropertiesSchema) { schema =>
      assertValidate("""{ "foo": 13 }""".stripMargin)(schema) { result =>
        assertErrors(result, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate("""{ "foo": true }""".stripMargin)(schema) { result =>
        assertErrors(
          result,
          Seq(
            typedjson.processor.WithPointer(
              result = TypeMismatch(
                expected = "number"
              ),
              pointer = Pointer.parse("/foo")
            )
          )
        )
        assertEquals(result.valid, false)
      }
    }
  }

  test("$ref at root") {
    withSchema(refAtRootSchema) { schema =>
      assertValidate("""{ "foo": 13 }""".stripMargin)(schema) { result =>
        assertErrors(result, Seq())
        assertEquals(result.problems.errors, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate("""{ "foo": [13] }""".stripMargin)(schema) { result =>
        assertErrors(result, Seq())
        assertEquals(result.problems.errors, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate("""{ "foo": true }""".stripMargin)(schema) { result =>
        assertErrors(
          result,
          Seq(
            typedjson.processor.WithPointer(
              result = TypeMismatch(
                expected = "number"
              ),
              pointer = Pointer.parse("/foo")
            ),
            typedjson.processor.WithPointer(
              result = TypeMismatch(
                expected = "array"
              ),
              pointer = Pointer.parse("/foo")
            )
          )
        )
        assertEquals(result.valid, false)
      }
    }
  }

  test("$ref to validation spec, with two '$ref's") {
    val lazyResolver = Some(MetaSchemas.lazyResolver)

    withSchema(refToValidationSpec) { schema =>
      assertValidate("""{ "$defs": { "foo": { "type": "boolean" } } }""".stripMargin)(schema, lazyResolver) { result =>
        assertErrors(result, Seq())
        assertEquals(result.problems.errors, Seq())
        assertEquals(result.valid, true)
      }
      assertValidate("""{ "$defs": { "foo": { "type": ["boolean"] } } }""".stripMargin)(schema, lazyResolver) {
        result =>
          assertErrors(result, Seq())
          assertEquals(result.problems.errors, Seq())
          assertEquals(result.valid, true)
      }
      assertValidate("""{ "$defs": { "foo": { "type": 13 } } }""".stripMargin)(schema, lazyResolver) { result =>
        assertErrors(
          result,
          Seq(
            typedjson.processor.WithPointer(
              result = NotInEnum(
                values = Seq("array", "boolean", "integer", "null", "number", "object", "string").map(StringValue(_))
              ),
              pointer = Pointer.parse("/$defs/foo/type")
            ),
            typedjson.processor.WithPointer(
              result = TypeMismatch(
                expected = "array"
              ),
              pointer = Pointer.parse("/$defs/foo/type")
            )
          )
        )
        assertEquals(result.valid, false)
      }
    }
  }

}