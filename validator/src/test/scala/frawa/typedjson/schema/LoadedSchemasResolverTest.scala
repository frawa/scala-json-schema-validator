package frawa.typedjson.schema

import munit.FunSuite
import frawa.typedjson.parser.Value
import frawa.typedjson.parser.ZioParser
import frawa.typedjson.parser.Parser
import frawa.typedjson.parser.NumberValue
import frawa.typedjson.parser.BoolValue
import frawa.typedjson.parser.NullValue
import frawa.typedjson.parser.StringValue
import frawa.typedjson.parser.ArrayValue
import frawa.typedjson.parser.ObjectValue
import scala.reflect.internal.Reporter
import munit.Assertions._
import java.net.URI
import TestUtil._
import UriUtil._

class LoadedSchemasResolverTest extends FunSuite {
  implicit val zioParser = new ZioParser()

  test("first schema loader") {
    val id = "https://example.net/root.json"
    withSchema(s"""{
                  |"$$id": "${id}",
                  |"type": "null"
                  |}""".stripMargin) { schema =>
      val resolver = LoadedSchemasResolver(schema)
      val uri1     = uri(id)
      assertEquals(resolver.base, uri1)
      assertEquals(resolver.resolveRef(id).map(_._1), Some(schema))
    }
  }

  test("$defs") {
    withSchema("""{
                 |"type": "null",
                 |"$defs": {
                 |    "foo": {
                 |        "type": "number"
                 |    }
                 |}
                 |}""".stripMargin) { schema =>
      val resolver = LoadedSchemasResolver(schema)
      assertEquals(resolver.base, uri(""))
      assertEquals(resolver.schemas.size, 1)

      val expected = SchemaValue(
        value = ObjectValue(
          properties = Map(
            "type" -> StringValue(
              value = "number"
            )
          )
        )
      )

      assertEquals(
        resolver.resolveRef("#/$defs/foo").map(_._1),
        Some(expected)
      )
    }
  }

  test("$defs with $id") {
    val id = "https://example.net/root.json"
    withSchema(s"""{
                  |"$$id": "${id}",
                  |"type": "null",
                  |"$$defs": {
                  |    "foo": {
                  |        "$$id": "https://example.net/foo.json",
                  |        "type": "number"
                  |    }
                  |}
                  |}""".stripMargin) { schema =>
      val resolver = LoadedSchemasResolver(schema)
      val uri1     = uri(id)
      assertEquals(resolver.base, uri1)
      assertEquals(resolver.schemas.size, 2)
      assertEquals(resolver.resolveRef(id).map(_._1), Some(schema))

      val expected = SchemaValue(
        value = ObjectValue(
          properties = Map(
            "$id" -> StringValue(
              value = "https://example.net/foo.json"
            ),
            "type" -> StringValue(
              value = "number"
            )
          )
        )
      )

      assertEquals(
        resolver.resolveRef("https://example.net/root.json#/$defs/foo").map(_._1),
        Some(expected)
      )

      assertEquals(
        resolver.resolveRef("https://example.net/foo.json").map(_._1),
        Some(expected)
      )
    }
  }

  test("anchor") {
    val id = "https://example.net/root.json"
    withSchema(s"""{
                  |"$$id": "${id}",
                  |"type": "null",
                  |"$$defs": {
                  |    "foo": {
                  |        "$$anchor": "foo",
                  |        "type": "number"
                  |    }
                  |}
                  |}""".stripMargin) { schema =>
      val resolver = LoadedSchemasResolver(schema)
      val uri1     = uri(id)
      assertEquals(resolver.base, uri1)
      assertEquals(resolver.resolveRef(id).map(_._1), Some(schema))
      val expected = SchemaValue(
        value = ObjectValue(
          properties = Map(
            "$anchor" -> StringValue(
              value = "foo"
            ),
            "type" -> StringValue(
              value = "number"
            )
          )
        )
      )

      assertEquals(
        resolver.resolveRef(s"${id}#foo").map(_._1),
        Some(expected)
      )
    }
  }

  test("$dynamicRef") {
    val id1 = "https://example.net/root1"
    val id2 = "https://example.net/root2"
    withLoadedSchemas(
      List(
        s"""{
           |"$$id": "${id1}",
           |"type": "null",
           |"$$dynamicAnchor": "anchor1"
           |}""".stripMargin,
        s"""{
           |"$$id": "${id2}",
           |"$$dynamicAnchor": "anchor1"
           |}""".stripMargin
      )
    ) { resolver =>
      val uriRoot1 = uri(id1)
      val uriRoot2 = uri(id2)

      val scope1 = DynamicScope.empty.push(uriRoot1)
      val scope2 = DynamicScope.empty.push(uriRoot2).push(uriRoot1)

      val resolver1 = resolver.withBase(uriRoot1)

      val Some((idA, idB)) = for {
        (schemaA, _) <- resolver1.resolveDynamicRef("#anchor1", scope2)
        (schemaB, _) <- resolver1.resolveDynamicRef("#anchor1", scope1)
        idA          <- SchemaValue.id(schemaA)
        idB          <- SchemaValue.id(schemaB)
      } yield ((idA, idB))
      assertEquals(idA, id2)
      assertEquals(idB, id1)
    }
  }

  test("$dynamicAnchor") {
    val id = "https://example.net/root"
    withLoadedSchemas(
      List(s"""|{
               |  "$$id": "${id}",
               |  "$$ref": "list",
               |  "$$defs": {
               |    "foo": {
               |      "$$anchor": "items",
               |      "type": "string"
               |    },
               |    "list": {
               |      "$$id": "list",
               |      "type": "array",
               |      "items": { "$$dynamicRef": "#items" },
               |      "$$defs": {
               |        "items": {
               |          "$$comment": "This is only needed to satisfy the bookending requirement",
               |          "$$dynamicAnchor": "items"
               |        }
               |      }
               |    }
               |  }
               |}""".stripMargin)
    ) { resolver =>
      val uriRoot = uri(id)

      assertEquals(resolver.dynamicSchemas, Set(uriRoot.resolve(uri("list#items"))))
      assertEquals(
        resolver.schemas.keySet,
        Set(
          uriRoot,
          uri(s"${id}#items"),
          uriRoot.resolve(uri("list")),
          uriRoot.resolve(uri("list#items"))
        )
      )

      val getAnchor  = Pointer.empty / "$anchor"
      val getComment = Pointer.empty / "$comment"

      val scope = DynamicScope.empty.push(uriRoot)
      val ok = for {
        (schema1, resolver1) <- resolver.resolveRef(id)
        id1                  <- SchemaValue.id(schema1)
        (schema2, resolver2) <- resolver1.resolveDynamicRef("#items", scope)
        StringValue(anchor2) <- getAnchor(schema2.value)
      } yield {
        assertEquals(id1, id)
        assertEquals(anchor2, "items")
        true
      }
      ok.getOrElse {
        fail("unexpected None")
        false
      }
    }
  }

  test("$dynamicAnchor from root") {
    val id = "https://example.net/root"
    withLoadedSchemas(
      List(s"""|{
               |  "$$id": "${id}",
               |  "$$ref": "list",
               |  "$$defs": {
               |    "foo": {
               |      "$$dynamicAnchor": "items",
               |      "type": "string"
               |    },
               |    "list": {
               |      "$$id": "list",
               |      "type": "array",
               |      "items": { "$$dynamicRef": "#items" },
               |      "$$defs": {
               |        "items": {
               |          "$$comment": "This is only needed to satisfy the bookending requirement",
               |          "$$dynamicAnchor": "items"
               |        }
               |      }
               |    }
               |  }
               |}""".stripMargin)
    ) { resolver =>
      val uriRoot = uri(id)

      assertEquals(
        resolver.dynamicSchemas,
        Set(uri(s"${id}#items"), uriRoot.resolve(uri("list#items")))
      )
      assertEquals(
        resolver.schemas.keySet,
        Set(
          uriRoot,
          uri("https://example.net/root"),
          uri("https://example.net/root#items"),
          uriRoot.resolve(uri("list")),
          uriRoot.resolve(uri("list#items"))
        )
      )

      val getDynamicAnchor = Pointer.empty / "$dynamicAnchor"
      val getType          = Pointer.empty / "type"

      val scope = DynamicScope.empty.push(uriRoot)
      val ok = for {
        (schema1, resolver1)        <- resolver.resolveRef(id)
        id1                         <- SchemaValue.id(schema1)
        (schema2, resolver2)        <- resolver1.resolveDynamicRef("#items", scope)
        StringValue(dynamicAnchor2) <- getDynamicAnchor(schema2.value)
        StringValue(type2)          <- getType(schema2.value)
      } yield {
        assertEquals(id1, id)
        assertEquals(dynamicAnchor2, "items")
        assertEquals(type2, "string")
        true
      }
      ok.getOrElse {
        fail("unexpected None")
        false
      }
    }
  }

  test("caching lazy resolutions") {
    withLoadedSchemas(Seq()) { resolver =>
      assertEquals(resolver.resolveRef("missing"), None)

      val resolver1 = resolver.withLazyResolver { case _ => Some(SchemaValue(NullValue)) }

      val Some((schema, resolver2)) = resolver1.resolveRef("cacheme")
      assertEquals(schema, SchemaValue(NullValue))
      assertEquals(resolver2.isInstanceOf[LoadedSchemasResolver], true)
      assertEquals(resolver2.asInstanceOf[LoadedSchemasResolver].schemas.keySet, Set(uri("cacheme")))
    }
  }
}