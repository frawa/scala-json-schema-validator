package frawa.typedjson.js

import frawa.typedjson.keywords._
import frawa.typedjson.meta.MetaSchemas
import frawa.typedjson.parser.{JawnParser, Offset, OffsetParser}
import frawa.typedjson.pointer.Pointer
import frawa.typedjson.validation.{ValidationProcessing, ValidationResult}

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportAll, JSExportTopLevel}

@JSExportTopLevel("TypedJsonFactory")
object TypedJsonFactory {
  private val parser = new JawnParser

  @JSExport
  def create(): TypedJson = {
    TypedJson(None)
  }

  @JSExport
  def withMetaSchema(): TypedJson = {
    val resolver     = MetaSchemas.lazyResolver
    val base         = MetaSchemas.draft202012
    val Some(schema) = resolver(base.resolve("schema"))
    val vocabulary   = Vocabulary.specDialect()
    val keywords     = Keywords(schema, Some(vocabulary), Some(resolver))
    TypedJson(Some(keywords))
  }

  def parseJsonOffsetValue(json: String): Either[OffsetParser.ParseError, Offset.Value] = {
    parser.parseWithOffset(json)
  }

  def pointerAt(value: Offset.Value, at: Int): Pointer = {
    parser.pointerAt(value)(at)
  }

  def offsetAt(pointer: Pointer, value: Offset.Value): Option[Offset] = {
    parser.offsetAt(value)(pointer)
  }
}

@JSExportTopLevel("TypedJson")
case class TypedJson(
    keywords: Option[Either[SchemaProblems, Keywords]],
    value: Option[Offset.Value] = None,
    result: Option[Either[OffsetParser.ParseError, Result[ValidationResult]]] = None
) {
  @JSExport
  def withSchema(schema: TypedJson): TypedJson = {
    val resolver    = MetaSchemas.lazyResolver
    val schemaValue = schema.value.map(Offset.withoutOffset).map(SchemaValue.root)
    val vocabulary  = Vocabulary.specDialect()
    val keywords    = schemaValue.map(Keywords(_, Some(vocabulary), Some(resolver)))
    this.copy(keywords = keywords).validate()
  }

  @JSExport
  def forValue(json: String): TypedJson = {
    val parsed = TypedJsonFactory.parseJsonOffsetValue(json)
    parsed match {
      case Right(value) =>
        this.copy(value = Some(value), result = None).validate()
      case Left(error) =>
        this.copy(value = None, result = Some(Left(error)))
    }
  }

  private def validate(): TypedJson = {
    keywords match {
      case Some(Right(keywords)) =>
        value match {
          case Some(value) =>
            val evaluator = Evaluator(keywords, ValidationProcessing())
            val result    = evaluator(InnerValue(Offset.withoutOffset(value)))
            this.copy(result = Some(Right(result)))
          case None =>
            this
        }
      case _ => this
    }
  }

  @JSExport
  def markers(): js.Array[Marker] = {
    val markers = (keywords, value, result) match {
      case (Some(Right(_)), Some(value), Some(Right(result))) if !result.valid =>
        val offsetAt = pointer => TypedJsonFactory.offsetAt(pointer, value)
        result.results
          .flatMap(_.errors)
          .map(Marker.fromError(offsetAt))
      case (Some(Left(problems)), _, _) => problems.errors.map(Marker.fromSchemaError) // TODO not needed?
      case (_, _, Some(Left(error))) =>
        Seq(Marker.fromParsingError(error))
      case _ => Seq.empty
    }
    js.Array(markers: _*)
  }

  @JSExport
  def suggestAt(offset: Int): js.Array[String] = {
    val pointer = value.map(TypedJsonFactory.pointerAt(_, offset))
    js.Array(Seq("hello", "world"): _*)
  }
}

@JSExportTopLevel("Marker")
@JSExportAll
case class Marker(
    start: Int,
    end: Int,
    pointer: String,
    message: String,
    severity: String
)

object Marker {
  def fromSchemaError(error: SchemaProblems.SchemaError): Marker = {
    // TODO localized messages
    val message = error.result.toString
    Marker(0, 0, error.pointer.toString, message, "error")
  }

  def fromError(offsetAt: Pointer => Option[Offset])(error: ValidationResult.Error): Marker = {
    val offset       = offsetAt(error.pointer)
    val (start, end) = offset.map(o => (o.start, o.end)).getOrElse((0, 0))
    // TODO localized messages
    val message = error.result.toString
    Marker(start, end, error.pointer.toString, message, "error")
  }

  def fromParsingError(error: OffsetParser.ParseError): Marker = {
    Marker(error.offset, error.offset, "", error.message, "error")
  }
}