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

package frawa.typedjson.processor

import frawa.typedjson.parser.ArrayValue
import frawa.typedjson.parser.BoolValue
import frawa.typedjson.parser.NumberValue
import frawa.typedjson.parser.ObjectValue
import frawa.typedjson.parser.StringValue
import frawa.typedjson.parser.Value
import frawa.typedjson.processor.SchemaProblems.{
  InvalidSchemaValue,
  MissingDynamicReference,
  MissingReference,
  SchemaError
}
import frawa.typedjson.util.UriUtil

import java.net.URI
import scala.reflect.ClassTag

sealed trait Keyword
sealed trait SimpleKeyword  extends Keyword
sealed trait TypeKeyword    extends SimpleKeyword
sealed trait NestingKeyword extends Keyword

case class TrivialKeyword(v: Boolean)                                    extends SimpleKeyword
case object NullTypeKeyword                                              extends TypeKeyword
case object BooleanTypeKeyword                                           extends TypeKeyword
case object StringTypeKeyword                                            extends TypeKeyword
case object NumberTypeKeyword                                            extends TypeKeyword
case object IntegerTypeKeyword                                           extends TypeKeyword
case object ArrayTypeKeyword                                             extends TypeKeyword
case object ObjectTypeKeyword                                            extends TypeKeyword
case class ObjectRequiredKeyword(names: Seq[String])                     extends SimpleKeyword
case class NotKeyword(keywords: Keywords)                                extends NestingKeyword
case class AllOfKeyword(keywords: Seq[Keywords])                         extends NestingKeyword
case class AnyOfKeyword(keywords: Seq[Keywords])                         extends NestingKeyword
case class OneOfKeyword(keywords: Seq[Keywords])                         extends NestingKeyword
case class UnionTypeKeyword(keywords: Seq[Keywords.KeywordWithLocation]) extends NestingKeyword
case class EnumKeyword(values: Seq[Value])                               extends SimpleKeyword
case class ArrayItemsKeyword(
    items: Option[Keywords] = None,
    prefixItems: Seq[Keywords] = Seq()
) extends NestingKeyword
case class ObjectPropertiesKeyword(
    properties: Map[String, Keywords] = Map(),
    patternProperties: Map[String, Keywords] = Map(),
    additionalProperties: Option[Keywords] = None
) extends NestingKeyword
case class IfThenElseKeyword(
    ifKeywords: Option[Keywords] = None,
    thenKeywords: Option[Keywords] = None,
    elseKeywords: Option[Keywords] = None
) extends NestingKeyword
case class PatternKeyword(pattern: String)                                                      extends SimpleKeyword
case class FormatKeyword(format: String)                                                        extends SimpleKeyword
case class MinimumKeyword(min: BigDecimal, exclude: Boolean = false)                            extends SimpleKeyword
case class UniqueItemsKeyword(unique: Boolean)                                                  extends SimpleKeyword
case class PropertyNamesKeyword(keywords: Keywords)                                             extends NestingKeyword
case class LazyResolveKeyword(resolved: URI, resolve: () => Either[Seq[SchemaError], Keywords]) extends NestingKeyword
case class MultipleOfKeyword(n: BigDecimal)                                                     extends SimpleKeyword
case class MaximumKeyword(max: BigDecimal, exclude: Boolean = false)                            extends SimpleKeyword
case class MaxLengthKeyword(max: BigDecimal)                                                    extends SimpleKeyword
case class MinLengthKeyword(min: BigDecimal)                                                    extends SimpleKeyword
case class MaxItemsKeyword(max: BigDecimal)                                                     extends SimpleKeyword
case class MinItemsKeyword(min: BigDecimal)                                                     extends SimpleKeyword
case class MaxPropertiesKeyword(max: BigDecimal)                                                extends SimpleKeyword
case class MinPropertiesKeyword(min: BigDecimal)                                                extends SimpleKeyword
case class DependentRequiredKeyword(required: Map[String, Seq[String]])                         extends SimpleKeyword
case class DependentSchemasKeyword(keywords: Map[String, Keywords])                             extends NestingKeyword
case class ContainsKeyword(schema: Option[Keywords] = None, min: Option[Int] = None, max: Option[Int] = None)
    extends NestingKeyword
case class UnevaluatedItemsKeyword(pushed: Keywords, unevaluated: Keywords)      extends NestingKeyword
case class UnevaluatedPropertiesKeyword(pushed: Keywords, unevaluated: Keywords) extends NestingKeyword

case class Keywords(
    schema: SchemaValue,
    keywords: Seq[Keywords.KeywordWithLocation] = Seq.empty[Keywords.KeywordWithLocation],
    ignored: Set[String] = Set.empty
) {
  import frawa.typedjson.util.SeqUtil._
  import Keywords._

  private def add(keyword: Keyword)(implicit scope: DynamicScope): Keywords =
    this.copy(keywords = keywords :+ localized(keyword, scope))

  private def addAll(
      schemas: Seq[SchemaValue]
  )(
      f: Seq[Keywords] => Keyword
  )(implicit resolver: SchemaResolver, scope: DynamicScope): Either[SchemaErrors, Keywords] = {
    val keywords0 = schemas.zipWithIndex.map { case (v, i) => Keywords.parseKeywords(v, scope.push(i)) }
    for {
      keywords <- sequenceAllLefts(keywords0)
    } yield {
      add(f(keywords)).withIgnored(keywords.flatMap(_.ignored).toSet)
    }
  }

  type SchemaErrors = Keywords.SchemaErrors

  private def withKeyword(keyword: String, value: Value, scope: DynamicScope)(implicit
      resolver: SchemaResolver
  ): Either[SchemaErrors, Keywords] = {
    implicit val scope1: DynamicScope = scope.push(keyword)
    (keyword, value) match {
      // TODO validation vocabulary
      case ("type", StringValue(typeName)) =>
        Right(
          getTypeCheck(typeName)
            .map(add(_))
            .getOrElse(withIgnored(s"$keyword-$typeName"))
        )

      // TODO validation vocabulary
      case ("type", ArrayValue(values)) =>
        def typeNames = Value.asStrings(values)

        def keywords = typeNames
          .flatMap(getTypeCheck)
          .map(localized(_, scope1))

        Right(add(UnionTypeKeyword(keywords)))

      case ("not", value) =>
        for {
          keywords <- Keywords.parseKeywords(SchemaValue(value), scope1)
        } yield {
          add(NotKeyword(keywords))
        }

      case ("items", value) =>
        for {
          keywords <- Keywords.parseKeywords(SchemaValue(value), scope1)
        } yield {
          updateKeyword(ArrayItemsKeyword())(check => check.copy(items = Some(keywords)))
        }

      case ("prefixItems", ArrayValue(vs)) =>
        val keywords0 = vs.map(v => Keywords.parseKeywords(SchemaValue(v), scope1))
        for {
          keywords <- sequenceAllLefts(keywords0)
        } yield {
          updateKeyword(ArrayItemsKeyword())(check => check.copy(prefixItems = keywords))
        }

      case ("unevaluatedItems", v) =>
        for {
          keywords <- Keywords.parseKeywords(SchemaValue(v), scope1)
        } yield {
          Keywords(schema).add(UnevaluatedItemsKeyword(this, keywords))(scope)
        }

      case ("properties", ObjectValue(properties)) =>
        mapKeywordsFor(properties, scope1) { keywords =>
          updateKeyword(ObjectPropertiesKeyword())(keyword => keyword.copy(properties = keyword.properties ++ keywords))
        }

      case ("patternProperties", ObjectValue(properties)) =>
        mapKeywordsFor(properties, scope1) { keywords =>
          updateKeyword(ObjectPropertiesKeyword())(keyword => keyword.copy(patternProperties = keywords))
        }

      case ("additionalProperties", value) =>
        for {
          keywords <- Keywords.parseKeywords(SchemaValue(value), scope1)
        } yield {
          updateKeyword(ObjectPropertiesKeyword())(keyword => keyword.copy(additionalProperties = Some(keywords)))
        }

      case ("unevaluatedProperties", v) =>
        for {
          keywords <- Keywords.parseKeywords(SchemaValue(v), scope1)
        } yield {
          Keywords(schema).add(UnevaluatedPropertiesKeyword(this, keywords))(scope)
        }

      case ("required", ArrayValue(values)) =>
        def names = Value.asStrings(values)

        Right(add(ObjectRequiredKeyword(names)))

      case ("allOf", ArrayValue(values)) =>
        addAll(values.map(SchemaValue(_)))(AllOfKeyword)

      case ("anyOf", ArrayValue(values)) =>
        addAll(values.map(SchemaValue(_)))(AnyOfKeyword)

      case ("oneOf", ArrayValue(values)) =>
        addAll(values.map(SchemaValue(_)))(OneOfKeyword)

      case ("if", value) =>
        updateKeywordsInside(SchemaValue(value))(IfThenElseKeyword()) { (keywords, keyword) =>
          keyword.copy(ifKeywords = Some(keywords))
        }

      case ("then", value) =>
        updateKeywordsInside(SchemaValue(value))(IfThenElseKeyword()) { (keywords, keyword) =>
          keyword.copy(thenKeywords = Some(keywords))
        }

      case ("else", value) =>
        updateKeywordsInside(SchemaValue(value))(IfThenElseKeyword()) { (keywords, keyword) =>
          keyword.copy(elseKeywords = Some(keywords))
        }

      // TODO validation vocabulary
      case ("enum", ArrayValue(values)) =>
        Right(add(EnumKeyword(values)))

      // TODO validation vocabulary
      case ("const", value) =>
        Right(add(EnumKeyword(Seq(value))))

      case ("$id", StringValue(_)) =>
        // handled during load
        Right(this)

      case ("$anchor", StringValue(_)) =>
        // handled during load
        Right(this)

      case ("$dynamicAnchor", StringValue(_)) =>
        // handled during load
        Right(this)

      case ("$defs", ObjectValue(_)) =>
        // handled during load
        Right(this)

      case ("$ref", StringValue(ref)) =>
        for {
          resolution <- resolver
            .resolveRef(ref)
            .map(Right(_))
            // .getOrElse(throw new IllegalStateException(s"$ref not found"))
            .getOrElse(Left(Seq(WithPointer(MissingReference(ref)))))
          keyword = lazyResolve(resolution, scope1)
        } yield {
          add(keyword)
        }

      case ("$dynamicRef", StringValue(ref)) =>
        for {
          resolution <- resolver
            .resolveDynamicRef(ref, scope)
            .map(Right(_))
            .getOrElse(Left(Seq(WithPointer(MissingDynamicReference(ref)))))
          keyword = lazyResolve(resolution, scope1)
        } yield {
          add(keyword)
        }

      case ("$comment", StringValue(_)) =>
        // only for schema authors and readers
        Right(this)

      case ("title", StringValue(_)) =>
        // ignore annotations
        Right(this)

      case ("default", _) =>
        // ignore annotations
        Right(this)

      case ("description", StringValue(_)) =>
        // ignore annotations
        Right(this)

      // TODO validation vocabulary
      case ("pattern", StringValue(pattern)) =>
        Right(add(PatternKeyword(pattern)))

      // TODO validation vocabulary
      case ("format", StringValue(format)) =>
        Right(add(FormatKeyword(format)))

      // TODO validation vocabulary
      case ("minimum", NumberValue(v)) =>
        Right(add(MinimumKeyword(v)))

      // TODO validation vocabulary
      case ("exclusiveMinimum", NumberValue(v)) =>
        Right(add(MinimumKeyword(v, exclude = true)))

      // TODO validation vocabulary
      case ("minItems", NumberValue(v)) if v >= 0 =>
        Right(add(MinItemsKeyword(v)))

      // TODO validation vocabulary
      case ("uniqueItems", BoolValue(v)) =>
        Right(add(UniqueItemsKeyword(v)))

      case ("propertyNames", value) =>
        for {
          keywords <- Keywords.parseKeywords(SchemaValue(value), scope1)
        } yield {
          add(PropertyNamesKeyword(keywords))
        }

      // TODO validation vocabulary
      case ("multipleOf", NumberValue(v)) if v > 0 =>
        Right(add(MultipleOfKeyword(v)))

      // TODO validation vocabulary
      case ("maximum", NumberValue(v)) =>
        Right(add(MaximumKeyword(v)))

      // TODO validation vocabulary
      case ("exclusiveMaximum", NumberValue(v)) =>
        Right(add(MaximumKeyword(v, exclude = true)))

      // TODO validation vocabulary
      case ("maxLength", NumberValue(v)) if v >= 0 =>
        Right(add(MaxLengthKeyword(v)))

      // TODO validation vocabulary
      case ("minLength", NumberValue(v)) if v >= 0 =>
        Right(add(MinLengthKeyword(v)))

      // TODO validation vocabulary
      case ("maxItems", NumberValue(v)) if v >= 0 =>
        Right(add(MaxItemsKeyword(v)))

      // TODO validation vocabulary
      case ("maxProperties", NumberValue(v)) if v >= 0 =>
        Right(add(MaxPropertiesKeyword(v)))

      // TODO validation vocabulary
      case ("minProperties", NumberValue(v)) if v >= 0 =>
        Right(add(MinPropertiesKeyword(v)))

      // TODO validation vocabulary
      case ("dependentRequired", ObjectValue(v)) =>
        val vv = v.view.flatMap {
          case (p, ArrayValue(vs)) =>
            Some(
              (
                p,
                vs.flatMap {
                  case StringValue(value) => Some(value)
                  case _                  => None
                }
              )
            )
          case _ => None
        }.toMap
        Right(add(DependentRequiredKeyword(vv)))

      case ("dependentSchemas", ObjectValue(ps)) =>
        val keywords0 = ps.view.map { case (p, v) =>
          Keywords
            .parseKeywords(SchemaValue(v), scope1)
            .map(p -> _)
        }.toSeq
        for {
          keywords <- sequenceAllLefts(keywords0).map(_.toMap)
        } yield {
          add(DependentSchemasKeyword(keywords))
        }

      case ("contains", v) =>
        for {
          keywords <- Keywords.parseKeywords(SchemaValue(v), scope1)
        } yield {
          updateKeyword(ContainsKeyword())(check => check.copy(schema = Some(keywords)))
        }

      // TODO validation vocabulary
      case ("minContains", NumberValue(v)) if v >= 0 =>
        Right(updateKeyword(ContainsKeyword())(keyword => keyword.copy(min = Some(v.toInt))))

      // TODO validation vocabulary
      case ("maxContains", NumberValue(v)) if v >= 0 =>
        Right(updateKeyword(ContainsKeyword())(keyword => keyword.copy(max = Some(v.toInt))))

      // // TODO
      // case ("$vocabulary", v) => {
      //   Right(this)
      // }
      // // TODO
      // case ("$schema", v) => {
      //   Right(this)
      // }
      // // TODO
      // case ("deprecated", v) => {
      //   Right(this)
      // }

      case _ => Right(withIgnored(keyword))
    }
  }

  private def lazyResolve(resolution: SchemaResolver.Resolution, scope: DynamicScope): LazyResolveKeyword = {
    val resolveLater = { () =>
      Keywords.parseKeywords(resolution, scope)
    }
    val resolved = resolution._2.base
    LazyResolveKeyword(resolved, resolveLater)
  }

  private def mapKeywordsFor(
      props: Map[String, Value],
      scope: DynamicScope
  )(f: Map[String, Keywords] => Keywords)(implicit resolver: SchemaResolver): Either[Seq[SchemaError], Keywords] = {
    val propKeywords0 = props.view
      .map { case (prop, v) =>
        (prop, Keywords.parseKeywords(SchemaValue(v), scope.push(prop)))
      }
      .map {
        case (prop, Right(keywords)) => Right((prop, keywords))
        case (prop, Left(errors))    => Left(errors.map(_.prefix(Pointer.empty / prop)))
      }
      .toSeq
    for {
      propKeywords <- sequenceAllLefts(propKeywords0)
      keywords = Map.from(propKeywords)
      ignored  = keywords.values.flatMap(_.ignored).toSet
    } yield {
      f(keywords).withIgnored(ignored)
    }
  }

  private def updateKeyword[K <: Keyword: ClassTag](
      newKeyword: => K
  )(f: K => K)(implicit scope: DynamicScope): Keywords = {
    val keywords0: Seq[KeywordWithLocation] =
      if (
        keywords.exists {
          case UriUtil.WithLocation(_, _: K) => true
          case _                             => false
        }
      ) {
        keywords
      } else {
        keywords :+ localized(newKeyword, scope)
      }
    this.copy(keywords = keywords0.map {
      case UriUtil.WithLocation(uri, keyword: K) => UriUtil.WithLocation(uri, f(keyword))
      case c @ _                                 => c
    })
  }

  private def updateKeywordsInside[K <: Keyword: ClassTag](
      schema: SchemaValue
  )(
      newKeyword: => K
  )(f: (Keywords, K) => K)(implicit resolver: SchemaResolver, scope: DynamicScope): Either[SchemaErrors, Keywords] = {
    for {
      keywords <- Keywords.parseKeywords(schema, scope)
    } yield {
      updateKeyword(newKeyword)(f(keywords, _))
    }
  }

  private def withIgnored(keyword: String): Keywords =
    this.copy(ignored = ignored + keyword)

  private def withIgnored(ignored: Set[String]): Keywords =
    this.copy(ignored = ignored.concat(ignored))

  private def getTypeCheck(typeName: String): Option[TypeKeyword] =
    typeName match {
      case "null"    => Some(NullTypeKeyword)
      case "boolean" => Some(BooleanTypeKeyword)
      case "string"  => Some(StringTypeKeyword)
      case "number"  => Some(NumberTypeKeyword)
      case "integer" => Some(IntegerTypeKeyword)
      case "array"   => Some(ArrayTypeKeyword)
      case "object"  => Some(ObjectTypeKeyword)
      case _         => None
    }
}

object Keywords {
  type SchemaErrors        = Seq[SchemaError]
  type KeywordWithLocation = UriUtil.WithLocation[Keyword]

  def parseKeywords(schema: SchemaValue, scope: DynamicScope)(implicit
      resolver: SchemaResolver
  ): Either[SchemaErrors, Keywords] = {
    val resolver1 = SchemaValue
      .id(schema)
      .map(id => resolver.withBase(resolver.absolute(id)))
      .getOrElse(resolver)
    val resolution: SchemaResolver.Resolution = (schema, resolver1)
    parseKeywords(resolution, scope)
  }

  def parseKeywords(resolution: SchemaResolver.Resolution, scope: DynamicScope): Either[SchemaErrors, Keywords] = {
    val schema    = resolution._1
    val resolver1 = resolution._2
//    implicit val scope1 = scope.push(resolver1.base)
    implicit val scope1: DynamicScope = SchemaValue
      .id(schema)
      .map(id => scope.push(resolver1.absolute(id)))
      .getOrElse(scope)
    schema.value match {
      case BoolValue(v) => Right(Keywords(schema).add(TrivialKeyword(v)))
      case ObjectValue(keywords) =>
        if (keywords.isEmpty) {
          Right(Keywords(schema).add(TrivialKeyword(true)))
        } else {
          keywords
            .foldLeft[Either[SchemaErrors, Keywords]](Right(Keywords(schema))) { case (keywords, (keyword, value)) =>
              val prefix = Pointer.empty / keyword
              accumulate(
                keywords,
                keywords
                  .flatMap(
                    _.withKeyword(keyword, value, scope1)(resolver1).swap
                      .map(_.map(_.prefix(prefix)))
                      .swap
                  )
              )
            }
        }
      case _ => Left(Seq(WithPointer(InvalidSchemaValue(schema.value))))
    }
  }

  private def accumulate(
      previous: Either[SchemaErrors, Keywords],
      current: Either[SchemaErrors, Keywords]
  ): Either[SchemaErrors, Keywords] = (previous, current) match {
    case (Right(_), Right(current))     => Right(current)
    case (Right(_), Left(errors))       => Left(errors)
    case (Left(previous), Left(errors)) => Left(previous :++ errors)
    case (Left(previous), _)            => Left(previous)
  }

  def localized(keyword: Keyword, scope: DynamicScope): KeywordWithLocation = {
    import frawa.typedjson.util.UriUtil._
    scope.uris.lastOption.map(WithLocation(_, keyword)).getOrElse(WithLocation(uri("#"), keyword))
  }
}