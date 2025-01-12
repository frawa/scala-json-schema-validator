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

package frawa.typedjson.output

import frawa.typedjson.keywords.Evaluated
import frawa.typedjson.keywords.EvaluatedIndices
import frawa.typedjson.keywords.EvaluatedProperties
import frawa.typedjson.keywords.Ignored
import frawa.typedjson.keywords.Keyword
import frawa.typedjson.keywords.KeywordLocation
import frawa.typedjson.pointer.Pointer
import frawa.typedjson.validation.ValidationError

import scala.collection.immutable.Seq

trait OutputOps[O]: // extends Monoid[O]:
  import OutputOps.Annotation

  def valid(pointer: Pointer): O
  def invalid(error: ValidationError, pointer: Pointer): O

  def all(os: Seq[O], pointer: Pointer): O

  extension (o: O)
    def not(pointer: Pointer): O
    def isValid: Boolean
    def withAnnotation(annotation: Annotation): O = withAnnotations(Seq(annotation))
    def withAnnotations(annotations: Seq[Annotation]): O
    def getAnnotations(): Seq[Annotation]
    def forKeyword(kl: KeywordLocation, k: Option[Keyword] = None): O
    def withError(error: ValidationError): O
    def isAggregating(os: Seq[O]): O

object OutputOps:
  trait Annotation

  def mergeAnnotations(es: Seq[Annotation]): Seq[Annotation] =
    val es1 = mergeEvaluatedAnnotations(
      es.filter(_.isInstanceOf[Evaluated]).map(_.asInstanceOf[Evaluated])
    )
    val es2 = es.filterNot(_.isInstanceOf[Evaluated])
    es1 ++ es2

  private def mergeEvaluatedAnnotations(es: Seq[Evaluated]): Seq[Annotation] =
    val indices = es.flatMap {
      case EvaluatedIndices(indices) => indices
      case _                         => Set()
    }.toSet
    val properties = es.flatMap {
      case EvaluatedProperties(properties) => properties
      case _                               => Set()
    }.toSet
    val ignored = es.flatMap {
      case Ignored(keywords) => keywords
      case _                 => Set()
    }.toSet
    val es1 = if indices.nonEmpty then Seq(EvaluatedIndices(indices)) else Seq()
    val es2 = if properties.nonEmpty then Seq(EvaluatedProperties(properties)) else Seq()
    val es3 = if ignored.nonEmpty then Seq(Ignored(ignored)) else Seq()
    es1 ++ es2 ++ es3
