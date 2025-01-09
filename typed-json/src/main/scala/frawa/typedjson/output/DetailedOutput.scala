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

import frawa.typedjson.keywords.Keyword
import frawa.typedjson.keywords.KeywordLocation
import frawa.typedjson.pointer.Pointer
import frawa.typedjson.validation.NotInvalid
import frawa.typedjson.validation.ValidationError

import scala.collection.immutable.Seq

// TODO this will converge to "basic" output format,
// see https://json-schema.org/draft/2020-12/json-schema-core.html#name-basic

case class DetailedOutput(
    valid: Boolean,
    instanceLocation: Pointer = Pointer.empty,
    keywordLocation: Option[KeywordLocation] = None,
    error: Option[ValidationError] = None,
    errors: Seq[DetailedOutput] = Seq(),
    annotations: Seq[OutputOps.Annotation] = Seq.empty
)

object DetailedOutput:

  given OutputOps[DetailedOutput] with
    def valid(pointer: Pointer): DetailedOutput = DetailedOutput(true, instanceLocation = pointer)
    def invalid(error: ValidationError, pointer: Pointer): DetailedOutput =
      DetailedOutput(false, error = Some(error), instanceLocation = pointer)

    def all(os: Seq[DetailedOutput], pointer: Pointer): DetailedOutput =
      val valid = os.forall(_.valid)
      val annotations =
        if valid then
          OutputOps.mergeAnnotations(
            os.filter(_.instanceLocation == pointer).flatMap(_.annotations)
          )
        else Seq()
      val errors =
        if valid then Seq()
        else os.filterNot(_.valid)
      if errors.size == 1 then
        // TODO really?
        errors(0)
      else
        DetailedOutput(
          valid,
          errors = errors,
          instanceLocation = pointer,
          annotations = annotations
        )

    extension (o: DetailedOutput)
      def not(pointer: Pointer): DetailedOutput =
        if o.valid then
          DetailedOutput(valid = false, error = Some(NotInvalid()), instanceLocation = pointer)
        else DetailedOutput(valid = true, instanceLocation = pointer)
      def isValid: Boolean = o.valid
      def withAnnotations(annotations: Seq[OutputOps.Annotation]): DetailedOutput =
        o.copy(annotations = o.annotations ++ annotations)
      def getAnnotations(): Seq[OutputOps.Annotation] = o.annotations
      def forKeyword(kl: KeywordLocation, k: Option[Keyword] = None): DetailedOutput =
        if o.keywordLocation.isDefined && !o.keywordLocation.contains(kl) then
          // TODO avoid this situation
          // new RuntimeException().printStackTrace()
          o
        else o.copy(keywordLocation = Some(kl))
      def withError(error: ValidationError): DetailedOutput =
        o.copy(error = Some(error))
      def isAggregating(os: Seq[DetailedOutput]): DetailedOutput =
        o
