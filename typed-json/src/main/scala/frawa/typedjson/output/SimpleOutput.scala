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
import frawa.typedjson.util.WithPointer
import frawa.typedjson.validation.NotInvalid
import frawa.typedjson.validation.ValidationError

import scala.collection.immutable.Seq

// TODO this will converge to "basic" output format,
// see https://json-schema.org/draft/2020-12/json-schema-core.html#name-basic

case class SimpleOutput(
    valid: Boolean,
    errors: Seq[SimpleOutput.Error] = Seq(),
    pointer: Pointer = Pointer.empty,
    annotations: Seq[OutputOps.Annotation] = Seq.empty
)

object SimpleOutput:
  import OutputOps.Annotation

  type Error = WithPointer[ValidationError]

  given OutputOps[SimpleOutput] with
    def valid(pointer: Pointer): SimpleOutput = SimpleOutput(true, Seq(), pointer)
    def invalid(error: ValidationError, pointer: Pointer): SimpleOutput =
      SimpleOutput(false, Seq(WithPointer(error, pointer)), pointer)

    def all(os: Seq[SimpleOutput], pointer: Pointer): SimpleOutput =
      val valid = os.forall(_.valid)
      val annotations =
        if valid then
          OutputOps.mergeAnnotations(os.filter(_.pointer == pointer).flatMap(_.annotations))
        else Seq()
      SimpleOutput(
        valid,
        os.flatMap(_.errors),
        pointer,
        annotations
      )

    extension (o: SimpleOutput)
      def not(pointer: Pointer): SimpleOutput =
        if o.valid then
          o.copy(
            valid = false,
            errors = Seq(WithPointer(NotInvalid(), pointer)),
            annotations = Seq()
          )
        else o.copy(valid = true, errors = Seq())
      def isValid: Boolean = o.valid
      def withAnnotations(annotations: Seq[Annotation]): SimpleOutput =
        o.copy(annotations = o.annotations ++ annotations)
      def getAnnotations(): Seq[Annotation]                                        = o.annotations
      def forKeyword(kl: KeywordLocation, k: Option[Keyword] = None): SimpleOutput = o
      def withError(error: ValidationError): SimpleOutput =
        o.copy(errors = WithPointer(error, o.pointer) +: o.errors)
      def isAggregating(os: Seq[SimpleOutput]): SimpleOutput =
        o
