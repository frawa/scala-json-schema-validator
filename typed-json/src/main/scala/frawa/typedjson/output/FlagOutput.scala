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
import frawa.typedjson.validation.ValidationError

import scala.collection.immutable.Seq

// see https://json-schema.org/draft/2020-12/json-schema-core.html#name-flag

case class FlagOutput(
    valid: Boolean,
    pointer: Pointer = Pointer.empty,
    annotations: Seq[OutputOps.Annotation] = Seq.empty
)

object FlagOutput:
  import OutputOps.Annotation
  given OutputOps[FlagOutput] with
    def valid(pointer: Pointer): FlagOutput                           = FlagOutput(true, pointer)
    def invalid(error: ValidationError, pointer: Pointer): FlagOutput = FlagOutput(false, pointer)

    def all(os: Seq[FlagOutput], pointer: Pointer): FlagOutput =
      FlagOutput(
        os.forall(_.valid),
        pointer,
        OutputOps.mergeAnnotations(os.filter(_.pointer == pointer).flatMap(_.annotations))
      )

    extension (o: FlagOutput)
      def not(pointer: Pointer): FlagOutput = o.copy(valid = !o.valid, annotations = Seq())
      def isValid: Boolean                  = o.valid
      def withAnnotations(annotations: Seq[Annotation]): FlagOutput =
        o.copy(annotations = o.annotations ++ annotations)
      def getAnnotations(): Seq[Annotation]                                      = o.annotations
      def forKeyword(kl: KeywordLocation, k: Option[Keyword] = None): FlagOutput = o
      def withError(error: ValidationError): FlagOutput                          = o
      def isAggregating(os: Seq[FlagOutput]): FlagOutput                         = o
