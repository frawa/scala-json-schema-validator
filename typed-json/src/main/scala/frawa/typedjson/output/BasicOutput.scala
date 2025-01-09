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
import frawa.typedjson.validation.SubSchemaFailed
import frawa.typedjson.validation.ValidationError

import scala.collection.immutable.Seq

// TODO this will converge to "basic" output format,
// see https://json-schema.org/draft/2020-12/json-schema-core.html#name-basic

case class BasicOutput(
    valid: Boolean,
    annotations: Seq[OutputOps.Annotation] = Seq.empty,
    error: Option[ValidationError] = None,
    errors: Seq[BasicOutput.Error] = Seq(),
    instanceLocation: Pointer = Pointer.empty,
    keywordLocation: Option[KeywordLocation] = None
)

object BasicOutput:
  case class Error(
      error: ValidationError,
      instanceLocation: Pointer = Pointer.empty,
      keywordLocation: KeywordLocation = KeywordLocation.empty
  )

  given OutputOps[BasicOutput] with
    def valid(pointer: Pointer): BasicOutput = BasicOutput(true, instanceLocation = pointer)
    def invalid(error: ValidationError, pointer: Pointer): BasicOutput =
      BasicOutput(false, error = Some(error), instanceLocation = pointer)

    def all(os: Seq[BasicOutput], pointer: Pointer): BasicOutput =
      val valid = os.forall(_.valid)
      val annotations =
        if valid then
          OutputOps.mergeAnnotations(
            os.filter(_.instanceLocation == pointer).flatMap(_.annotations)
          )
        else Seq()
      val errors =
        if valid then Seq()
        else
          os.filterNot(_.isValid).flatMap { o =>
            val self =
              Error(
                o.error.getOrElse(SubSchemaFailed()),
                o.instanceLocation,
                o.keywordLocation.getOrElse(KeywordLocation.empty)
              )
            self +: o.errors
          }
      if errors.size == 1 then
        // TODO realy?
        BasicOutput(
          valid,
          annotations,
          error = Some(errors(0).error),
          instanceLocation = errors(0).instanceLocation,
          keywordLocation = Some(errors(0).keywordLocation)
        )
      else
        BasicOutput(
          valid,
          annotations,
          errors = errors,
          instanceLocation = pointer
        )

    extension (o: BasicOutput)
      def not(pointer: Pointer): BasicOutput =
        if o.valid then
          BasicOutput(valid = false, error = Some(NotInvalid()), instanceLocation = pointer)
        else BasicOutput(valid = true, instanceLocation = pointer)
      def isValid: Boolean = o.valid
      def withAnnotations(annotations: Seq[OutputOps.Annotation]): BasicOutput =
        o.copy(annotations = o.annotations ++ annotations)
      def getAnnotations(): Seq[OutputOps.Annotation] = o.annotations
      def forKeyword(kl: KeywordLocation, k: Option[Keyword] = None): BasicOutput =
        if o.keywordLocation.isDefined && !o.keywordLocation.contains(kl) then
          // TODO avoid this situation
          // new RuntimeException().printStackTrace()
          o
        else o.copy(keywordLocation = Some(kl))
      def withError(error: ValidationError): BasicOutput =
        o.copy(error = Some(error))
      def isAggregating(os: Seq[BasicOutput]): BasicOutput =
        o
