package org.scalakittens.types

import scala.language.implicitConversions
import org.scalakittens.Result.{OK, Outcome}
import org.scalakittens.{Empty, Logging, Result}

// Action results
object ResultType {
  case class Value(code: Int,
                   text: String,
                   details: Outcome) {
    override def toString = s"$text ($code)"
    def isOkay: Boolean = this == Success
    def isError: Boolean = this == Failure || this == Error
    def unary_! : Boolean = !isOkay
  }

  implicit def asBoolean(r:Value): Boolean = r.isOkay

  val Unknown      = Value(0,"N/A", Empty)
  val Success      = Value(1,"Success", OK)
  val Error        = Value(2,"Partial", Result.error("partial"))
  val LoginFailure = Value(3,"Login Error", Result.error("login failed"))
  val Failure      = Value(4,"Partial", Result.error("failed"))
  val CallAgain    = Value(5,"Partial", Result.error("call again failed"))
  val LoggedIn     = Value(6,"Logged In", OK)
  def apply(b: Boolean): Value = if (b) Success else Error

  implicit def apply(r: Result[_]): Object = r.fold(_ ⇒ Success, _ ⇒ Logging.anError(r))
  def opposite(r: Value): Value = if (r.isOkay) Error else Success
}
