package scalakittens.experiments.logic

import scala.language.implicitConversions

/**
 * Ternary intuitionistic logic
 * Created by vpatryshev on 9/26/17.
 */
sealed trait Ternary {
  def unary_¬ = this → False

  def ∨(other: Ternary): Ternary

  def ∧(other: Ternary): Ternary

  def →(other: Ternary): Ternary

  def asBoolean: Boolean

  implicit def asOption: Option[Boolean]
}

case object True extends Ternary {
  override def ∨(other: Ternary) = True

  override def ∧(other: Ternary) = other

  override def →(other: Ternary) = other

  override def asBoolean = true

  override def asOption = Some(true)
}

case object Unknown extends Ternary {
  override def ∨(other: Ternary) = other match {
    case False ⇒ this
    case _ ⇒ other
  }

  override def ∧(other: Ternary) = other match {
    case True ⇒ this
    case _ ⇒ other
  }

  override def →(other: Ternary) = other match {
    case False ⇒ False
    case _ ⇒ other
  }

  override def asBoolean = true // it is double negation, fyi
  override def asOption = None
}

case object False extends Ternary {
  override def ∨(other: Ternary) = other

  override def ∧(other: Ternary) = False

  override def →(other: Ternary) = True

  override def asBoolean = false

  override def asOption = Some(false)
}

object Ternary {
  implicit def fromOption(opt: Option[Boolean]): Ternary = opt match {
    case None ⇒ Unknown
    case Some(true) ⇒ True
    case Some(false) ⇒ False
  }
}
