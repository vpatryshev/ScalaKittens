package scalakittens.scalaz

import scalaz._
import Scalaz._
// must be 'having' ... 'or' ...

class ValidationPlus[X,Y](base: Validation[X, Y]) {

  def having(p: Y => Boolean) = new {
    def orElse(bad: => X) = base match {
      case sucx@Success(x) if p(x) => sucx
      case Success(_) => Failure(bad)
      case f@Failure(_) => f
    }
  }
}

object ValidationPlus {

  implicit def pimpValidation[X,Y](x:Validation[X, Y]) = new ValidationPlus(x)
  implicit def pimpValidation[X,Y](x:Success[X, Y])    = new ValidationPlus(x)
  implicit def pimpValidation[X,Y](x:Failure[X, Y])    = new ValidationPlus(x)

  implicit def fromEither[L,R](either: Either[L,R]) = either match {
    case Left(x)  => Failure(x)
    case Right(y) => Success(y)
  }

  def notNull[Bad, Good](value: Good) = Success[Bad,Good](value) having (null !=)

  implicit def fromOption[R](opt: Option[R]) = opt match {
    case None  => Failure(())
    case Some(x) => Success(x)
  }
}
// please ignore this, it'll go to another project
//class ValidationPlus[Bad,Good](val root: Validation[Bad,Good]) {
//  def orElse(another: Validation[Bad, Good]) = root match {
//    case Success(_) => root
//    case Failure(_) => another
//  }
//
//  def getOrElse(alt: Good) = root match {
//    case Success(x) => x
//    case Failure(_) => alt
//  }
//}
//
//object ValidationPlus {
//  def success[Bad,Good](good:Good) = Success(good)
//  def failure[Bad,Good](bad:Bad) = Failure(bad)
//
//  implicit def pimp[Bad,Good](v: Validation[Bad,Good]) = new ValidationPlus[Bad,Good](v)
//  implicit def unpimp[Bad,Good](vp: ValidationPlus[Bad,Good]) = vp.root
//
//  def fromTryCatch[T](a: => T): ValidationPlus[Throwable,T] = try {
//    success(a)
//  } catch {
//    case e: Throwable => failure(e)
//  }
//}
//
