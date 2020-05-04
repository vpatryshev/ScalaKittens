package org.scalakittens

/**
 * dealing with Json
 */
trait CanBeJson {
  def toJsonString: String
}

trait ReadsJson {
  def parseJson(source: String): Result[Props]
}
