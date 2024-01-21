package org.scalakittens

import scalakittens.Result


trait ReadsJson[T] {
  def parseJson(source: String): Result[T]
}
