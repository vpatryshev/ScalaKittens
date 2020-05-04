package org.scalakittens.types

trait Person {
  def name: String
  
  def sameNameAs(alternativeName: String): Boolean
}
