package org.scalakittens

import org.scalakittens.types.{Metamorphoses, Money, Types}

/**
 * Similar to Scalaz object, it is supposed to refer to all our library traits.
 */
object Library
  extends DateAndTime
  with IO
  with Metamorphoses
  with Money
  with Ops
  with OS
  with Strings
  with parsing.HtmlContentExtractor
  with Types
