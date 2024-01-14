package scalakittens

import scalakittens.types._

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
  with Types
