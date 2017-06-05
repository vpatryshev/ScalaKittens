package scalakittens

import org.specs2.mutable.Specification

/**
 * Test cases for Strings class
 * TODO: fill it in
 *
 * Created by vpatryshev on 6/5/17.
 */
class StringsTest extends Specification {

  "Strings" should {

    "normalize" in {
      val sut1 = "Buonapartes. But I warn you, if you don’t tell me that this means war,"

      val buonaparte = Strings.normalize(sut1)

      buonaparte must_== "buonapartes but i warn you if you don't tell me that this means war "

      val sut2 = "two days’ sail from France"

      val dayssailcase = Strings.normalize(sut2)
      dayssailcase must_== "two days sail from france"
    }
  }
}
