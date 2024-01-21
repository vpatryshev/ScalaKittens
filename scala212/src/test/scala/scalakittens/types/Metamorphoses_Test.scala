package scalakittens.types

import scalakittens.{DateFormat, Good, Props, TimeReader}
import scalakittens.testing.TestBase

import scala.language.reflectiveCalls
import scala.languageFeature.postfixOps
import java.util.Date

/**
 * Test for Metamorphoses
 */
class Metamorphoses_Test extends TestBase with Metamorphoses with TimeReader {
  "intOr" >> {
    "work" >> {
      intOr(555, 456) must_== 555
      intOr("123", 456) must_== 123
      intOr("[123] Patient Name", 456) must_== 456
      intOr(new Date(), 77) must_== 77
    }

  }

  "Percentages " >> {
    "Work" >> {
      val somePercentageValue="SomePercentValue"
      var pp:Props = Props(Map())
      implicit def extractor(name:String) = {
        val ex = new DataExtractor(pp,s => {
          val dateFmt = DateFormat("MM/dd/yyyy")
          val d = dateFmt.parseCurrent(s.split(" ")(0))
          d
        })
        ex(name)
      }
      pp = Props(Map(somePercentageValue->"  0.00%  "))
      val v1 = somePercentageValue %%%;
      v1 must_== Good(0.0)

      pp = Props(Map(somePercentageValue->"100.0"))
      (somePercentageValue %%%) must_== Good(1.0)

      pp = Props(Map(somePercentageValue->"59%"))
      // to string isneeded because of precision errors
      val v2 = somePercentageValue %%%;
      v2 map (_.toString) must_== Good((0.59).toString)

      // this is a bad value because there is space between the the digits and % sign
      // This is debatable.
      pp = Props(Map(somePercentageValue->"1 %"))
      val v3 = somePercentageValue %%%;
      v3 mustBeBad

      pp = Props(Map(somePercentageValue->"abc"))
      val v4 = somePercentageValue %%%;
      v4 mustBeBad

      pp = Props(Map(somePercentageValue->""))
      val v5 = somePercentageValue %%%;
      v5 mustBeBad

    }
  }
}

