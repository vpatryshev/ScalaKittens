package scalakittens

import org.specs2.mutable.Specification
import scalakittens.types.Strings
import scalakittens.types.Strings._

/**
 * Test cases for Strings class
 * TODO: fill it in
 *
 * Created by vpatryshev on 6/5/17.
 */
class StringsTest extends Specification {

  "Strings" should {

    "check common prefix" in {
      commonPrefix("a", "b") === ""
      commonPrefix("abc", "") === ""
      commonPrefix("", "xyz") === ""
      commonPrefix("x = 123", "x = 456") === "x = "

    }

//    "normalize" in {
//      val sut1 = "Buonapartes. But I warn you, if you don’t tell me that this means war,"
//
//      val buonaparte = Strings.normalize(sut1)
//
//      buonaparte must_== "buonapartes but i warn you if you don't tell me that this means war "
//
//      val sut2 = "two days’ sail from France"
//
//      val dayssailcase = Strings.normalize(sut2)
//      dayssailcase must_== "two days sail from france"
//    }
//
//    "zip and unzip" in {
//      val sut1 = "From fairest creatures we desire increase. That thereby beauty's rose might never dye."
//      val zipped1 = Strings.zip(sut1)
//      val unzipped1 = Strings.unzip(zipped1)
//      unzipped1 must_== sut1
//
//      val sut2 =
//        """
//          |724485533533931757719839503961571123795236067255655963110814479
//          |6606505059404241090310483613632359365644443458382226883278767626556
//          |1446928141177150178425517075540856576897533463569424784885970469347
//          |2573998858228382779529468346052106116983594593879188554632644092552
//          |5505820555989451890716537414896033096753020431553625034984529832320
//          |6515830476641421307088193297172341510569802627346864299218381721573
//          |3348282307345371342147505974034518437235959309064002432107734217885
//          |1492760797597634415123079586396354492269159479654614711345700145048
//          |1673375621725734645227310544829807849651269887889645697609066342044
//          |7798902191443793283001949357096392170390483327088259620130177372720
//          |2718625919914428275437422351355675134084222299889374410534305471044
//          |3686958764051781280194375308138706399427728231564252892375145654438
//          |9905278079324114482614235728619311833261065612275553181020751108533
//          |7633806031082361675045635852164214869542347187426437544428790062485
//          |8270912404220765387542644541334517485662915742999095026230097337381
//          |3772416217274772361020678685400289356608569682262014198248621698902
//          |6091309402985706001743006700868967590344734174127874255812015493663
//          |9389969058177385916540553567040928213322216314109787108145997866959
//          |9704509681841906299443656015145490488092208448003482249207730403043
//          |1884298993931352668823496621019471619107014619685231928474820344958
//          |9770955356110702758174873332729667899879847328409819076485127263100
//          |1740166787363477605857245036964434897992034489997455662402937487668
//          |8397514044516657077500605138839916688140725455446652220507242623923
//          |7921152531816251253630509317286314220040645713052758023076651833519
//          |95689139748137504926429605010013651980186945639498
//        """.stripMargin
//
//      val zipped2 = Strings.zip(sut2)
//      val unzipped2 = Strings.unzip(zipped2)
//      unzipped2 must_== sut2
//    }
  }
}
