package Parentheses

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ParenthesesSuite extends AnyFunSuite with Matchers {

  val xml_ok = "<note><to>Tove</to><from>Jani</from><heading>Reminder</heading><body>Don't forget me this weekend!</body></note>"
  val xml_ko = "<note><to< Tove</to><from>Jani</from><heading>Reminder</heading><body>Don't forget me this weekend!</body></note>"

  test("equilibre du mot vide") {
    equilibre("") shouldBe true
  }

  test("equilibre ok") {
    equilibre("1*(2+3)") shouldBe true
  }

  test("equilibre imbriquée ok") {
    equilibre("(if (zero? x) max (/ 1 x))") shouldBe true
    equilibre("Je lui ai dit (que ce n'était pas (encore) fini).\n(Mais il n'écoutait pas...)") shouldBe true
  }

  test("equilibre KO - nombre") {
    equilibre(":-)") shouldBe false
  }

  test("equlibre KO - ordre") {
    equilibre("())(") shouldBe false
    equilibre("())()(") shouldBe false
    equilibre("(()()))(") shouldBe false
  }


  test("equlibre generique ok") {
    equilibreGenerique('(', ')')("") should ===(equilibre(""))
    equilibreGenerique('(', ')')("1*(2+3)") should ===(equilibre("1*(2+3)"))
    equilibreGenerique('(', ')')("(if (zero? x) max (/ 1 x))") should ===(equilibre("(if (zero? x) max (/ 1 x))"))
  }

  test("equlibre generique KO") {
    equilibreGenerique('(', ')')(":-)") should ===(equilibre(":-)"))
    equilibreGenerique('(', ')')("())()(") should ===(equilibre("())()("))
    equilibreGenerique('(', ')')("(()()))(") should ===(equilibre("(()()))("))
  }


  test("equlibreXml ok") {
    equilibreXml(xml_ok) shouldBe true
  }

  test("equlibreXml ko") {
    equilibreXml(xml_ko) shouldBe false
  }


}
