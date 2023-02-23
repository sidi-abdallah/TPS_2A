package Logique

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class LogiqueSuite extends AnyFunSuite with Matchers {

  val e1 = Equivalent(Proposition("R"), Et(Proposition("P"), Et(Proposition("A"), Proposition("S"))))
  val e2 = Proposition("P")
  val e3 = Non(Proposition("R"))
  val th = Ou(Non(Proposition("A")), Non(Proposition("S")))

  val i1: Interpretation = Map("A" -> true, "P" -> true, "R" -> false, "S" -> false)
  val i2: Interpretation = Map("A" -> false, "P" -> false, "R" -> true, "S" -> true)
  val i3: Interpretation = Map("A" -> true, "P" -> false, "R" -> false, "S" -> true)

  test("Evaluation") {
    evaluation(e1)(i1) shouldBe true
    evaluation(e1)(i2) shouldBe false
    evaluation(e1)(i3) shouldBe true

    evaluation(e2)(i1) shouldBe true
    evaluation(e2)(i2) shouldBe false
    evaluation(e2)(i3) shouldBe false

    evaluation(e3)(i1) shouldBe true
    evaluation(e3)(i2) shouldBe false
    evaluation(e3)(i3) shouldBe true

    evaluation(th)(i1) shouldBe true
    evaluation(th)(i2) shouldBe true
    evaluation(th)(i3) shouldBe false
  }

  test("Ensemble propositions") {
    ensembleProposition(Set(e1)) shouldEqual Set("A", "P", "R", "S")
    ensembleProposition(Set(e2, e3)) shouldEqual Set("P", "R")
    ensembleProposition(Set(e2, e3, th)) shouldEqual Set("A", "P", "R", "S")
  }

  test("Liste interprétation") {
    listeInterpretation(Set("A")) shouldEqual List(Map("A" -> false), Map("A" -> true))
    listeInterpretation(Set("A", "B")) shouldEqual List(Map("A" -> false, "B" -> false), Map("A" -> false, "B" -> true), Map("A" -> true, "B" -> true), Map("A" -> true, "B" -> false))
  }

  test("Tautologie") {
    tautologie(Constante(true)) shouldBe true
    tautologie(Constante(false)) shouldBe false

    tautologie(Ou(Non(Proposition("A")), Proposition("A"))) shouldBe true
    tautologie(Implique(Proposition("A"), Proposition("A"))) shouldBe true
    tautologie(Equivalent(Proposition("A"), Proposition("A"))) shouldBe true

    tautologie(Et(Non(Proposition("A")), Proposition("A"))) shouldBe false
    tautologie(e1) shouldBe false
    tautologie(e2) shouldBe false
    tautologie(e3) shouldBe false
    tautologie(th) shouldBe false

  }

  test("Consistante") {
    consistante(Constante(true)) shouldBe true
    consistante(Constante(false)) shouldBe false

    consistante(Ou(Non(Proposition("A")), Proposition("A"))) shouldBe true
    consistante(Implique(Proposition("A"), Proposition("A"))) shouldBe true
    consistante(Equivalent(Proposition("A"), Proposition("A"))) shouldBe true

    consistante(Et(Non(Proposition("A")), Proposition("A"))) shouldBe false

    consistante(e1) shouldBe true
    consistante(e2) shouldBe true
    consistante(e3) shouldBe true
    consistante(th) shouldBe true

  }

  test("Affichage") {
    affichage(e1) shouldEqual "R <=> P et A et S"
    affichage(e2) shouldEqual "P"
    affichage(e3) shouldEqual "!R"
    affichage(th) shouldEqual "!A ou !S"

    val e = Implique(Equivalent(Ou(Proposition("A"), Constante(false)), Proposition("B")), Non(Et(Constante(true), Proposition("A"))))
    affichage(e) shouldEqual "(A ou false <=> B) => !true et A"
  }

  test("Tableau - preuve") {
    new Tableau(List(e1, e2, e3), th).preuve shouldBe true
    new Tableau(List(e1, e2), th).preuve shouldBe false
    new Tableau(List(e1, e3), th).preuve shouldBe false
  }

  test("Tableau - affichage") {

    val tableau = new Tableau(List(e1, e2, e3), th)

    tableau.toStringSelectif{ ligne =>
      ligne.valeurBC.forall(x=>x)
    } shouldEqual "+---------+-------+----+\n| A P R S | 1 2 3 | Th |\n+---------+-------+----+\n| F V F V | V V V | V  |\n| F V F F | V V V | V  |\n| V V F F | V V V | V  |\n+---------+-------+----+"

    tableau.toStringSelectif(_ => true) shouldEqual "+---------+-------+----+\n| A P R S | 1 2 3 | Th |\n+---------+-------+----+\n| F F F F | V F V | V  |\n| F F F V | V F V | V  |\n| F F V V | F F F | V  |\n| F F V F | F F F | V  |\n| F V V F | F V F | V  |\n| F V V V | F V F | V  |\n| F V F V | V V V | V  |\n| F V F F | V V V | V  |\n| V V F F | V V V | V  |\n| V V F V | F V V | F  |\n| V V V V | V V F | F  |\n| V V V F | F V F | V  |\n| V F V F | F F F | V  |\n| V F V V | F F F | F  |\n| V F F V | V F V | F  |\n| V F F F | V F V | V  |\n+---------+-------+----+"
  }

  test("Parser d'expression"){

    parseExpression("Vrai") shouldEqual Some(Constante(true))
    parseExpression("Faux") shouldEqual Some(Constante(false))
    parseExpression("A") shouldEqual Some(Proposition("A"))
    parseExpression("!A") shouldEqual Some(Non(Proposition("A")))
    parseExpression("R ou A") shouldEqual Some(Ou(Proposition("R"),Proposition("A")))
    parseExpression("!R ou A") shouldEqual Some(Ou(Non(Proposition("R")),Proposition("A")))
    parseExpression("R ou !A") shouldEqual Some(Ou(Proposition("R"),Non(Proposition("A"))))
    parseExpression("R ou !A et Q") shouldEqual Some(Ou(Proposition("R"),Et(Non(Proposition("A")),Proposition("Q"))))
    parseExpression("R et !A ou Q") shouldEqual Some(Ou(Et(Proposition("R"),Non(Proposition("A"))),Proposition("Q")))
    parseExpression("R => A") shouldEqual Some(Implique(Proposition("R"),Proposition("A")))
    parseExpression("R => !A") shouldEqual Some(Implique(Proposition("R"),Non(Proposition("A"))))
    parseExpression("!R => A") shouldEqual Some(Implique(Non(Proposition("R")),Proposition("A")))
    parseExpression("R ou A => Q") shouldEqual Some(Implique(Ou(Proposition("R"),Proposition("A")),Proposition("Q")))
    parseExpression("R et A => Q") shouldEqual Some(Implique(Et(Proposition("R"),Proposition("A")),Proposition("Q")))
    parseExpression("R => A ou Q") shouldEqual Some(Implique(Proposition("R"),Ou(Proposition("A"),Proposition("Q"))))
    parseExpression("R => A et Q") shouldEqual Some(Implique(Proposition("R"),Et(Proposition("A"),Proposition("Q"))))
    parseExpression("R => A <=> !R ou A") shouldEqual Some(Equivalent(Implique(Proposition("R"),Proposition("A")),Ou(Non(Proposition("R")),Proposition("A"))))
    parseExpression("!(R ou A) => S") shouldEqual Some(Implique(Non(Ou(Proposition("R"),Proposition("A"))),Proposition("S")))

  }


}



