package NReines

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class NReinesSuite extends AnyFunSuite with Matchers {

  val r2 = new NReines(2)
  val r4 = new NReines(4)
  val r8 = new NReines(8)

  test("estOk") {
    r4.estOk(3, List(0, 2)) shouldBe true
    r4.estOk(0, List(0, 2)) shouldBe false
    r4.estOk(1, List(0, 2)) shouldBe false
    r4.estOk(0, List(1)) shouldBe false
    r4.estOk(1, List(1)) shouldBe false
    r4.estOk(2, List(1)) shouldBe false
    r4.estOk(3, List(1)) shouldBe true
  }

  test("solutions") {
    r2.solutions shouldEqual Set()
    r4.solutions shouldEqual Set(List(2, 0, 3, 1), List(1, 3, 0, 2))
    Set(List(3, 1, 6, 2, 5, 7, 4, 0), List(4, 0, 7, 5, 2, 6, 1, 3), List(2, 7, 3, 6, 0, 5, 1, 4)).subsetOf(r8.solutions) shouldBe true
  }

  test("Nombre de solution(s)") {
    r2.nombreSolutions shouldBe 0
    r4.nombreSolutions shouldBe 2
    r8.nombreSolutions shouldBe 92
  }

  test("Affichage") {
    r4.afficheSolution(List(1, 3, 0, 2)) shouldBe "O X O O\nO O O X\nX O O O\nO O X O"
    r8.afficheSolution(List(2, 0, 6, 4, 7, 1, 3, 5)) shouldBe "O O X O O O O O\nX O O O O O O O\nO O O O O O X O\nO O O O X O O O\nO O O O O O O X\nO X O O O O O O\nO O O X O O O O\nO O O O O X O O"
  }


}
