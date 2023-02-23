package Listes

import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

// Ces tests unitaires utilisent des vérifications de propriétés
// pour chaque forAll, les paramètres (liste, k, e, x, ...) sont générés aléatoirement
// et les tests sont rejoués plusieurs fois (au moins 20 fois)
// on se limite ici à des listes d'entiers

class ListesSuite extends AnyFunSuite with Matchers with ScalaCheckPropertyChecks {

  val smallInt: Gen[Int] = Gen.choose(-5, 20)
  val listGen: Gen[List[Int]] = Gen.listOf(smallInt)
  val booleanGen: Gen[Boolean] = Gen.oneOf(true, false)


  implicit val config: PropertyCheckConfiguration = PropertyCheckConfiguration(minSuccessful = 20)

  test("dernier") {
    forAll((listGen, "liste")) { (liste: List[Int]) =>
      if (liste.isEmpty)
        a[NoSuchElementException] should be thrownBy dernier(liste)
      else
        dernier(liste) shouldEqual liste.last

    }
  }

  test("kieme") {
    forAll((listGen, "liste"), (smallInt, "k")) { (liste: List[Int], k: Int) =>
      if (k < 0 || liste.drop(k).isEmpty)
        a[IndexOutOfBoundsException] should be thrownBy kieme(liste, k)
      else
        kieme(liste, k) shouldEqual liste(k)
    }
  }

  test("taille") {
    forAll((listGen, "liste")) { (liste: List[Int]) =>
      taille(liste) shouldEqual liste.size
    }
  }

  test("contient") {
    forAll((listGen, "liste"), (smallInt, "x")) { (liste: List[Int], x: Int) =>
      contient(liste, x) shouldEqual liste.contains(x)
    }

  }

  test("supprimerKieme") {
    forAll((listGen, "liste"), (smallInt, "k")) { (liste: List[Int], k: Int) =>
      supprimerKieme(liste, k) shouldEqual liste.patch(k, Nil, 1)
    }
  }

  test("ajouterKieme") {
    forAll((listGen, "liste"), (smallInt, "k"), (smallInt, "e")) { (liste: List[Int], k: Int, e: Int) =>
      ajouterKieme(liste, k, e) shouldEqual liste.patch(k, List(e), 0)
    }
  }

  test("identique") {
    forAll((listGen, "liste1"), (listGen, "liste2"), (booleanGen, "testIdentique")) { (liste1: List[Int], liste2: List[Int], testIdentique: Boolean) =>
      if (testIdentique)
        identique(liste1, liste1) shouldBe true
      else
        identique(liste1, liste2) shouldBe liste1 == liste2 // forte proba que ca soit false
    }
  }

  test("filtrer") {
    forAll((listGen, "liste"), (booleanGen, "testP1")) { (liste: List[Int], testP1: Boolean) =>
      lazy val p1 = (x: Int) => x > 0
      lazy val p2 = (x: Int) => x % 2 == 0
      val p = if (testP1) p1 else p2
      filtrer(liste, p) shouldEqual liste.filter(p)
    }
  }

  test("image") {
    forAll((listGen, "liste"), (booleanGen, "testP1")) { (liste: List[Int], testP1: Boolean) =>
      lazy val p1 = (x: Int) => -x
      lazy val p2 = (x: Int) => x*x
      val p = if (testP1) p1 else p2
      image(liste, p) shouldEqual liste.map(p)
    }
  }

}
