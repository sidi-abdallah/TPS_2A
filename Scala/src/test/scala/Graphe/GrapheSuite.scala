package Graphe

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class GrapheSuite extends AnyFunSuite with Matchers {

  test("Ajout d'arcs") {
    val graphe3 = graphe1 + Arc("E", "F")

    graphe3.arcs should contain(Arc("E", "F"))
    graphe3.noeuds should contain(Noeud("F"))

    graphe1.arcs.subsetOf(graphe3.arcs) shouldBe true
    graphe1.noeuds.subsetOf(graphe3.noeuds) shouldBe true
  }

  test("Union de graphe") {
    val graphe3 = graphe1 + graphe2

    graphe1.arcs.subsetOf(graphe3.arcs) shouldBe true
    graphe2.arcs.subsetOf(graphe3.arcs) shouldBe true

    graphe1.noeuds.subsetOf(graphe3.noeuds) shouldBe true
    graphe2.noeuds.subsetOf(graphe3.noeuds) shouldBe true
  }

  test("Voisins") {
    graphe1.voisins(Noeud("A")) shouldEqual Set(Noeud("B"))
    graphe1.voisins(Noeud("B")) shouldEqual Set(Noeud("A"))
    graphe1.voisins(Noeud("E")) shouldEqual Set()

    graphe2.voisins(Noeud("A")) shouldEqual Set(Noeud("F"))
    graphe2.voisins(Noeud("B")) shouldEqual Set(Noeud("D"), Noeud("E"))
  }


  test("Degre") {
    graphe1.degre(Noeud("A")) shouldBe 1
    graphe1.degre(Noeud("B")) shouldBe 1
    graphe1.degre(Noeud("E")) shouldBe 0

    graphe2.degre(Noeud("A")) shouldBe 1
    graphe2.degre(Noeud("B")) shouldBe 2
  }

  test("Distance"){
    graphe1.distance(Noeud("A"), Noeud("B")) shouldBe Some(1)
    graphe1.distance(Noeud("C"), Noeud("D")) shouldBe Some(1)
    graphe1.distance(Noeud("A"), Noeud("E")) shouldBe empty

    graphe2.distance(Noeud("A"), Noeud("B")) shouldBe Some(3)
    graphe2.distance(Noeud("A"), Noeud("C")) shouldBe Some(3)
    graphe2.distance(Noeud("A"), Noeud("D")) shouldBe Some(2)
    graphe2.distance(Noeud("A"), Noeud("E")) shouldBe Some(3)
    graphe2.distance(Noeud("A"), Noeud("F")) shouldBe Some(1)
    graphe2.distance(Noeud("A"), Noeud("G")) shouldBe Some(2)

  }

  test("Composantes connexes") {
    graphe1.composantesConnexes shouldEqual Set(Set(Noeud("A"), Noeud("B")), Set(Noeud("C"), Noeud("D")), Set(Noeud("E")))
    graphe2.composantesConnexes shouldEqual Set(Set(Noeud("A"), Noeud("B"), Noeud("C"), Noeud("D"), Noeud("E"), Noeud("F"), Noeud("G")))
  }

  test("BiColoration") {
    graphe1.estBicoloriable shouldBe true
    graphe2.estBicoloriable shouldBe false

  }

}
