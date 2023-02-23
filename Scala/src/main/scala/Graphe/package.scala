package object Graphe {

  val graphe1 = Graphe(
    Set(Noeud("A"), Noeud("B"), Noeud("C"), Noeud("D"), Noeud("E")),
    Set(Arc("A","B"), Arc("D", "C"))
  )

  val graphe2 = Graphe(
    Set(Noeud("A"), Noeud("B"), Noeud("C"), Noeud("D"), Noeud("E"), Noeud("F"), Noeud("G")),
    Set(Arc("A", "F"), Arc("B", "D"), Arc("B", "E"), Arc("C", "G"), Arc("D", "F"), Arc("E", "G"), Arc("F", "G"))
  )

}
