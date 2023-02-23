package Graphe

import scala.annotation.tailrec

case class Noeud(nom: String)

case class Arc(extremite1: String, extremite2: String)

case class Graphe(noeuds: Set[Noeud], arcs: Set[Arc]) {

  def +(arc: Arc): Graphe = {
    val new_graph = Graphe(noeuds +Noeud(arc.extremite1) + Noeud(arc.extremite2), arcs + arc)
    new_graph
  }

  def +(autre: Graphe): Graphe = {
    val new_graph = Graphe(this.noeuds ++ autre.noeuds, this.arcs ++ autre.arcs)
    new_graph
  }

  def voisins(noeud: Noeud): Set[Noeud] = {

    // Créez un Set de noeuds contenant les voisins du noeud en question
    val voisins = arcs.flatMap(arc => {
      if (arc.extremite1 == noeud.nom) Set(Noeud(arc.extremite2))
      else if (arc.extremite2 == noeud.nom) Set(Noeud(arc.extremite1))
      else Set.empty[Noeud]
    })
    // Excluez le noeud en question du Set de voisins
    voisins.diff(Set(noeud))
  }

  def degre(noeud: Noeud): Int = {
    voisins(noeud).size
  }

  def distance(depart: Noeud, arrive: Noeud): Option[Int] = {
    /* def distanceRec(noeud1: Noeud, noeud2: Noeud, distance: Int): Option[Int] = {

       // Vérifiez si un chemin existe entre les deux noeuds
      // if (!voisins(noeud1).contains(noeud2))  return None
       // Parcourez les voisins du noeud courant
       if (noeud1 == noeud2)  return None
       // Si le noeud courant est noeud2, nous avons trouvé la distance minimale
       for (voisin <- voisins(noeud1)) {
         val distanceVoisin = distanceRec(voisin, noeud2, distance + 1)
         if (distanceVoisin.isDefined) return distanceVoisin
       }

       // Si nous arrivons ici, cela signifie que nous n'avons pas trouvé de chemin entre les deux noeuds
       None
     }
     distanceRec(depart, arrive, 1)
 */
    /* def distanceRec(noeud1 :Noeud, noeud2: Noeud, noeudNb: Int, distance: Int): Option[Int] = {
       if (voisins(noeud1).isEmpty || voisins(noeud2).isEmpty) None
       else {
         if(voisins(noeud1).contains(noeud2) || noeudNb == degre(noeud1)) Some(distance)


       }
     }*/
    /* def distanceMinimale(noeud1: Noeud, noeud2: Noeud, distance: Int = 1): Option[Int] = {
       // Vérifiez si un chemin existe entre les deux noeuds
       if (!voisins(noeud1).contains(noeud2)) return None

       // Si le noeud courant est noeud2, nous avons trouvé la distance minimale
       if (noeud1 == noeud2) return Some(distance)

       // Parcourez les voisins du noeud courant
       val distances = voisins(noeud1).map(voisin => distanceMinimale(voisin, noeud2, distance + 1))
       val distancesValides = distances.filter(_.isDefined)
       if (distancesValides.isEmpty) None else Some(distancesValides.map(_.get).min)
     }*/
    def distanceMinimale(noeud1: Noeud, noeud2: Noeud, distance: Int = 0): Option[Int] = {
      // Vérifiez si un chemin existe entre les deux noeuds
      if (voisins(noeud1).isEmpty || voisins(noeud2).isEmpty) return None

      // Si le noeud courant est noeud2, nous avons trouvé la distance minimale
      if (noeud1 == noeud2) return Some(distance)

      // Parcourez les voisins du noeud courant
      val distances = voisins(noeud1).map(voisin => distanceMinimale(voisin, noeud2, distance + 1))
      val distancesValides = distances.filter(_.isDefined)
      if (distancesValides.isEmpty) None else Some(distancesValides.map(_.get).min)
    }
    distanceMinimale(depart, arrive, 1)
  }

  lazy val composantesConnexes: Set[Set[Noeud]] = ???

  lazy val estBicoloriable: Boolean = ???
}
