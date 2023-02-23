package NReines

class NReines(val n: Int) {

 // Question 1 :
 // étant donnée la position des reines précedentes, renvoye vrai si la colone est compatible
 def estOk(col: Int, reines: List[Int]): Boolean = {
   val nb_reines = reines.size
   def estOkRecursive(list_reines: List[Int]) : Boolean ={
     val nb_reines_rest = list_reines.size
     val diff = nb_reines +1 - nb_reines_rest
     if (list_reines.isEmpty) true
     else if (list_reines.head == col) false
     else if (list_reines.head - diff == col ||list_reines.head + diff == col) false
     else estOkRecursive(list_reines.tail)
   }
   estOkRecursive(reines)
 }

  // Question 2
  // calcule la liste des solutions
  lazy val solutions: Set[List[Int]] = {

    def solution_part(rang : Int): Set[List[Int]]={
      if (rang == 1) (0 until n).map(i => List(i)).toSet
      else for{
        solution <- solution_part(rang-1)
        reine <- 0 until n
        if estOk(reine,solution)
      }yield reine::solution

    }
    solution_part(n)
  }

  // Question 3
  // Retourne le nombre de solutions
  lazy val nombreSolutions: Int = solutions.size

  // question 4
  // transforme une solution en un String afichable
  def afficheSolution(solution: List[Int]): String = {
    solution.map(i =>{
      "O ".repeat(i) + "X" + " O".repeat(n-i-1)
    }).mkString("\n")
  }

  def afficheToutesSolutions(): Unit = for {
    (solution, i) <- solutions.zipWithIndex
  } println(s"Solution N°${i + 1} :\n " + afficheSolution(solution))



}
