package Conway

class Conway(init: Int = 1) {

  // renvoie le rang suivant : List(1, 1) --> List(2, 1)
  def lire(rang : List[Int]) : List[Int] = {

    def imbrique_lire(s: List[Int], n: Int, current: Int): List[Int] = s match {
      case Nil => n::current::Nil
      case x::reste if x == current => imbrique_lire(reste, n+1, x)
      case x::reste => n::current::imbrique_lire(reste,1,x)
    }

    imbrique_lire(rang, 0, rang.head)
  }

  // la suite infinie de tout les rangs
  lazy val rangs : LazyList[List[Int]] = {
    def imbrique_rang(list: List[Int]): LazyList[List[Int]] = {
      list #:: imbrique_rang(lire(list))
    }
    imbrique_rang(List(init))
  }

  //renvoie le rang sous forme de chaine de caractère
  // attention : rang commence à 1
  def apply(rang: Int): String = {
    rangs(rang - 1).mkString(" ")
  }

}
