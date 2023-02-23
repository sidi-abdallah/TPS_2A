package object Parentheses {

  // Écrire une fonction récursive qui indique si une phrase dispose de parenthèses bien construite
  def equilibre(phrase: String): Boolean = {

    def imbrique_equilibre(s: List[Char], n: Int): Boolean = s match {
      case Nil => n==0
      case x::reste if x != ')' && x!= '(' => imbrique_equilibre(reste, n)
      case _ if n < 0 => false
      case '(':: reste => imbrique_equilibre(reste, n+1)
      case ')':: reste => imbrique_equilibre(reste, n-1)
    }
    imbrique_equilibre(phrase.toList, 0)

  }

  // pareil, mais générique
  def equilibreGenerique(co: Char, cf: Char)(phrase: String): Boolean = {
    def imbrique_equilibre(s: List[Char], n: Int): Boolean = s match {
      case Nil => n==0
      case x::reste if x != cf && x!= co => imbrique_equilibre(reste, n)
      case _ if n < 0 => false
      case x::reste if x == co => imbrique_equilibre(reste, n+1)
      case x::reste if x == cf => imbrique_equilibre(reste, n-1)
    }
    imbrique_equilibre(phrase.toList, 0)
  }

  // utiliser la fonction générique pour définir la version xml avec < et > comme caractère ouvrant/fermant
  lazy val equilibreXml: String => Boolean = {
    s: String => equilibreGenerique('<', '>')(s);
  }

}
