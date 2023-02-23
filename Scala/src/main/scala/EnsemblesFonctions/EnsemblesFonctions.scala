// les packages object permettent de définir des méthodes/variables disponibles dans tout le package
package object EnsemblesFonctions {

  // type est l'équivalent de typedef en C
  // On représente des ensembles à l'aide de fonctions Int => Boolean
  type Ensemble = Int => Boolean

  // indique si un ensemble contient un élément
  def contient(s: Ensemble, elem: Int): Boolean = s(elem)

  // limite est utilisé pour l'affichage, ainsi que les fonctions pourTout et ilExiste
  val limite = 100

  // Renvoie un ensemble sous forme de chaîne de caractères
  // compris entre -limite et +limite
  def chaine(s: Ensemble): String = {
    val xs = for (i <- -limite to limite if contient(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  // affiche un ensemble
  def afficheEnsemble(s: Ensemble): Unit = println(chaine(s))


  /** *********************************************/
  /*        méthodes à définir ci-dessous        */
  /** *********************************************/

  // définir une méthode qui renvoie un singleton : l'ensemble qui ne contient que elem
  // i.e. la fonction qui renvoie vrai si on lui passe elem en paramètre, et faux sinon
  def singleton(elem: Int): Ensemble = {
    (x:Int) =>
      if(elem == x) true
      else false
  }

  // définir une méthode qui renvoie l'union de deux ensembles
  def union(s: Ensemble, t: Ensemble): Ensemble = {
    (x:Int) =>
      if(contient(s,x) || contient(t,x)) true
      else false
  }


  // définir une méthode renvoyant l'intersection de deux ensembles
  def intersection(s: Ensemble, t: Ensemble): Ensemble = {
    (x:Int) =>
      if(contient(s,x) && contient(t,x)) true
      else false
  }

  // définir une méthode diff qui renvoie la différence de deux ensembles (dans s, pas dans t)
  def difference(s: Ensemble, t: Ensemble): Ensemble = {
    (x:Int) =>
      if(contient(s,x) && !contient(t,x)) true
      else false
  }

  // définir une méhtode complement qui revoie le compléement d'un ensemble
  def complement(s: Ensemble): Ensemble = {
    (x:Int) =>
      if(!contient(s,x)) true
      else false
  }

  //définir une méthode filtre, qui renvoie le sous ensemble pour lequel p est vraie
  def filtrer(s: Ensemble, p: Int => Boolean): Ensemble = {
    (x:Int) =>
      if(contient(s,x) && p(x)) true
      else false
  }

  // définir une méthode pourTout, qui vérifie si p est vrai pour tout élément de s (de -limite à +limite)
  def pourTout(s: Ensemble, p: Int => Boolean): Boolean = {
    def increment(count: Int):Boolean = {
      if(count > limite) true
      else {
        if (contient(s, count) && !p(count)) false
        else increment(count + 1)
      }
    }
    increment(-limite)
  }



  // définir une méthode ilExiste qui renvoie vrai si un élément renvoie vraie pour p
  def ilExiste(s: Ensemble, p: Int => Boolean): Boolean = {
    def increment(count: Int):Boolean = {
      if(count > limite) false
      else {
        if (contient(s, count) && p(count)) true
        else increment(count + 1)
      }
    }
    increment(-limite)
  }

  // définir une fonction image qui renvoie l'ensemble image de s
  def image(s: Ensemble, f: Int => Int): Ensemble = {
    //ilExiste(s, )
    ???
  }
}
