package object Logique {

  // une interprétation associe une valeur de vérité à une proposition
  // Par exemple, on pourrait avoir l'interprétation ssuivante :
  // Map ( "A" -> true , "B" -> true, "C" -> false, "D" -> true)
  type Interpretation = Map[String, Boolean]

  // une base de connaissance est une liste d'expressions
  type BaseConnaissance = List[Expression]

  // un théorème est une expression
  type Theoreme = Expression

  // une expression logique se compose de constantes, de propositions,
  // et des opérateurs et, ou, non, ainsi que l'implication et l'équivalence
  sealed trait Expression

  sealed trait OperateurBinaire extends Expression {
    val expGauche: Expression
    val expDroite: Expression
  }

  case class Constante(valeur: Boolean) extends Expression

  case class Proposition(nom: String) extends Expression

  case class Non(exp: Expression) extends Expression

  case class Ou(expGauche: Expression, expDroite: Expression) extends OperateurBinaire

  case class Et(expGauche: Expression, expDroite: Expression) extends OperateurBinaire

  case class Implique(expGauche: Expression, expDroite: Expression) extends OperateurBinaire // (vrai => faux) est faux, tout le reste est vrai

  case class Equivalent(expGauche: Expression, expDroite: Expression) extends OperateurBinaire

  // Question 1 :
  // calculer la valeur de vérite d'une expression, étant donnée une interprétation
  def evaluation(expr: Expression)(implicit interp: Interpretation): Boolean = expr match {
      case Constante(valeur) => valeur
      case Proposition(nom) => interp(nom)
      case Non(exp) => !evaluation(exp)
      case Ou(expGauche, expDroite) => evaluation(expGauche) || evaluation(expDroite)
      case Et(expGauche, expDroite) => evaluation(expGauche) && evaluation(expDroite)
      case Implique(expGauche, expDroite) => !evaluation(expGauche) || evaluation(expDroite)
      case Equivalent(expGauche, expDroite) => evaluation(expGauche) == evaluation(expDroite)
  }


  // Question 2 :
  // renvoyer l'ensemble des noms des propositions comprises dans
  // un ensemble d'expression
  def ensembleProposition(expressions: Set[Expression]): Set[String] = {
    def propositions(expr: Expression): Set[String] = {
      expr match {
        case Proposition(nom) => Set(nom)
        case Non(exp) => propositions(exp)
        case Ou(expGauche, expDroite) => propositions(expGauche) ++ propositions(expDroite)
        case Et(expGauche, expDroite) => propositions(expGauche) ++ propositions(expDroite)
        case Implique(expGauche, expDroite) => propositions(expGauche) ++ propositions(expDroite)
        case Equivalent(expGauche, expDroite) => propositions(expGauche) ++ propositions(expDroite)
        case _ => Set()
      }
    }
    expressions.flatMap(propositions)
  }


  //Question 3 :
  // renvoyer la liste de toutes les interprétations possibles étant donnée
  // l'ensemble des propositions possibles dans l'ordre du code de Gray
  // exemple pour 3 propositions : FFF, FFV, FVV, FVF, VVF, VVV, VFV, VFF
  def listeInterpretation(e: Set[String]): List[Interpretation] = {
   val n = e.size
    val grayCode = (0 until (1 << n)).map(i => (i ^ (i >> 1)).toBinaryString.reverse.padTo(n, '0').reverse)
    val sortedPropositions = e.toList.sorted
    grayCode.map(code => sortedPropositions.zip(code.map(_ == '1')).toMap).toList
  }

  // question 4 :
  // définir si une expression est une tautologie, et si elle est consistante

  // tautologie = vrai pour tout interprétation
  def tautologie(expr: Expression): Boolean = {
    listeInterpretation(ensembleProposition(Set(expr))).forall(interp => evaluation(expr)(interp))
  }

  // consistante = au moins un modèle
  def consistante(expr: Expression): Boolean = {
    listeInterpretation(ensembleProposition(Set(expr))).exists(interp => evaluation(expr)(interp))
  }




  // Question 5 :
  // définir la fonction permettant d'afficher une expression
  def affichage(expr: Expression): String = {
    def parenthèses(exp: Expression) = exp match {
      case _: OperateurBinaire => s"${affichage(exp)}"
      case _ => affichage(exp)
    }

    expr match {
      case Constante(valeur) => valeur.toString
      case Proposition(nom) => nom
      case Non(exp) => s"!${parenthèses(exp)}"
      case Ou(expGauche, expDroite) => s"${parenthèses(expGauche)} ou ${parenthèses(expDroite)}"
      case Et(expGauche, expDroite) => s"${parenthèses(expGauche)} et ${parenthèses(expDroite)}"
      case Implique(expGauche, expDroite) => s"${parenthèses(expGauche)} => ${parenthèses(expDroite)}"
      case Equivalent(expGauche, expDroite) => s"${parenthèses(expGauche)} <=> ${parenthèses(expDroite)}"
    }
  }



  // Le tableau de vérité
  class Tableau(val BC: BaseConnaissance, val Th: Theoreme) {

  override def toString: String =
    "Base de connaissances :\n" +
      BC.zipWithIndex.map { case (expr, index) => s"(${index + 1}) " + affichage(expr) }.mkString("\n") +
      "\nThéorème :\n" + affichage(Th)

    // un ligne du tableau de vérité
    class Ligne(val valeurInter: List[Boolean], val valeurBC: List[Boolean], val valeurTh: Boolean)

    // Question 6 :
    // définir les propositions ainsi que toutes les lignes du tableau de vérité
    val propositions: Set[String] = ensembleProposition(BC.toSet + Th)

    val lignes: List[Ligne] = for {
      interp <- listeInterpretation(propositions)
    } yield new Ligne(
      interp.values.toList,
      BC.map(evaluation(_)(interp)),
      evaluation(Th)(interp)
    )
    //Question 7 :
    // Définir la fonction qui renvoie vrai si la base de connaissance infère le théorème
    def preuve: Boolean = lignes.forall(_.valeurTh)


    //Question 8 :
    // définir une méthode qui affiche tout ou parti du tableau de vérité
    def toStringSelectif(f: Ligne => Boolean): String = {
     val lignesSélectionnées = lignes.filter(f)
      if (lignesSélectionnées.isEmpty) "Aucune ligne ne correspond à ce critère"
      else {
        val titres = propositions.toList.sorted.mkString(" ")
        val enTête = s"$titres | BC | Th"
        val lignesFormattées = for {
          ligne <- lignesSélectionnées
        } yield ligne.valeurInter.mkString(" ") + " | " + ligne.valeurBC.mkString(" ") + " | " + ligne.valeurTh
        (enTête :: lignesFormattées).mkString("\n")
      }

    }

  }


  // Question 9
  // à partir d'une chaine de caractère, renvoyé, si possible, l'expression correspondante :
  def parseExpression(expr: String): Option[Expression] = ???

}







