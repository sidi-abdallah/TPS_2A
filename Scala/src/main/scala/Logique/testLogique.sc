import Logique._

val interp: Interpretation = Map("A" -> true, "B" -> true, "C" -> false, "D" -> true)
val expr1: Expression = Ou(Proposition("A"), Proposition("C"))

val e1 = Equivalent(Proposition("R"), Et(Proposition("P"), Et(Proposition("A"), Proposition("S"))))
val e2 = Proposition("P")
val e3 = Non(Proposition("R"))
val th = Ou(Non(Proposition("A")), Non(Proposition("S")))

