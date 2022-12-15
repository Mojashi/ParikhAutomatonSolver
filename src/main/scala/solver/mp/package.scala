package xyz.mojashi
package solver
package object mp {
  def addVector[Key, Value: Numeric](l: Map[Key, Value], r: Map[Key, Value])
                                    (implicit m: Numeric[Value]): Map[Key, Value] = {
    (l.keys ++ r.keys).map(key =>
      (key , m.plus(l.getOrElse(key, m.zero), r.getOrElse(key, m.zero)))
    ).toMap
  }
  def subVector[Key, Value: Numeric](l: Map[Key, Value], r: Map[Key, Value])
                                    (implicit m: Numeric[Value]): Map[Key, Value] = {
    (l.keys ++ r.keys).map(key =>
      (key, m.minus(l.getOrElse(key, m.zero), r.getOrElse(key, m.zero)))
    ).toMap
  }
  def mulVector[Key, Value: Numeric](constant: Value, r: Map[Key, Value])
                                    (implicit m: Numeric[Value]): Map[Key, Value] = {
    r.map { case (key, value) =>
      (key, m.times(constant, value))
    }
  }

  def getCoefficients[Label, Value: Numeric]
    (expression: Expression[Label, Value]): (Map[Label, Value], Value) = {
    val m = implicitly[Numeric[Value]]
    expression match {
      case Add(left, right) =>
        val (l, lc) = getCoefficients(left)
        val (r, rc) = getCoefficients(right)
        (addVector(l,r), m.plus(lc, rc))
      case Sub(left, right) =>
        val (l, lc) = getCoefficients(left)
        val (r, rc) = getCoefficients(right)
        (subVector(l, r)  , m.min(lc, rc))
      case Times(constant, term) =>
        val (r, rc) = getCoefficients(term)
        (mulVector[Label, Value](constant.v, r), m.times(constant.v, rc))
      case Var(v) => (Map((v, m.one)), m.zero)
      case Constant(v) => (Map(), v)
    }
  }


}