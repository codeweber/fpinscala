package fpinscala.answers.monoid

trait Monoid[A]:
    def op(a1: A, a2: A): A
    def zero: A


object Monoid: 
    given stringMonoid: Monoid[String] with
        def op(s1: String, s2: String): String = s1 ++ s2
        def zero: String = ""

    given listMonoid[A]: Monoid[List[A]] with
        def op(l1: List[A], l2: List[A]) = l1 ++ l2
        def zero: List[A] = Nil


    given optionMonoid[A]: Monoid[Option[A]] with
        def op(o1: Option[A], o2: Option[A]) = o1.orElse(o2)
        def zero = None 


    given endoMonoid[A]: Monoid[A => A] with
        def op(e1: (A => A), e2: (A => A)): (A => A) = e1.compose(e2)
        def zero = (a: A) => a: A


    given intAddition: Monoid[Int] with
        def op(i1: Int, i2: Int): Int = i1 + i2
        def zero = 0

    given booleanOr: Monoid[Boolean] with
        def op(b1: Boolean, b2: Boolean): Boolean = b1 || b2
        def zero: Boolean = false


    given intMultiplication: Monoid[Int] with
        def op(i1: Int, i2: Int): Int = i1 * i2
        def zero = 1

    given booleanAnd: Monoid[Boolean] with
        def op(b1: Boolean, b2: Boolean): Boolean = b1 && b2
        def zero: Boolean = true


    given intMin: Monoid[Int] with
        def op(i1: Int, i2: Int): Int = i1 min i2
        def zero : Int = Int.MaxValue


    case class orderedInterval(lb: Int, ub: Int, isOrdered: Boolean)

    given monoidOrderedInterval: Monoid[orderedInterval] with
        def op(a1: orderedInterval, a2: orderedInterval): orderedInterval =
            orderedInterval(a1.lb min a2.lb, a1.ub max a2.ub, a1.isOrdered && a2.isOrdered && a1.ub <= a2.lb)
        def zero: orderedInterval = orderedInterval(Int.MaxValue, Int.MinValue, true)


    def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] =
        new Monoid[A=>B]:
            def zero: A => B = a => B.zero
            def op(a1: A=>B, a2: A=>B): A=>B =
                a => B.op(a1(a), a2(a))

    given mapMergeMonoid[K,V](using V: Monoid[V]): Monoid[Map[K,V]] with
        def zero = Map[K,V]()
        def op(m1: Map[K,V], m2: Map[K,V]): Map[K,V] = 
            (m1.keySet ++ m2.keySet).foldLeft(zero) {
                (acc,k) =>
                    acc.updated(k, V.op(m1.getOrElse(k, V.zero), m2.getOrElse(k,V.zero)))
            }

end Monoid
