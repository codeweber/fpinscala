package fpinscala.answers.monoid

trait Foldable[F[_]]:
    def foldRight[A,B](as: F[A])(z: B)(f: (A,B) => B): B = 
        foldMap(as)( a => x => f(a,x) )(using Monoid.endoMonoid)(z)

    def foldLeft[A,B](as: F[A])(z: B)(f: (B,A) => B): B =
        foldMap(as)( a => b => f(b,a) )(using Monoid.dual(Monoid.endoMonoid) )(z)

    def foldMap[A,B](as: F[A])(f: A => B)(using mb: Monoid[B]): B =
        foldLeft(as)(mb.zero)( (b,a) => mb.op(b,f(a)))

    def concatenate[A](as: F[A])(m: Monoid[A]) =
        foldLeft(as)(m.zero)(m.op)

    def toList[A](fa: F[A]): List[A] =
        foldRight(fa)(List.empty)(_ :: _)


object Foldable:

    given foldableList: Foldable[List] with 
        override def foldRight[A,B](as: List[A])(z: B)(f: (A,B) => B): B =
            as.foldRight(z)(f)
        override def foldLeft[A,B](as: List[A])(z:B)(f: (B,A) => B): B =
            as.foldLeft(z)(f)


    given foldableOption: Foldable[Option] with
        override def foldRight[A,B](as: Option[A])(z: B)(f: (A,B) => B): B =
            as match
                case None => z 
                case Some(a) => f(a,z) 
        override def foldLeft[A,B](as: Option[A])(z: B)(f: (B,A) => B): B =
            foldRight(as)(z)( (a,b) => f(b,a) )


end Foldable