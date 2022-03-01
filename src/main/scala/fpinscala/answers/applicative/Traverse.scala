package fpinscala.answers.applicative

import fpinscala.answers.monad.{Functor}
import fpinscala.answers.monoid.{Foldable, Monoid}
import Applicative.Const
import fpinscala.answers.state.State

trait Traverse[F[_]] extends Functor[F] with Foldable[F]:

    self =>

    def traverse[G[_]:Applicative, A,B](fa: F[A])(f: A => G[B]): G[F[B]] =
        sequence(fa.map(f))

    def sequence[G[_]:Applicative,A](fga: F[G[A]]): G[F[A]] = 
        traverse(fga)(identity)

    //Exercise 12.14
    extension [A](fa: F[A])
        def map[B](f: A => B): F[B] =
            val g = Applicative.idApplicative
            traverse(fa)(x => g.unit(f(x)))

    override def foldMap[A, B:Monoid](as: F[A])(f: A => B): B =
        traverse[Const[B],A,Nothing](as)(f)

    //Section 12.7.2
    import State.{get,set, StateSA, stateMonad}
    def traverseS[S,A,B](fa: F[A])(f: A => State[S,B]): State[S,F[B]] =
        traverse[StateSA[S], A,B](fa)(f)(using stateMonad[S])

    def mapAccum[S,A,B](fa: F[A], s: S)(f: (A,S) => (B,S)): (F[B],S) =
        traverseS(fa) {
            (a: A) =>
                for
                    s1 <- get[S]
                    (b, s2) = f(a, s1)
                    _ <- set(s2)
                yield b
        }.run(s)

    def zipWithIndex[A](fa: F[A]): F[(A,Int)] =
        mapAccum(fa, 0)((a,s) => ((a,s), s+1))._1

    override def toList[A](fa: F[A]): List[A] =
        mapAccum[List[A], A, Unit](fa, Nil)( (a,s) => ((), a :: s))._2.reverse

    //Ex 12.16
    def reverse[A](fa: F[A]): F[A] = 
        val reversed = toList(fa).reverse 
        mapAccum(fa, reversed)( (a,s) => (s.head, s.tail))._1

    //Ex 12.17
    override def foldLeft[A,B](as: F[A])(z: B)(f: (B,A) => B): B =
        mapAccum(as, z)( (a, b) => (a, f(b, a)) )._2
    
    override def foldRight[A,B](as: F[A])(z: B)(f: (A,B) => B): B =
        mapAccum[B => B, A, A](as, x => x)( (a, s) => ( a, x => s(f(a, x)) ) )._2(z)


    def fuse[G[_], H[_], A, B](fa: F[A])(f: A => G[B], g: A => H[B])(using ag: Applicative[G], ah: Applicative[H]): (G[F[B]], H[F[B]]) = 
        traverse[[X] =>> (G[X], H[X]),A,B](fa)( a => (f(a), g(a)) )(using ag.product(ah))


    def compose[G[_]](using g: Traverse[G]): Traverse[ [X] =>> F[G[X]]] =
        new Traverse[[X] =>> F[G[X]]]:
            override def traverse[H[_]:Applicative, A,B](fa: F[G[A]])(f: A => H[B]): H[F[G[B]]] = 
                self.traverse(fa)(a => g.traverse(a)(x => f(x)))

object Traverse:

    //Exercise 12.13
    given listTraverse: Traverse[List] with
        override def traverse[G[_]:Applicative,A,B](fa: List[A])(f: A => G[B]): G[List[B]] =
            val g = summon[Applicative[G]]
            fa.foldRight(g.unit(List.empty))( (a,b) => g.map2(f(a), b)(_ :: _))


    given listOption: Traverse[Option] with
        override def traverse[G[_]:Applicative,A,B](fa: Option[A])(f: A => G[B]): G[Option[B]] =
            val g = summon[Applicative[G]]

            fa match 
                case None => g.unit(None)
                case Some(a) => f(a).map(Some(_))


end Traverse 
