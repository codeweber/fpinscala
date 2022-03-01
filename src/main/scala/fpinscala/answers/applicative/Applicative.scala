package fpinscala.answers.applicative

import fpinscala.answers.monad.{Functor}
import fpinscala.answers.monoid.{Monoid}

trait Applicative[F[_]] extends Functor[F]:

    self =>

    def unit[A](a: => A): F[A]
    def map2[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C]

    extension [A](fa: F[A])
        def map[B](f: A => B): F[B] =
            map2(fa, unit(()))((a, _) => f(a))

    def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] =
        as.foldRight(unit(List.empty))((a, fbs) => map2(f(a), fbs)(_ :: _))

    //Exercise 12.1
    def sequence[A](fas: List[F[A]]): F[List[A]] = 
        traverse(fas)(identity)

    def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
        sequence(List.fill(n)(fa))

    def product[A,B](fa: F[A], fb: F[B]): F[(A,B)] =
        map2(fa, fb)((_,_))


    //Exercise 12.2
    def apply[A,B](fg: F[A => B])(fa: F[A]): F[B] =
        map2(fa, fg)((a, g) => g(a))

    def map2ViaApply[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C] =
        apply(apply(unit(f.curried))(fa))(fb)

    def mapViaApply[A,B](fa: F[A])(f: A => B): F[B] =
        apply(unit(f))(fa)

    def map3[A,B,C,D](fa: F[A], fb: F[B], fc: F[C])(f: (A,B,C) => D): F[D] =
        apply(apply(apply(unit(f.curried))(fa))(fb))(fc)

    //Exercise 12.8
    def product[G[_]](g: Applicative[G]) =
        new Applicative[[X] =>> (F[X], G[X])]:
            def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), g.unit(a))
            def map2[A,B,C](fga: (F[A], G[A]), fgb: (F[B], G[B]))(f: (A,B) => C): (F[C], G[C]) =
                (self.map2(fga._1, fgb._1)(f), g.map2(fga._2, fgb._2)(f))


    //Exercise 12.9
    def compose[G[_]](g: Applicative[G]) =
        new Applicative[[X] =>> F[G[X]]]:
            def unit[A](a: => A): F[G[A]] = self.unit(g.unit(a))
            def map2[A,B,C](fga: F[G[A]], fgb: F[G[B]])(f: (A,B) => C): F[G[C]] =
                self.map2(fga, fgb)( (ga, gb) => g.map2(ga, gb)(f) )


object Applicative:

    type Id[A] = A
    given idApplicative: Applicative[Id] with 
        def unit[A](a: => A): Id[A] =
            a
        def map2[A,B,C](a: Id[A], b: Id[B])(f: (A,B)=>C): Id[C] =
            f(a,b)


    type Const = [A] =>> [B] =>> A
    given monoidApplicative[M](using m: Monoid[M]): Applicative[Const[M]] with
        def unit[A](a: => A): M = m.zero 
        def map2[A,B,C](m1: M, m2: M)(f: (A,B) => C): M = m.op(m1, m2)

end Applicative