package fpinscala.answers.effects

import fpinscala.answers.applicative.Monad
import scala.annotation.tailrec

enum Free[F[_],A]:
  case Return(a: A)
  case Suspend(s: F[A])
  case FlatMap[F[_],A,B](s: Free[F,A], f: A => Free[F,B]) extends Free[F,B]

  def map[B](f: A => B) = flatMap(x => Return(f(x)))
  def flatMap[B](f: A => Free[F,B]) = FlatMap(this, f)


object Free:
  given freeMonad[F[_]]: Monad[[X] =>> Free[F,X]] with
    def unit[A](a: => A): Free[F,A] = Return(a)
    extension [A](fa: Free[F,A])
      override def flatMap[B](f: A => Free[F,B]): Free[F,B] = fa.flatMap(f)

  @tailrec
  def runTrampoline[A](z: Free[Function0, A]): A =
    z match 
      case Return(a) => a 
      case Suspend(r) => r()
      case FlatMap(y, g) =>
        y match 
          case Return(a) => runTrampoline(g(a))
          case Suspend(r) => runTrampoline(g(r()))
          case FlatMap(x, f) => runTrampoline(x.flatMap(a => f(a).flatMap(g)))

  @tailrec
  def step[F[_],A](a: Free[F,A]): Free[F,A] =
    a match
      case FlatMap(FlatMap(x,f), g) => step(x.flatMap(k => f(k)).flatMap(g))
      case FlatMap(Return(x), f) => step(f(x))
      case _ => a

  def run[F[_], A](z: Free[F,A])(using mF: Monad[F]): F[A] = 
    step(z) match 
      case Return(a) => mF.unit(a) 
      case Suspend(r) => r
      case FlatMap(Suspend(r), g) => r.flatMap(x => run(g(x)))
      case _ => sys.error("Should never occur")


end Free