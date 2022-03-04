package fpinscala.answers.effects

import scala.annotation.tailrec

enum TailRec[A]:
    case Return(a: A)
    case Suspend(resume: () => A)
    case FlatMap[A,B](sub: TailRec[A], k: A => TailRec[B]) extends TailRec[B]

    def flatMap[B](f: A => TailRec[B]): TailRec[B] = FlatMap(this, f)
    def map[B](f: A => B): TailRec[B] = flatMap( a => Return(f(a)))



object TailRec:

  @tailrec
  def run[A](z: TailRec[A]): A =
    z match 
      case Return(a) => a 
      case Suspend(r) => r()
      case FlatMap(y, g) =>
        y match 
          case Return(a) => run(g(a))
          case Suspend(r) => run(g(r()))
          case FlatMap(x, f) => run(x.flatMap(a => f(a).flatMap(g)))

end TailRec
