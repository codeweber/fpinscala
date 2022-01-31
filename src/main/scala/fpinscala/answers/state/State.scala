package fpinscala.answers.state
// Answer to Exercise 6.10

case class State[S, +A](run: S => (A,S)):

    def map[B](f: A => B): State[S, B] = 
        State {
            s1 =>
                val (a, s2) = this.run(s1)
                (f(a), s2)
        }


    def flatMap[B](f: A => State[S, B]): State[S, B] = 
        State {
            s1 =>
                val (a, s2) = this.run(s1)
                f(a).run(s2)
        }



object State:

    def unit[S, A](a: A): State[S, A] =
        State( (s) => (a,s))


    def map2[S, A, B, C](sa: State[S, A], sb: State[S, B])(f: (A,B) => C ) = 
        State {
            (s1: S) =>
                val (a, s2) = sa.run(s1)
                val (b, s3) = sb.run(s2)
                (f(a, b), s3)
        }

    def sequence[S,A](ss: List[State[S, A]]): State[S, List[A]] = 
        ss.foldLeft(unit[S,List[A]](List[A]()))( (sb, sa) => map2(sa, sb)( _ :: _) )

    