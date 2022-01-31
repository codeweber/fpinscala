package fpinscala.answers.testing
import fpinscala.answers.state.RNG
import fpinscala.answers.state.State

import RNG.{Rand, nonNegativeLessThan}


object Gen:

    // Note that this opaque type is transparent only
    // with the GEN object.
    opaque type Gen[A] = Rand[A]

    def choose(start: Int, stopExclusive: Int): Gen[Int] = 
        nonNegativeLessThan(stopExclusive - start).map(_ + start) 

    def unit[A](a: => A): Gen[A] = State.unit[RNG, A](a)

    def boolean: Gen[Boolean] = RNG.boolean

    def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = 
        State.sequence(List.fill(n)(g))


    def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
        for
            a1 <- g1
            a2 <- g2 
            b <- RNG.boolean
        yield if b then a1 else a2 

    extension [A](self: Gen[A])
        def flatMap[B](f: A => Gen[B]): Gen[B] =
            self.flatMap(f)

        def map[B](f: A => B): Gen[B] =
            self.map(f)
    
        def listOfN(size: Gen[Int]): Gen[List[A]] =
            size.flatMap( n => Gen.listOfN(n, self) )

        def sample =
            self.run

end Gen