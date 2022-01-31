package fpinscala.answers.state

import scala.annotation.tailrec

trait RNG:
    def nextInt: (Int, RNG)



object RNG:

    type Rand[+A] = State[RNG, A]
        
        //RNG => (A, RNG)


    def nonNegativeInt(rng: RNG): (Int, RNG) =
        val (nextInt, nextRNG) = rng.nextInt
        val nextNonNegativeInt = 
            nextInt match
                case Int.MinValue => 0
                case x if x < 0 => x.abs
                case _ => nextInt
        (nextNonNegativeInt, nextRNG)

    def double(rng: RNG): (Double, RNG) = 
        val (nextNonNegativeInt, nextRNG) = nonNegativeInt(rng)
        val nextDouble = nextNonNegativeInt.toDouble / (Int.MaxValue.toDouble + 1.0)
        (nextDouble, nextRNG)
        
    def ints(count: Int)(rng: RNG): (List[Int], RNG) =
        count match
            case n if n <= 0 => (List[Int](), rng)
            case n if n  > 0 => 
                val (nextInt, nextRNG) = rng.nextInt
                val (nextInts, nextNextRNG) = ints(n-1)(nextRNG)
                (nextInt :: nextInts, nextNextRNG)

    
    def intsTailRec(count: Int)(rng: RNG): (List[Int], RNG) =

        @tailrec
        def agg(n: Int, r: RNG, xs: List[Int]): (List[Int], RNG) =
            if n <= 0 then
                (xs, r)
            else
                val (x, r1) = r.nextInt
                agg(n-1, r1, x :: xs)

        agg(count, rng, List[Int]())

    

    def unit[A](a: A): Rand[A] =
        rng => (a, rng)

    def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
        r1 =>
            val (a, r2) = s(r1)
            (f(a), r2)

    def doubleWithMap: Rand[Double] =
        map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1.0))

    
end RNG
    
