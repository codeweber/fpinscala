package fpinscala.answers.state

import scala.annotation.tailrec

trait RNG:
    def nextInt: (Int, RNG)



object RNG:

    import State.*

    type Rand[+A] = State[RNG, A]

    val int: Rand[Int] = State[RNG, Int] { _.nextInt }

    def nonNegativeInt: Rand[Int] =
        int.map( x => if x < 0 then x - Int.MinValue else x )
        

    def nonNegativeLessThan(n: Int): Rand[Int] = 
        int.flatMap {
            i =>
                val mod = i % n
                if (i - mod + (n - 1) >= 0) then 
                    unit(mod)
                else
                    nonNegativeLessThan(n)
        }

    
    def double: Rand[Double] = 
        nonNegativeInt.map( x => x.toDouble / (Int.MaxValue.toDouble) )
        
    
    def ints(n: Int): Rand[List[Int]] =
        sequence(List.fill(n)(int))

    
end RNG
    
