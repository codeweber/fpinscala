package fpinscala.answers.testing

import Gen.Gen

case class SGen[A](forSize: Int => Gen[A]):

    def map[B](f: A => B): SGen[B] =
        SGen {
            n =>
                this.forSize(n).map(f)
        }

    def apply(size: Int): Gen[A] =
        this.forSize(size)


object SGen:

    def listOf[A](g: Gen[A]): SGen[List[A]] = 
        SGen {
            n => Gen.listOfN(n, g)
        }

    