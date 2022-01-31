import org.scalatest.flatspec.AnyFlatSpec
import fpinscala.answers.state.*

import State.*
import RNG.*

class RNGSpecification extends AnyFlatSpec {

    "nonNegativeInt" should "generate positive values" in {

        val positiveInts: Rand[List[Int]] = sequence(List.fill(100)(nonNegativeInt): List[Rand[Int]])
        assert(positiveInts.run(SimpleRNG(0))._1.forall( _ >= 0))
    }

    "nonNegativeLessThan" should "generate values in correct range" in {

        val maxExclusive = 25
        val smallIntsGen = sequence(List.fill(100)(nonNegativeLessThan(maxExclusive)))
        val smallInts = smallIntsGen.run(SimpleRNG(0))._1
        assert(smallInts.forall( x => x >= 0 && x < maxExclusive ))
    }

    "double" should "generate double values in the range of [0,1]" in {

        val numDouble = 100
        val doubleGen = sequence(List.fill(numDouble)(double))
        val doubleVals = doubleGen.run(SimpleRNG(42))._1
        assert(doubleVals.forall( x => x >= 0.0 && x <= 1.0 ))

    }

    

}
