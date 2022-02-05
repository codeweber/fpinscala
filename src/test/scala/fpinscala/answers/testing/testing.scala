package fpinscala.answers.testing
import org.scalatest.flatspec.AnyFlatSpec

import Prop.*
import Gen.*

class GenSpecification extends AnyFlatSpec {

    "A prop for list max" should "pass" in {
        val smallInt = choose(-10,10)
        val maxProp = forAll(listOf1(smallInt)) {
            ns => 
                val maxVal =  ns.max 
                !ns.exists(_ > maxVal)
        }
        val results = runProp(maxProp)
        println(results._2)
        assert(results._1)
    }
    
    "A prop for list sorted" should "pass" in {
        val smallInt = choose(-10,10)
        val sortedProp = forAll(listOf1(smallInt)) {
            ns => 
                val nsSorted = ns.sorted
                nsSorted.head == ns.min
        }
        val results = runProp(sortedProp)
        println(results._2)
        assert(results._1)
    }
    

}
