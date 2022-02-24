package fpinscala.answers.monoid

import org.scalatest.flatspec.AnyFlatSpec
import fpinscala.answers.testing.Prop
import fpinscala.answers.testing.Gen.*


def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
    val identity = Prop.forAll(gen) {
        a => m.op(a, m.zero) == a
    }

    val genTriple = (gen ** gen ** gen).map(x => (x._1._1, x._1._2, x._2))

    val associativity = Prop.forAll(genTriple) {
        (a1,a2,a3) => m.op(a1, m.op(a2,a3)) == m.op(m.op(a1, a2),a3)
    } 

    identity && associativity


class monoidSpec extends AnyFlatSpec {

    "A prop for monoid int" should "pass" in {
        val smallInt = choose(-100,100)
        val monoidIntProp = monoidLaws(Monoid.intAddition, smallInt) 
        val results = Prop.runProp(monoidIntProp)
        println(results._2)
        assert(results._1)
    }

    "A prop for monoid list" should "pass" in {
        val smallInt = choose(-256,256)
        val genList = listOf(smallInt)
        val monoidProp = monoidLaws(Monoid.listMonoid, genList) 
        val results = Prop.runProp(monoidProp)
        println(results._2)
        assert(results._1)
    }

    "foldMap for indexed Seq containing integers with the addition monoid" should "give the same as summing" in {
        val x: IndexedSeq[Int] = (0 to 100).toIndexedSeq
        assert(x.sum == Utils.foldMap(x)(identity)(using Monoid.intAddition))

    }

    "foldMap applied to IndexedSeqs with integers with the addition monoid" should "give the same as summing" in {
        val smallInt = choose(-256,256)
        val idxSeqOfInt = listOf(smallInt).map(_.toIndexedSeq)
        val idxSeqProp = Prop.forAll(idxSeqOfInt) {
            x => x.sum == Utils.foldMap(x)(identity)(using Monoid.intAddition)
        }
        val results = Prop.runProp(idxSeqProp)
        assert(results._1)

    }

    "isOrdered" should "return true for an ordered sequence" in {
        val x = (0 to 100).toIndexedSeq
        assert(Utils.isOrdered(x))
    }
    
    "isOrdered" should "return false for an unordered sequence" in {
        val x = IndexedSeq(10,5)
        assert(!Utils.isOrdered(x))
    }
    

    "countWords" should "correct count the number of words in a short sentence" in {
        val x = "A quick brown fox"
        val output = Utils.countWords(x)
        println(s"the number of words found is: $output")

        assert(output == 4)
    }

    "countWords" should "correct count the number of words in a string containing only whitespace" in {
        val x = "   "
        val output = Utils.countWords(x)
        println(s"the number of words found is: $output")

        assert(output == 0)
    }

    "countWords" should "correct count the number of words in an empty string" in {
        val x = ""
        val output = Utils.countWords(x)
        println(s"the number of words found is: $output")

        assert(output == 0)
    }

    "countWordsWithFold" should "correct count the number of words in a short sentence" in {
        val x = "A quick brown fox"
        val output = Utils.countWordsWithFold(x)
        println(s"the number of words found is: $output")

        assert(output == 4)
    }

    "countWordsWithFold" should "correct count the number of words in a string containing only whitespace" in {
        val x = "   "
        val output = Utils.countWordsWithFold(x)
        println(s"the number of words found is: $output")

        assert(output == 0)
    }

    "countWordsWithFold" should "correct count the number of words in an empty string" in {
        val x = ""
        val output = Utils.countWordsWithFold(x)
        println(s"the number of words found is: $output")

        assert(output == 0)
    }

    "bag" should "return the correct result" in {
        val x = "a rose is a rose".split(" ").toIndexedSeq
        val expected: Map[String, Int] = Map("a" -> 2, "rose" -> 2, "is" -> 1)
        assert(expected == Utils.bag(x))

    }

}