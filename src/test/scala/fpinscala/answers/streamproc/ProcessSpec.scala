package fpinscala.answers.streamproc

import org.scalatest.flatspec.AnyFlatSpec


class ProcessSpec extends AnyFlatSpec:

    "liftOne" should "process and emit first element in a stream" in {

        val streamIn = LazyList.from(1)

        val f: Int => Int = (x: Int) => 10 * x

        val proc = Process.liftOne(f)

        val xs = proc(streamIn).toList

        assert( xs == List(f(streamIn.head)) )
    }

    "lift" should "process an entire stream" in {

        val streamIn = LazyList(1,2,3,4,5)

        val f: Int => Int = (x: Int) => 10 * x

        val proc = Process.lift(f)

        val xs = proc(streamIn).toList

        assert( xs == streamIn.map(f).toList )

    }

    "filter" should "select correct elements of a stream" in {

        val pred: Int => Boolean = x => x % 2 == 0
        val proc = Process.filter(pred)

        val numToSample = 5  
        val streamIn: LazyList[Int] = LazyList.from(1)
        val xs = proc(streamIn).take(numToSample).toList
        assert( xs == streamIn.filter(pred).take(numToSample))
    }

    "sum" should "create a stream with cumulative sum" in {

        val streamIn: LazyList[Double] = LazyList(10.0, 11.0, 12.0)

        val proc = Process.sum
        val xs = proc(streamIn).toList 

        assert( xs == List(10.0, 21.0, 33.0))


    }

    "take" should "return the expected number of elements" in {

        val streamIn = LazyList.from(1)
        val numToTake = 10

        val proc = Process.take[Int](numToTake)

        val xs = proc(streamIn).toList 

        assert(xs == streamIn.take(numToTake).toList )
    }

    "drop" should "remove the expected number of elements" in {

        val streamIn = LazyList(1,2,3,4,5)
        val numToDrop = 3

        val proc = Process.drop[Int](numToDrop)

        val xs = proc(streamIn).toList 

        assert(xs == streamIn.drop(numToDrop).toList )
    }

    "count" should "count emit number of elements observed so far" in {

        val streamIn = LazyList("a", "b", "c")

        val proc = Process.count[String]

        val xs = proc(streamIn).toList

        assert(xs == (0 to streamIn.size).toList )

    }

    "mean" should "calculcate the correct running average" in {

        val streamIn = LazyList[Double](2.0, 4.0, 6.0)
        val mvAvg = List[Double](2.0, 3.0, 4.0)

        val xs = Process.mean(streamIn).toList

        assert ( xs == mvAvg )

    }

    "sumLoop" should "create a stream with cumulative sum" in {

        val streamIn: LazyList[Double] = LazyList(10.0, 11.0, 12.0)

        val proc = Process.sumLoop
        val xs = proc(streamIn).toList 

        assert( xs == List(10.0, 21.0, 33.0))


    }

    "countLoop" should "count count number of elements observed so far" in {

        val streamIn = LazyList("a", "b", "c")

        val proc = Process.countLoop[String]

        val xs = proc(streamIn).toList

        assert(xs == (0 to streamIn.size).toList )

    }

    "Pipline" should "correctly fuse filter and increment" in {

        val streamIn = LazyList.from(1).take(10)

        val predicate: Int => Boolean = _ % 2 == 0
        val f: Int => Int = _ + 1

        val proc = Process.filter[Int](predicate) |> Process.lift(f)

        val xs = proc(streamIn).toList

        assert(xs == streamIn.filter(predicate).map(f).toList )

    }