package fpinscala.answers.localeffects


import org.scalatest.flatspec.AnyFlatSpec


class STArraySpec extends AnyFlatSpec:

    "STArray" should "be correct after creation using factory" in {
        val x = ST.runST {
            new RunnableST[List[Int]]:
                def apply[S] =
                    for
                        arr <- STArray(5, 1)
                        out <- arr.freeze 
                    yield out
        }

        assert(x == List.fill(5)(1))
    }


    "STArray" should "be correct after creation using factory method for list" in {
        val listIn = List.fill(5)(1)

        val listOut = ST.runST {
            new RunnableST[List[Int]]:
                def apply[S] =
                    for
                        arr <- STArray.fromList(listIn)
                        out <- arr.freeze 
                    yield out
        }

        assert(listIn == listOut)
    }

    "Reverse" should "reverse a list" in {
        val listIn = (1 to 5).toList

        val listOut = ST.runST {
            new RunnableST[List[Int]]:
                def apply[S] =
                    for
                        arr <- STArray.fromList(listIn)
                        _   <- arr.reverse
                        out <- arr.freeze 
                    yield out
        }

        assert(listIn.reverse == listOut)


    }

    "Quicksort" should "return a correctly sorted list" in {

        val listIn = List(3,1,4,1,5,9)

        val listOut = Utils.quicksort(listIn)

        assert(listIn.sorted == listOut)

    }