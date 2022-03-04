package fpinscala.answers.localeffects


import org.scalatest.flatspec.AnyFlatSpec

class STSpec extends AnyFlatSpec:

    "STRef" should "not be instantiable" in {
        //val x = new STRef[Nothing, Int] { var cell = 0 }
        //This will not compile, since STRef is a sealed trait
        assert (true)
    }

    "RunnableST" should "return expected result when executed" in {
        val p = 
            new RunnableST[(Int, Int)]:
                def apply[S] =
                    for
                        r1 <- STRef(1)
                        r2 <- STRef(2)
                        x1 <- r1.read 
                        x2 <- r2.read 
                        _  <- r1.write(x2+1)
                        _  <- r2.write(x1+1)
                        a1 <- r1.read
                        a2 <- r2.read 
                    yield (a1, a2)


        assert(ST.runST(p) == (3,2))

    }