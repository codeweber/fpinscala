package fpinscala.answers.localeffects

object Utils:

    def partition[S](arr: STArray[S,Int], n: Int, r: Int, pivot: Int): ST[S, Int] = 

        val swp1 = 
            for 
                _  <- arr.swap(pivot, r)
                j <- STRef(n)
            yield j

        val nxt = (n until r).foldLeft(swp1){
            (st, i) =>
                for
                    stRefJ <- st
                    j <- stRefJ.read
                    pvt <- arr.read(r)
                    x <- arr.read(i)
                    _ <- if (x < pvt) then arr.swap(i,j).flatMap(_ => stRefJ.write(j+1)) else ST(())
                yield stRefJ  
        }

        for 
            stRefJ <- nxt
            j <- stRefJ.read
            _ <- arr.swap(j, r)
        yield j


    def qs[S](a: STArray[S, Int], n: Int, r: Int): ST[S, Unit] = 
        if (n < r) then
            for
                pivot <- partition(a, n, r, n + (r-n)/2)
                _     <- qs(a, n, pivot-1)
                _     <- qs(a, pivot+1, r)
            yield ()
        else ST(())


    def quicksort(xs: List[Int]): List[Int] =
        if xs.length < 2 then 
            xs 
        else 
            ST.runST(new RunnableST[List[Int]] {
                def apply[S] = 
                    for
                        arr <- STArray.fromList(xs)
                        size <- arr.size 
                        _ <- qs(arr, 0, size-1)
                        sorted <- arr.freeze 
                    yield sorted
            })
