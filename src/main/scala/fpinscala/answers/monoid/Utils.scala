package fpinscala.answers.monoid

object Utils:

    def concatenate[A](as: List[A])(using m: Monoid[A]): A =
        as.foldLeft(m.zero)(m.op)

    def foldMap[A,B](as: List[A])(f: A => B)(using m: Monoid[B]): B =
        concatenate(as.map(f))

    def foldMap[A,B](v: IndexedSeq[A])(f: A => B)(using m: Monoid[B]): B =

        val len = v.length
        if len >= 2 then
            val (h, t) = v.splitAt(len/2) 
            m.op(foldMap(h)(f), foldMap(t)(f))
        else if len == 1 then 
            f(v(0))
        else
            m.zero
            

    /*
    // First attempt, prior to looking at hint
    def isOrdered(v: IndexedSeq[Int]): Boolean =

        val len = v.length 
        if len > 0 then 
            (v(0) == foldMap(v)(identity)(using Monoid.intMin)) && isOrdered(v.drop(1))
        else true
    */

    // Second attempt
    def isOrdered(v: IndexedSeq[Int]): Boolean =

        val x = foldMap(v)(a => Monoid.orderedInterval(a,a,true))(using Monoid.monoidOrderedInterval)
        x.isOrdered

    def countWords(s: String): Int =

        val m = WC.wcMonoid
        import WC.*
        
        def foo(inStr: String): WC =
            val l = inStr.length

            if l > 2 then
                val (h,t) = inStr.splitAt(l/2)
                m.op(foo(h), foo(t))
            else if !inStr.contains(" ") then
                Stub(inStr)
            else
                val (h,t) = inStr.splitAt(1)
                Part(h.trim, 0, t.trim)
                
        val wc = foo(s)
        wc match
            case Stub(x) => if x.nonEmpty then 1 else 0
            case Part(h,n,t) => n+(if h.nonEmpty then 1 else 0) + (if t.nonEmpty then 1 else 0)



    def countWordsWithFold(s: String): Int =

        import WC.*
        def convert(c: Char): WC =
            if c.isWhitespace then
                Part("", 0, "")
            else
                Stub(c.toString)

        def countNonEmpty(x: String): Int = x.length.min(1)

        foldMap(s.toIndexedSeq)(convert)(using WC.wcMonoid) match
            case Stub(x) => countNonEmpty(x) 
            case Part(x,n,y) => countNonEmpty(x) + n + countNonEmpty(y)


    //Exercise 10.18
    def bag[A](as: IndexedSeq[A]): Map[A, Int] =

        import Monoid.intAddition
        foldMap(as)(x => Map(x->1))(using Monoid.mapMergeMonoid)

    
