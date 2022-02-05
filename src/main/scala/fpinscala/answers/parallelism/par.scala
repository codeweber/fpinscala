package fpinscala.answers.parallelism

import java.util.concurrent.{ExecutorService, Callable, Future, TimeUnit}


object Par:

    // Use an opaque type. 
    opaque type Par[A] = ExecutorService => Future[A]

    def unit[A](a: A): Par[A] = 
        (es: ExecutorService) => UnitFuture(a)

    private case class UnitFuture[A](get: A) extends Future[A]:
        def isDone = true
        def get(timeout: Long, units: TimeUnit) = get
        def isCancelled = false
        def cancel(evenIfRunning: Boolean): Boolean = false


    def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = 
        (es: ExecutorService) =>
            val af = a(es)
            val bf = b(es)
            UnitFuture(f(af.get, bf.get))


    def fork[A](a: => Par[A]): Par[A] = 
        es => es.submit(
            new Callable[A]:
                def call = a(es).get  
        )

    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))


    def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)


    //Exercise 7.4
    def asyncF[A,B](f: A => B): A => Par[B] =
        a => lazyUnit(f(a))


    def map[A,B](pa: Par[A])(f: A => B): Par[B] =
        map2(pa, unit(()))( (a,_) => f(a))
    
    //Exercise 7.5
    //Note that the following uses foldRight. It's a simple implementation that is not 
    def sequence[A](ps: List[Par[A]]): Par[List[A]] =
        ps.foldRight(unit(Nil))( (pa, pb) => map2(pa,pb)( (a,b) => a :: b ))


    def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
        val fbs: List[Par[B]] = ps.map(asyncF(f))
        sequence(fbs)
    }

    //Exercise 7.6
    def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = 
        val filteredList = parMap(as)( (a) => if f(a) then Some(a) else None )
        map(filteredList)(_.flatten)


    // Exercise 7.13
    def flatMap[A,B](a: Par[A])(f: A => Par[B]): Par[B] =
        es =>
            val x = run(es)(a).get
            f(x)(es)

    def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
        flatMap(cond)(if _ then t else f)

    def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
        flatMap(n)(choices(_))
    
    def join[A](a: Par[Par[A]]): Par[A] =
        es => 
            val pa = run(es)(a).get
            run(es)(pa)


    def joinFromFlatMap[A](a: Par[Par[A]]): Par[A] =
        flatMap(a)( (x) => ( es => run(es)(x)) )

    def flatMapFromJoin[A,B](a: Par[A])(f: A => Par[B]): Par[B] =
        join( map(a)(f) )


    def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] =
        map2(p,p2)(_ == _)




