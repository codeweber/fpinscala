package fpinscala.answers.monad

trait Monad[F[_]] extends Functor[F]:

    def unit[A](a: => A): F[A]

    extension [A](ma: F[A])
        def flatMap[B](f: A => F[B] ): F[B]

        def map[B](f: A => B): F[B] =
            ma.flatMap(x => unit(f(x)))

        def map2[B,C](mb: F[B])(f: (A,B) => C): F[C] =
            for
                a <- ma
                b <- mb
            yield f(a,b)


    def sequence[A](mas: List[F[A]]): F[List[A]] =
        mas.foldRight(unit(List.empty))((ma, agg) => ma.map2(agg)(_ :: _))

    def traverse[A,B](la: List[A])(f: A => F[B]): F[List[B]] =
        //sequence(la.map(f)) // a valid but inefficient implementation, requiring two passes of the list
        la.foldRight(unit(List.empty))((a, agg) => f(a).map2(agg)(_ :: _))

    def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
        /*
        // a recursive implementation
        if n <= 0 then
            unit(List.empty)
        else
            ma.map2(replicateM(n-1,ma))(_ :: _)
        */
        sequence(List.fill(n)(ma))


    def filterM[A](l: List[A])(f: A => F[Boolean]): F[List[A]] = 
        // A recursive definition
        l match
            case Nil => unit(Nil)
            case (x :: xs) => 
                for
                    b <- f(x)
                    ys <- filterM(xs)(f)
                yield if b then x :: ys else ys
        

        /*
        --Note that this can also be written using a foldRight
        l.foldRight(unit(List.empty)) {
            (a, agg) => 
                f(a).flatMap {
                    b => if b then unit(a).map2(agg)(_ :: _) else agg
                }
        }
        */
    
    def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
        (a: A) =>
            f(a).flatMap(g)


    extension [A](ma: F[A])
        def flatMapFromCompose[B](f: A => F[B]): F[B] =
            compose(_ => ma, f)(())

    def join[A](mma: F[F[A]]): F[A] = 
        mma.flatMap(x => x)


object Monad:

    given monadOption: Monad[Option] with
        def unit[A](a: => A): Option[A] =
            Some(a)

        extension [A](ma: Option[A])
            def flatMap[B](f: A => Option[B]): Option[B] =
                ma.flatMap(f)

    given monadList: Monad[List] with 
        def unit[A](a: => A): List[A] =
            List(a)

        extension [A](ml: List[A])
            def flatMap[B](f: A => List[B]): List[B] =
                ml.flatMap(f)


        