package fpinscala.answers.applicative


trait Monad[F[_]] extends Applicative[F]:

    self =>

    def join[A](ffa: F[F[A]]): F[A] = ffa.flatMap(identity)

    def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
        a => f(a).flatMap(g)

    extension [A](fa: F[A])
        override def map[B](f: A => B): F[B] =
            self.flatMap(fa)( a => unit(f(a)))

        def flatMap[B](f: A => F[B]): F[B] = join(fa.map(f))


    def map2[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C] =
        for
            a <- fa
            b <- fb
        yield f(a,b)


    def forever[A,B](a: F[A]): F[B] =
        lazy val t: F[B] = forever(a)
        a.flatMap(_ => t)

    def doWhile[A](a: F[A])(cond: A => F[Boolean]): F[Unit] = 
        for
            a1 <- a
            ok <- cond(a1)
            _  <- if (ok) doWhile(a)(cond) else unit(())
        yield ()