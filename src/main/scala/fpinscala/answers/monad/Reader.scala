package fpinscala.answers.monad

case class Reader[R, A](run: R => A)


object Reader:

    type readerRA = [R] =>> [A] =>> Reader[R,A]
    given readerMonad[R]: Monad[readerRA[R]] with
        def unit[A](a: => A): Reader[R,A] = Reader(r => a)
        extension [A](reader: Reader[R,A])
            def flatMap[B](f: A => Reader[R,B]): Reader[R,B] =
                Reader {
                    r => f(reader.run(r)).run(r)
                }



end Reader