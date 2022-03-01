package fpinscala.answers.monad

case class Id[A](value: A)


object Id:

    given monadId: Monad[Id] with
        def unit[A](a: => A): Id[A] = Id(a)
        
        extension [A](id: Id[A])
            def flatMap[B](f: A => Id[B]) = f(id.value)
            