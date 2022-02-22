package fpinscala.answers.parser

import scala.util.matching.Regex

type aParser[+A] = Location => Result[A]

object aParser extends Parsers[aParser]:

    import Result.{Success, Failure}


    //Exercise 9.13 
    //Implement string, regex, succeed, slice
    def string(s: String) =
        l =>
            if (l.input.startsWith(s, l.offset)) then
                Success(s, s.length)
            else
                Failure(l.toError(s"Expected $s"), false)


    def regex(r: Regex): aParser[String] = 
        l =>
            r.findPrefixOf(l.remaining) match
                case Some(m) => Success(m, m.length)
                case None    => Failure(l.toError(s"Expected patter ${r.toString}"), false)

    def succeed[A](a: A) = 
        l => Success(a, 0)


    def or[A, B>:A](x: aParser[A], y: => aParser[B]): aParser[B] =
        l => 
            x(l) match
                case Failure(e,false) => y(l)
                case r => r


    extension [A](p: aParser[A])
        def slice: aParser[String] = 
            l => 
                p(l) match
                    case Success(_, n) => Success(l.slice(n), n)
                    case e@Failure(_,_) => e


        def scope(msg: String): aParser[A] =
            l =>
                p(l).mapError(_.push(l, msg))

        def label(msg: String) =
            l =>
                p(l).mapError(_.label(msg))

        def flatMap[B](f: A => aParser[B]): aParser[B] =
            l =>
                p(l) match
                    case e@Failure(_,_) => e 
                    case Success(a,n) =>
                        f(a)(l.advanceBy(n))
                        .addCommit(n!=0)
                        .advanceSuccess(n)
                
        def attempt: aParser[A] =
            l => p(l).uncommit

        def run(input: String): Either[ParseError, A] =
            p(Location(input,0)) match
                case Success(a,_) => Right(a)
                case Failure(e,_) => Left(e)




