package fpinscala.answers.parser


import scala.util.matching.Regex

trait Parsers[Parser[+?]]: 
    
    self => // reassign 'this' using a self-type

    // A string parser
    def string(s: String): Parser[String]

    // A character parser
    def char(c: Char): Parser[Char] =
        string(c.toString).map(_(0)) 

    // The following resembles the 'unit' type observed in previous chapters
    def succeed[A](a: A): Parser[A]

    def regex(r: Regex): Parser[String]

    def or[A, B>:A](x: Parser[A], y: => Parser[B]): Parser[B]

    extension [A](p: Parser[A])
        def run(input: String): Either[ParseError, A]
        def |[B>:A](p2: => Parser[B]): Parser[B] = self.or(p,p2)
        def map[B](f: A => B): Parser[B] =
            p.flatMap(f andThen succeed)

        def many: Parser[List[A]] = 
            // Exercise 9.3
            // Define this in terms of or, map2 and succeed
            // Note the recursive definition, without a base case
            // this requires that map2 and product are non-strict
            map2(p.many)(_ :: _) | succeed(Nil)

        def slice: Parser[String]

        def product[B](p2: => Parser[B]): Parser[(A,B)] =
            for
                a <- p
                b <- p2
            yield (a,b) 

        def **[B](p2: => Parser[B]): Parser[(A,B)] = product(p2)

        def map2[B,C](p2: => Parser[B])(f: (A,B) => C): Parser[C] =
            product(p2).map( f.tupled )

        def many1: Parser[List[A]] =
            map2(p.many)( _ :: _ )

        def listOfN(n: Int): Parser[List[A]] = 
            //Exercise 9.4 
            //Implement using map2 and succeed
            if n <= 0 then
                succeed(List())
            else 
                p.map2(p.listOfN(n-1))(_ :: _)

        def flatMap[B](f: A => Parser[B]): Parser[B]

        //9.5.1
        //Add a primitive combinator for managing the message shown in a parse error
        def label(msg: String): Parser[A]

        //9.5.2
        def scope(msg: String): Parser[A]

        //9.5.3
        def attempt: Parser[A]

    object Laws:
        import fpinscala.answers.testing.Gen.Gen
        import fpinscala.answers.testing.Prop
        import fpinscala.answers.testing.Prop.*

        def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
            Prop.forAll(in)( s => run(p1)(s) == run(p2)(s))

        def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
            equal(p, p.map(a => a))(in)

        def succeedLaw[A](V: Gen[A])(S: Gen[String]): Prop =
            Prop.forAll( V ** S ) { case v ** s => run(succeed(v))(s) == Right(v) }

    end Laws

end Parsers
