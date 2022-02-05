package fpinscala.answers.testing

import fpinscala.answers.state.RNG
import fpinscala.answers.state.SimpleRNG
import fpinscala.answers.parallelism.Par
import java.util.concurrent.Executors

type TestCases = Int 
type FailedCase = String
type SuccessCount = Int 
type PropLabel = String
type MaxSize = Int

enum Result:

        def isFalsified: Boolean =
            this match
                case Falsified(_, _) => true
                case _ => false

        case Falsified(failure: FailedCase, successess: SuccessCount)
        case Passed
        case Proved

end Result

case class Prop(run: (MaxSize, TestCases, RNG) => Result):

    import Result.*

    def &&(that: Prop): Prop =
        Prop {
            (maxSize, n, rng) =>
                val resultThis = this.run(maxSize, n, rng)
                resultThis match
                    case Falsified(f, s) => Falsified("Left Prop failed\n" + f, s)
                    case _ => 
                        val resultsThat = that.run(maxSize, n, rng)
                        resultsThat match
                            case Falsified(f, s) => Falsified("Right Prop failed\n" + f, s)
                            case x => x
                           

        }

    def ||(that: Prop): Prop =
        Prop {
            (maxSize, n, rng) =>
                val resultThis = this.run(maxSize, n, rng)
                resultThis match
                    case Falsified(f, s) => 
                        val resultsThat = that.run(maxSize, n, rng)
                        resultsThat match
                            case Falsified(f, s) => Falsified("Left and Right Props failed\n" + f, s)
                            case x => x
                    case x => x                        
                    

        }

object Prop:

    import Gen.*
    import Result.*

    def buildMsg[A](s: A, e: Exception): String = 
        s"""|test case: $s
            |generated an exception: ${e.getMessage}
            |stack trace:
            |${e.getStackTrace.mkString("\n")}
            |""".stripMargin

    def randomLazyList[A](g: Gen[A])(rng: RNG): LazyList[A] =
        LazyList.unfold(rng)( r => Some(g.sample(r)) )


    def forAll[A](as: Gen[A])(f: A => Boolean): Prop = 
        Prop {
            (max, n, rng) =>
                randomLazyList(as)(rng).zip(LazyList.from(0)).take(n).map {
                    case (a,i) =>
                        try
                            if f(a) then Passed else Falsified(a.toString, i)
                        catch
                            case e: Exception => Falsified(buildMsg(a,e), i)
                }.find(_.isFalsified).getOrElse(Passed)
        }


    def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop =
        Prop {
            (maxSize, n, rng) =>

                val props = 
                    LazyList.from(0).take((n min maxSize) + 1).map( i => forAll(g(i))(f) )

                val casesPerSize = 1 + (n / maxSize)
                val prop = 
                    props.map( {
                        p =>
                            Prop {
                                (m,_,r) =>
                                    p.run(m, casesPerSize, r)
                            }
                    }).toList.reduce(_ && _)
                prop.run(maxSize, n, rng)

        } 

    def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
        forAll(g(_))(f)


    def runProp(p: Prop, 
            maxSize: Int = 100,
            testCases: Int = 100,
            rng: RNG = SimpleRNG(System.currentTimeMillis)): (Boolean, String) =
                p.run(maxSize, testCases, rng) match 
                    case Falsified(msg, n) =>
                        (false, s"! Falsified after $n passed tests:\n $msg")
                    case Passed =>
                        (true, s"+ OK, passed $testCases tests.")
                    case Proved =>
                        (true, s"+ OK, proved property")

    def check(p: => Boolean): Prop =
        Prop {
             (_,_,_) =>
                 if (p) Proved else Falsified("()", 0)
        }

    val S = weighted(
        choose(1,4).map(Executors.newFixedThreadPool) -> 0.75,
        unit(Executors.newCachedThreadPool) -> 0.25
    )

    def forAllPar[A](g: Gen[A])(f: A => Par.Par[Boolean]): Prop =
        import Par.*
        forAll (S ** g) { case s ** a => Par.run(s)(f(a)).get }


    object ** {
        def unapply[A,B](p: (A,B)) = Some(p)
    }


end Prop