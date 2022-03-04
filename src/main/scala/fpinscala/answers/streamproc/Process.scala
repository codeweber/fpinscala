package fpinscala.answers.streamproc

// A simple data type for representing stream transformations

// Note that process can be in one of three states:
// - Halt: Indic
enum Process[I,O]:
  case Halt()
  case Await(recv: Option[I] => Process[I,O])
  case Emit(head: O, tail: Process[I,O])

  def repeat: Process[I,O] =

    def go(p: Process[I,O]): Process[I,O] =
      p match
        case Halt() => go(this) // Restart the initial process
        case Await(recv) => 
          Await {
            case None => recv(None) // If None is returned from input, run the original recv routine
            case i => go(recv(i))
          }
        case Emit(h,t) => Emit(h, go(t))

    go(this)

  def apply(s: LazyList[I]): LazyList[O] = 
    this match
      case Halt() => LazyList()
      case Await(recv) =>
        s match
          case h #:: t => recv(Some(h))(t)
          case _ => recv(None)(s)
      case Emit(h, t) =>
        h #:: t(s)

  def |>[O2](p2: Process[O,O2]): Process[I,O2] = 
    p2 match
      case Halt() => Halt()
      case Emit(o2, t2) => Emit(o2, this |> t2)
      case Await(recv2) =>
        this match
          case Halt() => Halt() |> recv2(None)
          case Emit(o, t) => t |> recv2(Some(o))
          case Await(recv) => Await(x => recv(x) |> p2)

  def ++(p: => Process[I,O]): Process[I,O] =
    this match
      case Halt() => p 
      case Emit(h, t) => Emit(h, t ++ p)
      case Await(recv) => Await(recv andThen (_ ++ p))

  def map[O2](f: O => O2): Process[I,O2] = this |> Process.lift(f)

  def flatMap[O2](f: O => Process[I,O2]): Process[I,O2] = 
    this match
      case Halt() => Halt()
      case Emit(h, t) => f(h) ++ t.flatMap(f)
      case Await(recv) => Await( x => recv(x).flatMap(f))

object Process:

  object Emit:
    def apply[I,O](head: O): Process[I,O] = Emit(head, Halt())


  //Define some helper functions
  def await[I,O](f: I => Process[I,O], fallback: Process[I,O] = Halt[I,O]()): Process[I,O] =
    Await[I,O] {
      case Some(i) => f(i)
      case None => fallback
    }

  def liftOne[I,O](f: I => O): Process[I,O] =
    await[I,O]( i => Emit(f(i)) )

  def lift[I,O](f: I => O): Process[I,O] = liftOne(f).repeat

  def filter[I](p: I => Boolean): Process[I,I] =
    Await[I,I] {
      case Some(i) if p(i) => Emit(i)
      case _ => Halt()
    }.repeat

  def loop[S,I,O](z: S)(f: (I,S) => (O,S)): Process[I,O] =
    await[I,O] {
      i => 
        val (o, z2) = f(i, z)
        Emit(o, loop(z2)(f))
    }
    

  def sum: Process[Double, Double] = 
    def go(acc: Double): Process[Double, Double] = 
      await[Double,Double]( x => Emit(acc+x, go(acc+x)))

    go(0.0)

  def sumLoop: Process[Double, Double] = 
    loop(0.0)( (i, agg) => (agg+i, agg+i))

  def take[I](n: Int): Process[I,I] = 
    if n <= 0 then
      Halt()
    else 
      await( x => Emit(x, take(n-1)))

  def id[I]: Process[I,I] =
    await[I,I]( x => Emit(x) ).repeat

  def drop[I](n: Int): Process[I,I] = 
    if (n <= 0) then 
      id 
    else 
      await[I,I]( x => drop(n-1) ) 

  def count[I]: Process[I,Int] =
    def go(n: Int): Process[I,Int] = 
      await[I,Int]( x => Emit(n+1, go(n+1)))

    Emit(0, go(0))


  def countLoop[I]: Process[I, Int] = 
    Emit(0,loop(0)( (i, agg) => (agg+1, agg+1)))


  def mean: Process[Double, Double] =
    def go(agg: Double, num: Double): Process[Double, Double] = 
      await[Double,Double] {
        x =>
          val aggNew = agg+x
          val numNew = num + 1
          val mv = aggNew / numNew
          Emit(mv, go(aggNew, numNew))
      }

    go(0.0,0.0)


  import fpinscala.answers.applicative.Monad

  given processMonad[I]: Monad[[O] =>> Process[I,O]] with
      def unit[A](a: => A): Process[I,A] = Emit(a)
      extension[A](fa: Process[I,A])
        override def flatMap[B](f: A => Process[I,B]): Process[I,B] =
          fa.flatMap(f)

    



