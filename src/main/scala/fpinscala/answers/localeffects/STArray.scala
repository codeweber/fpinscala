package fpinscala.answers.localeffects

sealed abstract class STArray[S,A](using manifest: Manifest[A]):
    protected def value: Array[A]

    def size: ST[S, Int] = ST(value.size)

    def write(i: Int, a: A): ST[S, Unit] = 
        new ST[S, Unit]:
            def run(s: S) =
                value(i) = a
                ((), s)

    def read(i: Int): ST[S, A] = ST(value(i))

    def freeze: ST[S, List[A]] = ST(value.toList)

    def fill(xs: Map[Int, A]): ST[S, Unit] = 
        xs.foldLeft(ST[S,Unit](()))( (st, kv) => st.flatMap(_ => write.tupled(kv)) )


    def swap(i: Int, j: Int): ST[S, Unit] = 
        for
            x <- read(i)
            y <- read(j)
            _ <- write(i, y)
            _ <- write(j, x)
        yield ()

    def reverse: ST[S, Unit] = 
        val sz = value.size
        (0 until sz/2).foldLeft(ST[S,Unit](()))( (st, i) => st.flatMap(_ => swap(i, sz-1-i)))

object STArray:

    def apply[S, A:Manifest](sz: Int, v: A): ST[S, STArray[S,A]] = 
        ST {
            new STArray[S,A]:
                lazy val value = Array.fill(sz)(v)
        }


    def fromList[S, A:Manifest](xs: List[A]): ST[S, STArray[S,A]] =
        ST {
            new STArray[S,A]:
                lazy val value = xs.toArray
        }

