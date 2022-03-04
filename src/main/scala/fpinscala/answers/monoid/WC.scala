package fpinscala.answers.monoid

enum WC:
  case Stub(chars: String)
  case Part(lStub: String, words: Int, rStub: String)

object WC:
  
  given wcMonoid: Monoid[WC] with
      import WC.*
      def op(a1: WC, a2: WC): WC =

          (a1, a2) match
              case (Stub(c1), Stub(c2)) => Stub(c1 ++ c2)
              case (Stub(c), Part(l,n,r)) => Part(c ++ l, n, r)
              case (Part(l,n,r), Stub(c)) => Part(l, n, r ++ c)
              case (Part(l1,n1,r1), Part(l2,n2,r2)) => 
                Part(l1, n1+n2+(if (r1++l2).isEmpty then 0 else 1), r2)

      def zero: WC = Stub("")
end WC
