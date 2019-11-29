package scala


import org.junit.Test
import org.junit.Assert._

class CurriedTest {
  @Test def t1(): Unit = {
    class PartiallyAppliedInitializer[A](builder: collection.mutable.Builder[A, List[A]]) {
      def applyEnd = builder.result
      def applyNextSeq(seq: Seq[A]) = {
        builder ++= seq
        this
      }
      def applyNext(a: A) = {
        builder += a
        this
      }
    }
    object FastListInitializer extends Curried {
      def applyBegin[A]:PartiallyAppliedInitializer[A] = new PartiallyAppliedInitializer(List.newBuilder[A])
    }
    
    assertEquals(List(0, 4), FastListInitializer(0, 4))
  }
}
