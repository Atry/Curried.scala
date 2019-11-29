package scala

object CurriedTest {
  def main(args: Array[String]): Unit = {

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
    
    assert(FastListInitializer(0, 4) == List(0, 4))

  }
}