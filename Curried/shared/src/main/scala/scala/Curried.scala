package scala
import scala.quoted._
object Curried {

  def applyBeginImpl(callee: Expr[_ <: Curried], explicitTypes: Type[_ <: AnyKind]*)(given qctx: QuoteContext): Expr[_] = {
    import qctx.tasty.{_, given}
    val select = Select.unique(callee.unseal, "applyBegin")

    val typeTrees = explicitTypes.view.map(_.unseal).toList
    (if (typeTrees.isEmpty) {
      select
    } else {
      TypeApply(
        select,
        typeTrees
      )
    }).seal

  }

  def applyEndImpl(builder: Expr[_])(given qctx: QuoteContext): Expr[_] = {
    import qctx.tasty.{_, given}
    Select.unique(builder.unseal, "applyEnd").seal
  }

  def applyNextImpl(builder: Expr[_], arg: Expr[_])(given qctx: QuoteContext): Expr[_] = {
    import qctx.tasty.{_, given}
    Select.overloaded(builder.unseal, "applyNext", Nil, List(arg.unseal)).seal
  }

  inline def applyRest(builder: Any) <: Any = ${
    applyEndImpl('builder)
  }

  inline def applyRest(builder: Any, head: Any, tail: Any*) <: Any = {
    applyRest(applyNext(builder, head), tail: _*)
  }

  inline def applyNext(builder: Any, head: Any) <: Any = ${
    applyNextImpl('builder, 'head)
  }

  inline def applyBegin0(callee: Curried) <: Any = ${
    applyBeginImpl('callee)
  }

  inline def applyBegin1[A <: AnyKind](callee: Curried) <: Any = ${
    applyBeginImpl('callee, summon[Type[A]])
  }

  inline def applyBegin2[A <: AnyKind, B <: AnyKind](callee: Curried) <: Any = ${
    applyBeginImpl('callee, summon[Type[A]], summon[Type[B]])
  }
}

/** A marker trait that enables curried varargs.
  *
  * Given a function call `f(a, b, c)`,
  * when `f` is a subtype of [[Curried]],
  * it should be rewritten to `f.applyBegin.applyNext(a).applyNext(b).applyNext(c).applyEnd`.
  *
  * Optionally, some arguments to a Curried call may be a sequence argument marked as `_*`.
  * Given a function call `f(p1, s1: _*, p2)`,
  *  when translating it to the curried form,
  *  the sequence argument will becomes a `foldLeft` call.
  *
  * <pre>
  * f.applyBegin
  *   .applyNext(p1)
  *   .applyNextSeq(s1)
  *   .applyNext(p2)
  * .applyEnd
  * </pre>
  *
  * Unlike traditional repeated parameters,
  * which restrict the sequence argument at the last position,
  * sequence arguments in a curried call are allowed at any position.
  *
  * When a [[Curried]] is invoked with some type arguments,
  * those type arguments will be moved to the `applyBegin` method.
  * Therefore, `List[Int](1 to 3: _*)` should be translated to `(1 to 3).foldLeft(List.applyBegin[Int])(_.applyNext(_)).applyEnd`.
  *
  * @example Fast list builder
  *
  * {{{
  * class PartiallyAppliedInitializer[A](builder: collection.mutable.Builder[A, List[A]]) {
  *   def applyEnd = builder.result
  *   def applyNextSeq(seq: Seq[A]) = {
  *     builder ++= seq
  *     this
  *   }
  *   def applyNext(a: A) = {
  *     builder += a
  *     this
  *   }
  * }
  * object FastListInitializer extends Curried {
  *   def applyBegin[A]:PartiallyAppliedInitializer[A] = new PartiallyAppliedInitializer(List.newBuilder[A])
  * }
  *
  * FastListInitializer(0, 4) should be(List(0, 4))
  * FastListInitializer[Int](0, 100 to 103: _*, 1) should be(List(0, 100, 101, 102, 103, 1))
  * }}}
  */
trait Curried extends Any with LowPriorityCurried {
  inline def apply(varargs: Any*) <: Any = {
    Curried.applyRest(Curried.applyBegin0(this), varargs: _*)
  }
}

trait LowPriorityCurried extends Any { this: Curried =>

  inline def apply[A <: AnyKind](varargs: Any*) <: Any = {
    Curried.applyRest(Curried.applyBegin1[A](this), varargs: _*)
  }
  inline def apply[A <: AnyKind, B <: AnyKind](varargs: Any*) <: Any = {
    Curried.applyRest(Curried.applyBegin2[A, B](this), varargs: _*)
  }

}