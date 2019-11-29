package scala
import scala.quoted._
object Curried {

  def applyImpl(callee: Expr[_ <: Curried], args: Expr[Seq[_]], explicitTypes: Type[_ <: AnyKind]*)(given qctx: QuoteContext): Expr[_] = {
    import qctx.tasty.{_, given}

    val unpackedArgs = args.unseal.underlyingArgument match {
      case Typed(Repeated(unpackedArgs, _), _) =>
        unpackedArgs
      case tree =>
        Nil
    }

    println(unpackedArgs)

    val applyBegin = {
      val typeTrees = explicitTypes.view.map(_.unseal).toList
      val select = Select.unique(callee.unseal.underlyingArgument, "applyBegin")
      if (typeTrees.isEmpty) {
        select
        // FIXME: inference type parameters
      } else {
        TypeApply(
          select,
          typeTrees
        )
      }
    }

    val builder = unpackedArgs.foldLeft(applyBegin) { (builder, arg) =>
      Select.overloaded(builder, "applyNext", Nil, List(arg))
    }

    val result = Select.unique(builder, "applyEnd")

    result.seal
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
  inline def apply(varargs: Any*) <: Any = ${
    Curried.applyImpl('this, 'varargs)
  }
}

trait LowPriorityCurried extends Any { this: Curried =>

  inline def apply[A <: AnyKind](varargs: Any*) <: Any = ${
    Curried.applyImpl('this, 'varargs, summon[Type[A]])
  }
  inline def apply[A <: AnyKind, B <: AnyKind](varargs: Any*) <: Any = ${
    Curried.applyImpl('this, 'varargs, summon[Type[A]], summon[Type[B]])
  }

}