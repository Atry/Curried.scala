package scala
import language.experimental.macros
import reflect.macros.whitebox
private[scala] object Curried {
  final class Macros(val c: whitebox.Context) {
    import c.universe._
    def apply(varargs: Tree*): Tree = {
      val q"${typeApply @ q"$callee.$_[..$typeArguments]"}(..$_)" = c.macroApplication
      q"""
        ${
          varargs.foldLeft[Tree](atPos(typeApply.pos)(q"$callee.applyBegin[..$typeArguments]")) { (partiallyAppliedCallee, argument) =>
            atPos(argument.pos) {
              argument match {
                case q"$sequenceArgument: _*" =>
                  q"$partiallyAppliedCallee.applyNextSeq($sequenceArgument)"
                case _ =>
                  q"$partiallyAppliedCallee.applyNext($argument)"
              }
            }
          }
        }.applyEnd
      """
    }
  }
}

/** A marker trait that enables curried varargs.
  *
  * Given a function call `f(a, b, c)`,
  * when f is a subtype of [[Curried]],
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
trait Curried extends Any with CurriedWithTypeParameters {
  def apply(varargs: Any*): Any = macro Curried.Macros.apply
}
