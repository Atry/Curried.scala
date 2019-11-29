import meta._

val generateCurriedWithTypeParameters = taskKey[Seq[File]]("Generate CurriedWithTypeParameters.scala file.")

generateCurriedWithTypeParameters := {
  val outputFile = (sourceManaged in Compile).value / "scala" / "CurriedWithTypeParameters.scala"
  IO.write(
    outputFile,
    q"""
      package scala {
        import language.experimental.macros
        private[scala] trait CurriedWithTypeParameters extends Any {
          ..${
            for (i <- (1 to 22)) yield q"""
              def apply[
                ..${
                  for (j <- 0 until i) yield {
                    Type.Param(Nil, Type.Name(s"A$j"), Nil, Type.Bounds(None, None), Nil, Nil)
                  }
                }
              ](varargs: Any*): Any = macro Curried.Macros.apply
            """
          }
        }
      }
    """.syntax,
    scala.io.Codec.UTF8.charSet
  )
  Seq(outputFile)
}

Compile / sourceGenerators ++= {
  if (!isDotty.value) {
    Some(generateCurriedWithTypeParameters.taskValue)
  } else {
    None
  }
} 
