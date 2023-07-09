package io.cucumber.scala
object StepDetailsFactory {
  def apply(frame: StackTraceElement, name: String, expr: String, manifests: Seq[java.lang.reflect.Type], body: Any => Any) =
    ScalaStepDetails(frame, name, expr, manifests, body)
}
