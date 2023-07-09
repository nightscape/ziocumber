package io.github.nightscape.ziocumber

import java.io.File
import java.lang.reflect.{ParameterizedType, Type}
import java.time.Clock
import java.util.stream.Collectors
import java.util.{Locale, UUID, List => JList}

import io.cucumber.core.runtime.TimeServiceEventBus
import io.cucumber.core.stepexpression.{
  Argument,
  StepExpression,
  StepExpressionFactory,
  StepTypeRegistry
}
import io.cucumber.cucumberexpressions.{
  ExpressionFactory,
  ParameterTypeRegistry
}
import io.cucumber.messages.types.Feature
import io.cucumber.messages.types.Step
import io.cucumber.scala.{ScalaScenarioScopedStepDefinition, StepDetailsFactory}
import zio.{RIO, ZIO}
import zio.test.Assertion._
import zio.test._

import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters.RichOptional
import io.cucumber.gherkin.GherkinParser
import java.nio.file.Path
import izumi.reflect.macrortti.LightTypeTag
import izumi.reflect.Tag

abstract class Ziocumber extends ZIOSpecDefault { self =>
  def featureFiles: Seq[Path]
  val parser = GherkinParser.builder().build()
  val featureDocuments = featureFiles
    .map(parser.parse(_))
    .flatMap(_.iterator().asScala)
    .flatMap(_.getGherkinDocument().toScala)
  val features = featureDocuments.flatMap(_.getFeature().toScala)
  val expressionFactory = new ExpressionFactory(
    new ParameterTypeRegistry(Locale.ENGLISH)
  )
  private val uuidSupplier: java.util.function.Supplier[UUID] = () =>
    UUID.randomUUID()
  val stepExpressionFactory = new StepExpressionFactory(
    new StepTypeRegistry(Locale.ENGLISH),
    new TimeServiceEventBus(Clock.systemUTC(), uuidSupplier)
  )

  def frame(self: Any): StackTraceElement = {
    val frames = Thread.currentThread().getStackTrace
    val currentClass = self.getClass.getName
    // Note: the -1 check is here for Scala < 2.13 and objects
    frames.reverse
      .find(f => f.getClassName == currentClass && f.getLineNumber != -1)
      .get
  }
  case class ZioStepDefinition[R, I, O](
      name: String,
      expr: String,
      body: I => RIO[R, O],
      manifests: Tag[_]*
  ) extends ScalaScenarioScopedStepDefinition(
        StepDetailsFactory(
          frame(self),
          name,
          expr,
          manifests.map(t => typeFromManifest(t.tag)),
          i => body(i.asInstanceOf[I])
        )
      ) {
    def fillBody(i: I): RIO[R, O] = body(i)
    def unsafeFillBody(a: Any): RIO[R, O] = fillBody(a.asInstanceOf[I])
  }

  def step(s: String) = new ZStep(s)
  def Given(s: String) = new ZStep("Given " + s)
  def When(s: String) = new ZStep("When " + s)
  def Then(s: String) = new ZStep("Then " + s)

  class ZStep(text: String) {
    val Array(name, regex) = text.split(" ", 2)

    private def doApply[R, T1: Tag, O](
        f: T1 => RIO[R, O]
    ): ZioStepDefinition[R, T1, O] =
      ZioStepDefinition(name, regex, f, Tag[T1])
    def apply[R, O](f: Unit => RIO[R, O]): ZioStepDefinition[R, Unit, O] =
      doApply(f)
    def apply[R, T1: Tag, O](f: T1 => RIO[R, O]): ZioStepDefinition[R, T1, O] =
      doApply(f)
    def apply[R, T1: Tag, T2: Tag, O](
        f: (T1, T2) => RIO[R, O]
    ): ZioStepDefinition[R, (T1, T2), O] =
      doApply(f.tupled)
    def apply[R, T1: Tag, T2: Tag, T3: Tag, O](
        f: (T1, T2, T3) => RIO[R, O]
    ): ZioStepDefinition[R, (T1, T2, T3), O] =
      doApply(f.tupled)
    def apply[R, T1: Tag, T2: Tag, T3: Tag, T4: Tag, O](
        f: (T1, T2, T3, T4) => RIO[R, O]
    ): ZioStepDefinition[R, (T1, T2, T3, T4), O] =
      doApply(f.tupled)
  }
  def stepDefinitions
      : ZIO[TestEnvironment, Throwable, Seq[ZioStepDefinition[_, _, _]]]

  private def emptyCellsToNull(
      cells: JList[JList[String]]
  ): JList[JList[String]] =
    cells
      .stream()
      .map(row =>
        row
          .stream()
          .map(s => if (s.isEmpty) null else s)
          .collect(Collectors.toList())
      )
      .collect(Collectors.toList())

  def argumentsFrom(
      expression: StepExpression
  )(step: Step, types: Type*): JList[Argument] = {

    step
      .getDataTable()
      .asScala
      .map { table =>
        val cells = emptyCellsToNull(
          table
            .getRows()
            .stream()
            .map(
              _.getCells.stream().map(_.getValue).collect(Collectors.toList())
            )
            .collect(Collectors.toList())
        )
        expression.`match`(step.getText, cells, types: _*)
      }
      .orElse(step.getDocString.asScala.map { docString =>
        val content = docString.getContent
        val contentType = docString.getMediaType.get()
        expression.`match`(step.getText, content, contentType, types: _*)
      })
      .getOrElse(expression.`match`(step.getText, types: _*))
  }
  private def typeFromManifest(m: LightTypeTag): Type = if (m.typeArgs.isEmpty)
    Class.forName(m.longNameWithPrefix)
  else
    new ParameterizedType {
      def getRawType: Class[_] = Class.forName(m.longName)
      def getActualTypeArguments: Array[Type] =
        m.typeArgs.map(t => typeFromManifest(t)).toArray
      def getOwnerType: Type = null
    }
  def matchingStepDefinition(
      stepDefinitions: Seq[ZioStepDefinition[_, _, _]]
  )(step: Step): (ZioStepDefinition[_, _, _], JList[Argument]) = {
    val matches = stepDefinitions
      .map { sd =>
        val stepExp = stepExpressionFactory.createExpression(sd)
        val args = argumentsFrom(stepExp)(
          step,
          sd.manifests.map(m => typeFromManifest(m.tag)): _*
        )
        sd -> args
      }
      .filterNot(_._2 == null)
    matches match {
      case Seq() =>
        throw new RuntimeException(
          s"Could not find matching step definition for $step among\n${stepDefinitions.mkString("\n")}"
        )
      case Seq(a) => a
      case s =>
        throw new RuntimeException(
          s"Ambiguous step definitions for $step:\n${s.mkString("\n")}"
        )
    }
  }
  def compileStepDefinition(
      sd: ZioStepDefinition[_, _, _],
      args: JList[Argument]
  ) = {
    val argsAsTuple = args.asScala.map(_.getValue).toSeq match {
      case Seq()               => ()
      case Seq(v1)             => v1
      case Seq(v1, v2)         => (v1, v2)
      case Seq(v1, v2, v3)     => (v1, v2, v3)
      case Seq(v1, v2, v3, v4) => (v1, v2, v3, v4)
    }
    val task =
      sd.unsafeFillBody(argsAsTuple).asInstanceOf[RIO[TestEnvironment, _]]
    task
  }
  override def spec: Spec[TestEnvironment, Any] =
    suite(getClass.getSimpleName)(
      features.map { feature =>
        suite(feature.getName)(
          feature.getChildren.asScala
            .flatMap(_.getScenario.toScala)
            .map { s =>
              test(s.getName) {
                stepDefinitions.flatMap { sds =>
                  val matchStep = matchingStepDefinition(sds)(_)
                  val backgroundSteps = feature
                    .getChildren()
                    .asScala
                    .flatMap(_.getBackground.toScala)
                    .flatMap(_.getSteps.asScala)
                  val steps = backgroundSteps ++ s.getSteps.asScala
                  steps
                    .map { step =>
                      val (sd, args) = matchStep(step)
                      compileStepDefinition(sd, args)
                        .catchAll(t =>
                          assertTrue(
                            s"""Step "${step.getKeyword}${step.getText}" (${step.getLocation} failed with "$t"""" == ""
                          )
                        )
                        .map {
                          case t: TestResult => t
                          case _             => assertTrue(true)
                        }
                    }
                    .reduce[RIO[TestEnvironment, TestResult]] { case (f1, f2) =>
                      f1 && f2
                    }
                }
              }
            }
            .toSeq: _*
        )
      }: _*
    )

  implicit class RichZioTestResult[R, E](self: ZIO[R, E, TestResult]) {
    def &&[R2](other: ZIO[R2, E, TestResult]): ZIO[R with R2, E, TestResult] =
      for {
        a1 <- self
        a2 <- other
      } yield a1 && a2
  }
}
