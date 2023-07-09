//> using file ../src/Ziocumber.scala
//> using file ../src/StepDetailsFactory.scala
//> using dep "dev.zio::zio-test-sbt:2.0.15"
package io.github.nightscape.ziocumber.test

import zio._
import zio.Console._
import zio.test._

import scala.jdk.CollectionConverters._
import io.github.nightscape.ziocumber.Ziocumber
import io.cucumber.datatable.DataTable
import java.nio.file.Path
import java.nio.file.Paths

object ZiocumberStepsTest extends Ziocumber {
  override def featureFiles: Seq[Path] = Seq(
    Paths.get("./test/Ziocumber.feature")
  )
  private val calcRefZio = Ref.make(new RpnCalculator)
  override val stepDefinitions = calcRefZio.map(calcRef =>
    Seq(
      Given("a calculator I just turned on") { () =>
        calcRef.set(new RpnCalculator)
      },
      When("I add {int} and {int}") { (arg1: Int, arg2: Int) =>
        calcRef.update { calc =>
          Seq(arg1, arg2, "+").foldLeft(calc) { (calc, arg) =>
            calc.push(arg)
          }
        }
      },
      When("I press (.+)$") { (what: String) =>
        calcRef.update(_.push(what))
      },
      Then("the result is {int}") { (expected: Int) =>
        calcRef.get.map(calc => assertTrue(calc.value().intValue() == expected))
      },
      Given("the previous entries:") { (entries: List[Entry]) =>
        calcRef.update { calc =>
          entries.foldLeft(calc) { (c, entry) =>
            Seq(entry.first, entry.second, entry.operation).foldLeft(c) {
              (c, arg) => c.push(arg)
            }
          }
        }
      }
    )
  )
}
case class Entry(first: Int, second: Int, operation: String)

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class RpnCalculator(stack: List[Int] = List.empty) {
  def push(arg: Any): RpnCalculator = {
    val newStack = (arg, stack) match {
      case ("-", x :: y :: remainder) => (x - y) :: remainder
      case ("+", x :: y :: remainder) => (x + y) :: remainder
      case ("*", x :: y :: remainder) => (x * y) :: remainder
      case ("/", x :: y :: remainder) => (x / y) :: remainder
      case (parseable, _)             => parseable.toString().toInt :: stack
    }
    RpnCalculator(newStack)
  }

  def PI(): RpnCalculator = {
    push(Math.PI)
  }

  def value(): Int = {
    stack.head
  }
}
