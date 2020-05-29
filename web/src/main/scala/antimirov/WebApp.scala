package antimirov.web

import antimirov.Rx
import org.scalajs.dom
import org.scalajs.dom.{document, html}
import scala.scalajs.js.annotation.JSExportTopLevel
import scala.util.{Failure, Success, Try}

object WebApp {

  def appendPar(targetNode: dom.Node, text: String): Unit = {
    val parNode = document.createElement("p")
    parNode.textContent = text
    targetNode.appendChild(parNode)
    ()
  }

  @JSExportTopLevel("addClickedMessage")
  def addClickedMessage(): Unit = {
    appendPar(document.body, "You clicked the button!")
  }

  def main(args: Array[String]): Unit = {
    document.addEventListener("DOMContentLoaded", { (e: dom.Event) =>
      setupUI()
    })
  }

  def compile(s: String): Option[Rx] =
    Try(Rx.parse(s)) match {
      case Success(rx) => Some(rx)
      case Failure(e) =>
        println(e)
        None
    }

  var alphaStr: String = ".*"
  var betaStr: String = ".*"
  var strStr: String = "hello world"

  var alphaRx: Option[Rx] = None
  var betaRx: Option[Rx] = None

  def writeInto(idx: String, value: String): Unit = {
    println(s"write $value into $idx")
    // it's really an <output> element but this seems to work.
    val area = document.getElementById(idx).asInstanceOf[html.TextArea]
    area.value = value
    ()
  }

  def updateAll(): Unit = {
    println("updateAll ran!")
    val alpha: String = document.getElementById("alpha").asInstanceOf[html.TextArea].value
    val beta: String = document.getElementById("beta").asInstanceOf[html.TextArea].value
    val str: String = document.getElementById("str").asInstanceOf[html.TextArea].value

    val ok =
      alphaRx.isDefined && alpha == alphaStr &&
      betaRx.isDefined && beta == betaStr &&
      str == strStr

    if (ok) return ()

    println("done some real work")

    alphaRx = compile(alpha)
    betaRx = compile(beta)

    alphaStr = alpha
    betaStr = beta
    strStr = str

    val error = "error"

    println((alphaRx, betaRx).toString)

    alphaRx match {
      case Some(a) =>
        writeInto("not-alpha", (~a).toString)
        writeInto("str-in-alpha", a.accepts(str).toString)
      case None =>
        writeInto("not-alpha", error)
        writeInto("str-in-alpha", error)
    }

    betaRx match {
      case Some(b) =>
        writeInto("str-in-beta", b.accepts(str).toString)
      case None =>
        writeInto("str-in-beta", error)
    }

    (alphaRx, betaRx) match {
      case (Some(a), Some(b)) =>
        writeInto("alpha-lt-beta", (a < b).toString)
        writeInto("alpha-eq-beta", (a === b).toString)
        writeInto("alpha-gt-beta", (a > b).toString)
        writeInto("alpha-and-beta", (a & b).toString)
        writeInto("alpha-xor-beta", (a ^ b).toString)
        writeInto("alpha-minus-beta", (a - b).toString)
      case _ =>
        writeInto("alpha-lt-beta", error)
        writeInto("alpha-eq-beta", error)
        writeInto("alpha-gt-beta", error)
        writeInto("alpha-and-beta", error)
        writeInto("alpha-xor-beta", error)
        writeInto("alpha-minus-beta", error)
    }
  }

  def setupUI(): Unit = {
    List("alpha", "beta", "str").foreach { idx =>
      val area = document.getElementById(idx).asInstanceOf[html.TextArea]
      area.addEventListener("keyup", { (_: dom.KeyboardEvent) => updateAll() })
    }
    updateAll()
  }
}
