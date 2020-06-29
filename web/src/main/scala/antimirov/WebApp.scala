package antimirov.web

import antimirov.Rx
import org.scalajs.dom
import org.scalajs.dom.{document, html}
import scala.scalajs.js.annotation.JSExportTopLevel
import scala.util.{Failure, Success, Try}

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

object WebApp {

  @js.native
  @JSGlobal("drawGraph")
  def drawGraph(identifier: String, dot: String): String = js.native

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

  def compile(s: String): Try[Rx] =
    Try(Rx.parse(s))

  var alphaStr: String = ""
  var betaStr: String = ""
  var strStr: String = "hello world"

  var alphaRx: Option[Rx] = None
  var alphaError: String = ""

  var betaRx: Option[Rx] = None
  var betaError: String = ""

  def text(s: String): org.scalajs.dom.raw.Element = {
    val span = document.createElement("span")
    span.textContent = s
    span
  }

  def errorText(s: String): org.scalajs.dom.raw.Element = {
    val italic = document.createElement("i")
    italic.textContent = s
    italic
  }

  def writeInto(idx: String, s: String): Unit =
    writeInto(idx, text(s))

  def writeInto(idx: String, elem: org.scalajs.dom.raw.Element): Unit = {
    val node = document.getElementById(idx)
    while (node.hasChildNodes) node.removeChild(node.lastChild)
    node.appendChild(elem)
    ()
  }

  def updateAll(): Unit = {
    val alpha: String = document.getElementById("alpha").asInstanceOf[html.TextArea].value
    val beta: String = document.getElementById("beta").asInstanceOf[html.TextArea].value
    val str: String = document.getElementById("str").asInstanceOf[html.TextArea].value

    val ok =
      alphaRx.isDefined && alpha == alphaStr &&
      betaRx.isDefined && beta == betaStr &&
      str == strStr

    if (ok) return ()

    compile(alpha) match {
      case Success(rx) =>
        alphaRx = Some(rx)
        alphaError = ""
      case Failure(e) =>
        alphaRx = None
        alphaError = s"α: ${e.getMessage}"
    }

    compile(beta) match {
      case Success(rx) =>
        betaRx = Some(rx)
        betaError = ""
      case Failure(e) =>
        betaRx = None
        betaError = s"β: ${e.getMessage}"
    }

    alphaStr = alpha
    betaStr = beta
    strStr = str

    alphaRx match {
      case Some(a) =>
        writeInto("alpha-card", text(a.cardRepr))
        writeInto("not-alpha", text((~a).toString))
        writeInto("str-in-alpha", text(a.accepts(str).toString))
        val dfa = a.toDfa
        writeInto("alpha-dfa", text(dfa.edges.length.toString))
        drawGraph("#alpha-graph", dfa.toDot('α', "monospace"))
      case None =>
        writeInto("alpha-card", errorText(alphaError))
        writeInto("not-alpha", errorText(alphaError))
        writeInto("str-in-alpha", errorText(alphaError))
        writeInto("alpha-dfa", errorText(alphaError))
        writeInto("alpha-graph", "")
    }

    betaRx match {
      case Some(b) =>
        writeInto("beta-card", text(b.cardRepr))
        writeInto("not-beta", text((~b).toString))
        writeInto("str-in-beta", text(b.accepts(str).toString))
        val dfa = b.toDfa
        writeInto("beta-dfa", text(dfa.edges.length.toString))
        drawGraph("#beta-graph", dfa.toDot('β', "monospace"))
      case None =>
        writeInto("beta-card", errorText(betaError))
        writeInto("not-beta", errorText(betaError))
        writeInto("str-in-beta", errorText(betaError))
        writeInto("beta-dfa", errorText(betaError))
        writeInto("beta-graph", "")
    }

    (alphaRx, betaRx) match {
      case (Some(a), Some(b)) =>
        writeInto("alpha-lt-beta", text((a < b).toString))
        writeInto("alpha-eq-beta", text((a === b).toString))
        writeInto("alpha-gt-beta", text((a > b).toString))
        writeInto("alpha-and-beta", text((a & b).toString))
        writeInto("alpha-xor-beta", text((a ^ b).toString))
        writeInto("alpha-minus-beta", text((a - b).toString))
      case _ =>
        val errors =
          (alphaError.nonEmpty, betaError.nonEmpty) match {
            case (true, true) => s"$alphaError, $betaError"
            case (true, false) => alphaError
            case (false, true) => betaError
            case (false, false) => sys.error("should not happen")
          }
        writeInto("alpha-lt-beta", errorText(errors))
        writeInto("alpha-eq-beta", errorText(errors))
        writeInto("alpha-gt-beta", errorText(errors))
        writeInto("alpha-and-beta", errorText(errors))
        writeInto("alpha-xor-beta", errorText(errors))
        writeInto("alpha-minus-beta", errorText(errors))
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
