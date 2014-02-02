

//import java.awt.TextField

import swing._
import swing.Dialog._
import swing.event._
import scala.swing.Label
import scala.swing.MainFrame
import scala.swing.TextField
import scala.swing.event.Key
import scala.swing.event.KeyPressed

object TR {
  var a: Int = 0
  var neueEingabe:Boolean = false
  var zahl: Double = 0
  var neueZahl: Double = 0
  var VZ: String = "="
  var neuesVZ: String = "="
  var label: Label = new Label("0")
  var strLetzteOp = new Array[String](1)
  // speichert die jeweiligen Operationen, d.h. +, -, /, /-, x, x- etc.
  //var z = new Array[String](3)
  var strVZ: Array[String] = new Array[String](1)
  var strInp: Array[String] = new Array[String](1)
  var strE: Array[String] = new Array[String](1)
  var strVZE: Array[String] = new Array[String](1)
  var arrZahl: Array[Double] = new Array[Double](1)
  strVZ(0) = ""
  strVZE(0) = ""
  strInp(0) = "0"
  strE(0) = ""
  arrZahl(0) = 0.0
  strLetzteOp(0) = "="

  //speichert die letzten Eingaben
  var Mem: Double = 0
  var (setE: Boolean, istNeg: Boolean, setKomma: Boolean) = (false, false, false)
  var window = new MainFrame
  var (btn0, btn1, btn2, btn3, btn4, btn5, btn6, btn7, btn8, btn9) = (new Button("0"), new Button("1"), new Button("2"), new Button("3"),
    new Button("4"), new Button("5"), new Button("6"), new Button("7"), new Button("8"), new Button("9"))
  var btnC: Button = new Button("c")
  var (btnMPlus, btnRM, btnMDel) = (new Button("M+"), new Button("Rec M"), new Button("Del M"))
  var (btnPlus, btnMin, btnMal, btnDiv, btnPt, btnEnt) = (new Button("+"), new Button("-"), new Button("x"), new Button("/"), new Button("."), new Button("="))
  var (btnEx, btnE, btnPM, btnBack) = (new Button("Exit"), new Button("E"), new Button("+/-"), new Button("<"))
  var textField: TextField = new TextField("")

  def main(args: Array[String]) {
    window.contents = new BoxPanel(Orientation.Vertical) {
      contents += label
      contents += textField
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += btn1
        contents += btn2
        contents += btn3
        contents += btnPlus
      }
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += btn4
        contents += btn5
        contents += btn6
        contents += btnMin
      }
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += btn7
        contents += btn8
        contents += btn9
        contents += btnMal
      }
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += btnPt
        contents += btn0
        contents += btnE
        contents += btnDiv
      }
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += btnC
        contents += btnMPlus
        contents += btnMDel
        contents += btnRM
      }
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += btnBack
        contents += btnPM
        contents += btnEnt
        contents += btnEx
      }
    }
    window.listenTo(btn0, btn1, btn2, btn3, btn4, btn5, btn6, btn7, btn8, btn9,
      btnC, btnMPlus, btnRM, btnMDel,
      btnPlus, btnMin, btnMal, btnDiv, btnPt, btnEnt,
      btnEx, btnE, btnPM, btnBack,textField.keys)
    //listenTo(textField.keys)
    //reactions += {
    window.reactions += {
      case ButtonClicked(button) =>
        if (button.text >= "0" && button.text <= "9") {
          if (neueEingabe && !setE) {
            strInp(a) = button.text
            neueEingabe = false
          }
          else if (!setE) strInp(a) += button.text
          else strE(a) += button.text
          strAll()
        }
        if (button == btnC) deleteAll()
        if (button == btnMPlus) Mem += textField.text.toDouble
        if (button == btnRM) {
          textField.text = Mem toString()
          neueEingabe = true
        }
        if (button == btnMDel) Mem = 0
        if (button == btnPlus) rechne(textField.text.toDouble, "+")
        if (button == btnMin) rechne(textField.text.toDouble, "-")
        if (button == btnMal) rechne(textField.text.toDouble, "x")
        if (button == btnDiv) rechne(textField.text.toDouble, "/")
        if (button == btnPt && !setKomma && !setE) {
          strInp(a) += "."
          setKomma = true
          strAll()
        }
        if (button == btnEx) System.exit(0)
        if (button == btnE ) { //&& !setE
          //textField.text += "E"
          setKomma = true
          setE = true
          strAll()
          //strE(a) += button.text
        }
        if (button == btnEnt) rechne(textField.text.toDouble, "=")
        if (button == btnPM) {
          if (!istNeg) {
            strVZ(a) = "-"
            istNeg = true
          }
          else {
            strVZ(a) = ""
            istNeg = false
          }
          strAll()
        }
        if (button == btnBack) {}
      //case KeyPressed(_, Key.Enter, _, _) => rechne(textField.text.toDouble, "=")
    }
  }

  def deleteAll(): Unit = {
    strE(a) = ""
    strVZ(a) = ""
    strInp(a) = "0"
    strVZE(a) = ""
    setE = false
    setKomma = false
    istNeg = false
    neuesVZ = "="
    VZ = "="
    zahl = 0
    neueZahl = 0
    strAll()
  }

  def deleteTF(): Unit = {
    strE(a) = ""
    strVZ(a) = ""
    strInp(a) = "0"
    strVZE(a) = ""
    setE = false
    setKomma = false
    istNeg = false

    //strAll()
  }
  def rechne(neueZahl: Double, neuesVZ: String) = {
    if (VZ == "+") zahl = zahl + neueZahl
    if (VZ == "-") zahl -= neueZahl
    if (VZ == "x") zahl *= neueZahl
    if (VZ == "/") zahl /= neueZahl
    if (VZ == "=") zahl = neueZahl
    VZ = neuesVZ
    neueEingabe = true
    strAll()
    textField.text = zahl.toString
    deleteTF()
  }

  def strAll(): Unit = {
    textField.text = strVZ(a) + strInp(a)
    //showMessage(message = strVZ + strInp)
    if (setE) textField.text += "E" + strVZE(a) + strE(a)

  } //+ (if (setE == true) "E" + strVZE + strE)

  def arrayRunter(): Unit = {
    strVZ = strVZ :+ ""
    strVZE = strVZE :+ ""
    strInp = strInp :+ "0"
    strE = strE :+ ""
    arrZahl = arrZahl :+ 0.0
    a += 1
  }

  window.title = "Taschenrechner"
  //window.location = new Point(40, 10)
  //window.size = new Dimension(300, 500)
  window.visible = true
}








