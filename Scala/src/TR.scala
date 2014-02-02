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
  var b: Int = 0
  var stellenE: Float = 0
  var neueEingabe: Boolean = false
  var zahl: Double = 0
  var neueZahl: Double = 0
  var VZ: String = "="
  var neuesVZ: String = "="
  var label: Label = new Label("0")
  var strInp: String = ""
  var arrZahl = Array[Double]()
  var Mem: Double = 0
  var (setE: Boolean, istNeg: Boolean, setKomma: Boolean, eIstNeg: Boolean) = (false, false, false, false)
  var window = new MainFrame
  var (btn0, btn1, btn2, btn3, btn4, btn5, btn6, btn7, btn8, btn9) = (new Button("0"), new Button("1"), new Button("2"), new Button("3"),
    new Button("4"), new Button("5"), new Button("6"), new Button("7"), new Button("8"), new Button("9"))
  var btnC: Button = new Button("C")
  var (btnMPlus, btnRM, btnMDel) = (new Button("M+"), new Button("Rec M"), new Button("Del M"))
  var (btnPlus, btnMin, btnMal, btnDiv, btnPt, btnEnt) = (new Button("+"), new Button("-"), new Button("x"), new Button("/"), new Button("."), new Button("="))
  var (btnEx, btnE, btnPM, btnEPM, btnBack, btnPrevRes, btnNextRes) = (new Button("Exit"), new Button("E"), new Button("+/-"), new Button("E+/-"),
    new Button("<"), new Button("<< Ergebn."), new Button("Ergeb. >>"))
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
        contents += btnBack
        contents += btnDiv
      }
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += btnC
        contents += btnPM
        contents += btnE
        contents += btnEPM
      }
      contents += new BoxPanel(Orientation.Horizontal) {
        //contents += btnC
        contents += btnMPlus
        contents += btnMDel
        contents += btnRM
        contents += btnEnt
      }
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += btnPrevRes
        contents += btnNextRes
        contents += btnEx
      }
    }
    //btnEnt.defaultButton
    window.listenTo(btn0, btn1, btn2, btn3, btn4, btn5, btn6, btn7, btn8, btn9,
      btnC, btnMPlus, btnRM, btnMDel,
      btnPlus, btnMin, btnMal, btnDiv, btnPt, btnEnt,
      btnEx, btnE, btnPM, btnEPM, btnBack, btnPrevRes, btnNextRes, textField.keys)


    window.reactions += {
      case KeyPressed(_, Key.Enter, _, _) => label.text_=(textField.text)
      case ButtonClicked(button) =>
        if (button.text >= "0" && button.text <= "9") {
          if (neueEingabe && !setE) {
            textField.text = button.text
            neueEingabe = false
          }
          else textField.text += button.text
        }
        if (button == btnC) {
          textField.text = "0"
          strInp = "0"
          setE = false
          setKomma = false
          arrZahl.drop(a - 1)
          b = 0
          zahl = 0
          neueZahl = 0
          neueEingabe = true
        }
        if (button == btnMPlus) {
          Mem += textField.text.toDouble
          neueEingabe = true
        }
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
          textField.text += "."
          setKomma = true
        }
        if (button == btnEx) System.exit(0)
        if (button == btnE && !setE) {
          textField.text += "E"
          setKomma = true
          setE = true
        }
        if (button == btnEnt) rechne(textField.text.toDouble, "=")
        if (button == btnPM) {
          strInp = textField.text
          if (strInp(0) == '-') {
            textField.text = strInp.tail
          }
          else textField.text = '-' + strInp
        }
        if (button == btnEPM) {
          if (setE) {
            strInp = textField.text
            if (eIstNeg) {
              //showConfirmation(message = "Hi")
              textField.text = strInp.replaceFirst("E-", "E")
              eIstNeg = false
            }
            else {
              textField.text = strInp.replaceFirst("E", "E-")
              eIstNeg = true
            }
          }
        }
        if (button == btnBack) {
          textField.text = textField.text.init
          neueEingabe = false
        }
        if (button == btnPrevRes) {
          b += 1
          if (b >= 0 && b <= a) textField.text = arrZahl(a - b).toString
        }
        if (button == btnNextRes) {
          b -= 1
          if (b >= 0 && b <= a) textField.text = arrZahl(a - b).toString
        }

      //case KeyPressed(textField.text, Key.Enter, _, _) => rechne(textField.text.toDouble, "=")
    }

  }

  def rechne(neueZahl: Double, neuesVZ: String) = {
    a += 1
    arrZahl = arrZahl :+ neueZahl
    var y: Int = 0
    arrZahl(a) = neueZahl
    if (VZ == "+") zahl = zahl + neueZahl
    if (VZ == "-") zahl -= neueZahl
    if (VZ == "x") zahl *= neueZahl
    if (VZ == "/") zahl /= neueZahl
    if (VZ == "=") zahl = neueZahl
    VZ = neuesVZ
    textField.text = zahl.toString
    strInp = "0"
    setE = false
    setKomma = false
    stellenE = 0
    neueEingabe = true
    eIstNeg = false
  }

  window.title = "Taschenrechner"
  window.visible = true
}
