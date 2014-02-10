import swing._
import swing.Dialog._
import java.awt.color
import scala.collection.mutable.Queue//
import scala.collection._
import swing.event._
import scala.swing.Label
import scala.swing.MainFrame
import scala.swing.TextField
import scala.swing.event.Key
import scala.swing.event.KeyPressed

object TR {
  var a: Int = 0
  var b: Int = 0
  var zählerKlammer: Int = 0
  var stellenE: Float = 0
  var neueEingabe: Boolean = false
  var zahl = List(0.0)
  //zählen mit zählerKLammer
  var neueZahl = 0.0
  var VZ = List("=")
  //zählen mit zählerKlammer
  var neuesVZ: String = "="
  var label: Label = new Label("0")
  var strInp: String = ""
  var arrZahl = List(0.0)
  //new Array[Double](1)
  var Mem: Double = 0
  var (setE: Boolean, istNeg: Boolean, setKomma: Boolean, eIstNeg: Boolean) = (false, false, false, false)
  var window = new MainFrame
  var (btn0, btn1, btn2, btn3, btn4, btn5, btn6, btn7, btn8, btn9) = (new Button("0"), new Button("1"), new Button("2"), new Button("3"),
    new Button("4"), new Button("5"), new Button("6"), new Button("7"), new Button("8"), new Button("9"))
  var btnC: Button = new Button("C")
  var (btnMPlus, btnRM, btnMDel) = (new Button("M+"), new Button("Rec M"), new Button("Del M"))
  var (btnPlus, btnMin, btnMal, btnDiv, btnPt, btnEnt) = (new Button("+"), new Button("-"), new Button("x"), new Button("/"), new Button("."), new Button("="))
  var (btnEx, btnE, btnPM, btnEPM, btnBack, btnPrevRes, btnNextRes, btnKlaAuf, btnKlaZu) = (new Button("Exit"), new Button("E"), new Button("+/-"), new Button("E+/-"),
    new Button("<"), new Button("<< Ergebn."), new Button("Ergeb. >>"), new Button("("), new Button(")"))
  var textField: TextField = new TextField("")
  btnEPM.enabled = false
  btnRM.enabled = false
  btnMDel.enabled = false
  btnC.enabled = false
  btnPrevRes.enabled = false
  btnNextRes.enabled = false
  btnKlaZu.enabled = false

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
        contents += btnKlaAuf
        contents += btnKlaZu
        //contents += btnBack
        //contents += btnDiv
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
    //btnEnt.defaultButton = true
    //btnEnt.background = (0,0,220)
    window.listenTo(btn0, btn1, btn2, btn3, btn4, btn5, btn6, btn7, btn8, btn9,
      btnC, btnMPlus, btnRM, btnMDel,
      btnPlus, btnMin, btnMal, btnDiv, btnPt, btnEnt,
      btnEx, btnE, btnPM, btnEPM, btnBack, btnPrevRes, btnNextRes, btnKlaAuf, btnKlaZu)
    window.reactions += {
      case KeyPressed(_, Key.Enter, _, _) => label.text_=(textField.text)
      case ButtonClicked(button) =>
        if (button.text >= "0" && button.text <= "9") {
          buttonsActivate()
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
          zahl(zählerKlammer) = 0.0
          neueZahl = 0.0
          neueEingabe = true
          buttonsActivate()
        }
        if (button == btnMPlus) {
          Mem += textField.text.toDouble
          neueEingabe = true
          buttonsActivate()
        }
        if (button == btnRM) {
          textField.text = Mem toString()
          buttonsActivate()
        }
        if (button == btnMDel) {
          Mem = 0.0
          buttonsActivate()
        }
        if (button == btnPlus) rechne(textField.text.toDouble, "+")
        if (button == btnMin) rechne(textField.text.toDouble, "-")
        if (button == btnMal) rechne(textField.text.toDouble, "x")
        if (button == btnDiv) rechne(textField.text.toDouble, "/")
        if (button == btnPt && !setKomma && !setE) {
          textField.text += "."
          setKomma = true
          buttonsActivate()
        }
        if (button == btnEx) System.exit(0)
        if (button == btnE && !setE) {
          textField.text += "E"
          setKomma = true
          setE = true
          buttonsActivate()
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
              buttonsActivate()
            }
            else {
              textField.text = strInp.replaceFirst("E", "E-")
              eIstNeg = true
            }
          }
        }
        if (button == btnBack) {
          textField.text = textField.text.init
          if (textField.text == "") {
            textField.text = "0"
            buttonsActivate()
          }
          neueEingabe = false
        }
        if (button == btnPrevRes) {
          b += 1
          if (b >= 0 && b <= a) {
            textField.text = arrZahl(a - b).toString
            buttonsActivate()
          }
          buttonsActivate()
        }
        if (button == btnNextRes) {
          b -= 1
          if (b >= 0 && b <= a) textField.text = arrZahl(a - b).toString
        }
        if (button == btnKlaAuf) {
          zählerKlammer += 1
          btnKlaZu.enabled = true
        }
      if (button == btnKlaZu) {
        //zählerKlammer -= 1
        rechne(textField.text toDouble, ")")
        zählerKlammer -= 1
      }
      //case KeyPressed(textField.text, Key.Enter, _, _) => rechne(textField.text.toDouble, "=")
    }
  }

  def buttonsActivate() = {
    if (textField.text == "0") {
      btnE.enabled = false
      btnBack.enabled = false
      btnC.enabled = false
    }
    if (a > 0) btnPrevRes.enabled = true else btnPrevRes.enabled = false
    if (b > 0) btnNextRes.enabled = true else btnNextRes.enabled = false
    if (setE) {
      btnPt.enabled = false
      btnE.enabled = false
      btnEPM.enabled = true
    } else {
      btnPt.enabled = true
      btnE.enabled = true
      btnEPM.enabled = false
    }
    //if (setE) btnE.enabled = false else btnE.enabled = true
    if (Mem == 0.0) {
      btnRM.enabled = false
      btnMDel.enabled = false
    } else {
      btnRM.enabled = true
      btnMDel.enabled = true
    }
    if (setKomma && setE) btnPt.enabled = true else btnPt.enabled = false
    if (zählerKlammer > 0) btnKlaZu.enabled = true else btnKlaZu.enabled = false
  }

  def rechne(neueZahl: Double, neuesVZ: String) = {
    a = a + 1 //+= 1
    arrZahl = arrZahl :+ neueZahl //mL = mL :+ 33.74 lt. REPL
    label.text = arrZahl(a).toString // a.toString + " " +
    var temp = zahl(zählerKlammer)
    //arrZahl(a) = neueZahl
    if (VZ == ")") zahl = zahl :+ neueZahl
    if (VZ == "+") temp = zahl(zählerKlammer) + neueZahl
    if (VZ == "-") temp = zahl(zählerKlammer) - neueZahl
    if (VZ == "x") temp = zahl(zählerKlammer) * neueZahl
    if (VZ == "/") temp = zahl(zählerKlammer) / neueZahl
    if (VZ == "=") temp = neueZahl //zahl(zählerKlammer) = temp//.toDouble
    zahl(zählerKlammer) = temp
    VZ = VZ :+ neuesVZ//VZ(zählerKlammer) = neuesVZ
    textField.text = zahl(zählerKlammer).toString
    strInp = "0"
    setE = false
    setKomma = false
    stellenE = 0
    neueEingabe = true
    eIstNeg = false
    buttonsActivate()
  }

  window.title = "Taschenrechner"
  window.visible = true
}
