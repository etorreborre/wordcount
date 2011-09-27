package gui

import swing._
import Orientation._

class LabeledFieldPanel(label: String) extends BoxPanel(Horizontal) {
  protected lazy val textField = new TextField("") { editable = false }

  contents += new Label(label+" ")
  contents += textField
  border = Swing.EtchedBorder(Swing.Lowered)

  def text = textField.text
  def text_=(t: String): Unit = textField.text = t
}
