package gui

import swing._
import Orientation._
import reactive._

class LabeledFieldPanel(label: String, text: EventStream[String] = new EventSource[String]) extends BoxPanel(Horizontal) with Observing {
  protected lazy val textField = new TextField("") { editable = false }

  contents += new Label(label+" ")
  contents += textField
  border = Swing.EtchedBorder(Swing.Lowered)

  text.foreach { t => textField.text = t }
}
