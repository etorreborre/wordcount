package gui

import swing._
import Orientation._
import reactive.{Observing, Signal, Var}

class LabeledFieldPanel(label: String, text: Signal[String]) extends BoxPanel(Horizontal) with Observing {
  protected lazy val textField = new TextField(text.now) { editable = false }

  contents += new Label(label+" ")
  contents += textField
  border = Swing.EtchedBorder(Swing.Lowered)

  text.change.foreach { t => textField.text = t }
}
