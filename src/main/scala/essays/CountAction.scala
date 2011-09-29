package essays

import gui.EventSourceAction
import gui.Images._
import javax.swing.KeyStroke
import java.awt.event.{ActionEvent, KeyEvent}
import swing.event.Event

case class CountAction() extends EventSourceAction("Count") {
  icon = getIcon("stats-icon.png")
  mnemonic = KeyEvent.VK_C
  accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_C, ActionEvent.ALT_MASK))
}

case class CountDone(results: Results) extends Event
case class DoCount() extends Event

