package gui

import swing._
import event.SelectionChanged
import javax.swing.KeyStroke
import java.awt.event.{ActionEvent, KeyEvent}
import Images._
import java.io.File
import reactive._

case class OpenFileMenuItem(start: String) extends MenuItem("Open") with EventStreamSourceProxy[File] { outer =>
  var file: Option[String] = None
  val fileChooser = new FileChooser(new java.io.File(start))

  action = new Action("Open") {
    icon = getIcon("folder-icon.png")
    mnemonic = KeyEvent.VK_O
    accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_O, ActionEvent.ALT_MASK))

    def apply() {
      fileChooser.showOpenDialog(outer)
      file = Some(fileChooser.selectedFile.getPath)
      source.fire(fileChooser.selectedFile)
    }
  }
}


