package essays

import gui._
import swing._
import event.{Event, SelectionChanged}
import java.awt.event.{ActionEvent, KeyEvent}
import javax.swing.{KeyStroke}
import Images._
import java.awt.{Dimension}
import BackgroundAction._

object CountApp extends SimpleSwingApplication with Publisher {

   implicit lazy val top = new MainFrame {
     title = "Words counter"
     size = new Dimension(900, 700)
     preferredSize = size
     iconImage = getImage("stats-icon.png")

     menuBar  = LeftMenuBar(openFileMenuItem, countMenuItem)
     contents = PositionedBorderPanel(north = selectedFilePanel, center = resultsPanel)
  }

  lazy val openFileMenuItem  = OpenFileMenuItem("./src/test/resources")
  lazy val countMenuItem     = ActionMenuItem(CountAction())

  lazy val selectedFilePanel = new LabeledFieldPanel("Selected file")
  lazy val resultsPanel      = new ResultsPanel()

  listenTo(openFileMenuItem, countMenuItem)
  reactions += {
    case SelectionChanged(_) => openFileMenuItem.file.map(selectedFilePanel.text = _); publish(DoCount())
    case DoCount()           => openFileMenuItem.file.map { f => publish(CountDone(WordCounter.count(f))).invokeLater }
    case CountDone(r)        => resultsPanel.countDone(r)
  }


}

case class CountAction() extends PublishableAction("Count") {
  icon = getIcon("stats-icon.png")
  mnemonic = KeyEvent.VK_C
  accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_C, ActionEvent.ALT_MASK))
  def apply() {
    publish(DoCount())
  }
}
case class CountDone(results: Results) extends Event
case class DoCount() extends Event

