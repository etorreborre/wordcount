package essays

import gui._
import swing._
import Images._
import java.awt.{Dimension}
import BackgroundAction._
import reactive._

object CountApp extends SimpleSwingApplication with Observing {

  /**
   * Main frame. It is defined as implicit so that any Background action invoked with invokeLater can set a wait cursor on it
   */
   implicit lazy val top = new MainFrame {
     title = "Words counter"
     size = new Dimension(900, 700)
     preferredSize = size
     iconImage = getImage("stats-icon.png")

     menuBar  = LeftMenuBar(openFileMenuItem, countMenuItem)
     contents = PositionedBorderPanel(north = selectedFilePanel, center = resultsPanel)
  }

  /**
   * GUI components
   */
  lazy val openFileMenuItem: OpenFileMenuItem   = OpenFileMenuItem("./src/test/resources")
  lazy val countAction                          = CountAction()
  lazy val countMenuItem                        = ActionMenuItem(countAction)
  lazy val selectedFilePanel: LabeledFieldPanel = new LabeledFieldPanel("Selected file", filePath.change)
  lazy val resultsPanel                         = ResultsPanel(results)
  val actionWaitCursor: ActionWaitCursor        = ActionWaitCursor(top, actionProgress)

  /**
   * Event streams
   */
  lazy val filePoller                    = new FilePoller(filePath)
  lazy val filePath: Signal[String]      = openFileMenuItem.map(_.getPath).hold("")
  lazy val doCount: EventStream[Any]     = filePath.distinct.change | filePoller | countAction
  lazy val actionProgress                = new EventSource[ActionInProgress]
  lazy val results: EventStream[Results] = doCount flatMap { doIt => WordCounter.count(filePath.now).inBackground(actionProgress) }

}

