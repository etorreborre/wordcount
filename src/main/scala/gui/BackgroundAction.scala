package gui

import swing.UIElement
import java.awt.Cursor._
import javax.swing.SwingUtilities
import Images._
import java.awt.{Point, Toolkit, Cursor}

class BackgroundAction[T](action: =>T)(implicit parent: UIElement) {
  def execute = {
    SwingUtilities.invokeLater(new Runnable {
      def run =  try {
        parent.cursor = waitCursor
        action
       } finally { parent.cursor = new Cursor(DEFAULT_CURSOR) }
    })
  }

  lazy val waitCursor = Toolkit.getDefaultToolkit.createCustomCursor(getImage("clock-icon.png"), new Point(0,0), "cursor");
}

object BackgroundAction {
  implicit def unitToBackgroundAction[T](action: =>T)(implicit parent: UIElement) = new InvokeLaterBackgroundAction(action)
  class InvokeLaterBackgroundAction[T](action: =>T)(implicit parent: UIElement) {
    def invokeLater = new BackgroundAction(action).execute
  }
}