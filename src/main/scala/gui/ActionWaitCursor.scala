package gui

import gui.Images._
import swing.UIElement
import java.awt.{Cursor, Point, Toolkit}
import Images._
import reactive.{Observing, EventStream}

case class ActionWaitCursor(ui: UIElement, actionInProgress: EventStream[ActionInProgress]) extends Observing {
  private lazy val waitCursor = Toolkit.getDefaultToolkit.createCustomCursor(getImage("clock-icon.png"), new Point(0,0), "cursor")

  actionInProgress.foreach { action => action match {
      case Started()  => ui.cursor = waitCursor
      case Finished() => ui.cursor = new Cursor(Cursor.DEFAULT_CURSOR)
    }
  }
}