package gui

import swing.UIElement
import java.awt.Cursor._
import javax.swing.SwingUtilities
import Images._
import java.awt.{Point, Toolkit, Cursor}
import reactive._

class BackgroundAction[T](action: =>T) {

  def inBackground(progress: EventSource[ActionInProgress] = new EventSource[ActionInProgress]()): EventStream[T] = {
    val result: EventSource[T] = new EventSource
    SwingUtilities.invokeLater(new Runnable {
      def run =  try {
        progress.fire(Started())
        result.fire(action)
       } finally { progress.fire(Finished()) }
    })
    result
  }
}

object BackgroundAction {
  implicit def actionToBackgroundAction[T](action: =>T) = new BackgroundAction(action)
}

sealed trait ActionInProgress
case class Started() extends ActionInProgress
case class Finished() extends ActionInProgress
