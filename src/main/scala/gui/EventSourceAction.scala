package gui

import swing._
import reactive.{EventSource, Trigger}

abstract class EventSourceAction(title: String) extends Action(title) with Trigger {
  def apply {
    source.fire(true)
  }
}