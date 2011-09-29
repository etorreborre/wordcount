package gui

import swing.MenuItem

case class ActionMenuItem(a: EventSourceAction) extends MenuItem(a.title) {
  action = a
}