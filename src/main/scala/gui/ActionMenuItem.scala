package gui

import swing.{Publisher, Action, MenuItem}

case class ActionMenuItem(a: PublishableAction) extends MenuItem(a.title) with Publisher {
  action = a
  a.publisher = this
}