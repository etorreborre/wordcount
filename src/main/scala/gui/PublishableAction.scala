package gui

import swing._
import event.Event

abstract class PublishableAction(title: String) extends Action(title) {
  private var aPublisher: Option[Publisher] = None
  
  def publisher = aPublisher
  def publisher_=(p: Publisher): Unit = aPublisher = Some(p)

  def publish(e: Event) = aPublisher map (_ publish e)
}