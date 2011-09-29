package reactive

trait Trigger extends EventStreamSourceProxy[Boolean] {
  lazy val source = new EventSource[Boolean]
}