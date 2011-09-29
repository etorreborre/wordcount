package reactive

import java.io.File

class FilePoller(path: Signal[String]) extends Trigger {

  val lastModified = Var(new File(path.now).lastModified())
  val timer = new Timer(0, 200, {t =>  false}) foreach { tick =>
    if (new File(path.now).lastModified() > lastModified.now) {
      lastModified() = new File(path.now).lastModified()
      source.fire(true)
    }
  }
}

