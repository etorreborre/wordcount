package reactive

import java.io.File

class FilePoller(path: Signal[String], delay: Long = 200) extends Trigger {

  private var lastModified = new File(path.now).lastModified()

  val timer = new Timer(0, delay, {t =>  false}) foreach { tick =>
    if (new File(path.now).lastModified() > lastModified) {
      lastModified = new File(path.now).lastModified()
      source.fire(true)
    }
  }
}


