package co.uproot.abandon

import org.apache.fop.events.Event
import org.apache.fop.events.EventFormatter
import org.apache.fop.events.EventListener
import org.apache.fop.events.model.EventSeverity
import org.apache.fop.apps.FOUserAgent

/** A simple event listener that writes the events to stdout and stderr. */
class SysOutEventListener(fileName: String) extends EventListener {

    def processEvent(event: Event):Unit = {
        val msg = EventFormatter.format(event)
        val severity = event.getSeverity()
        if (severity == EventSeverity.INFO) {
            // println("[INFO ] " + msg)
        } else if (severity == EventSeverity.WARN) {
            println(fileName + " [WARN ] " + msg)
        } else if (severity == EventSeverity.ERROR) {
            println(fileName + " [ERROR] " + msg)
        } else if (severity == EventSeverity.FATAL) {
            println(fileName + " [FATAL] " + msg)
        } else {
            assert(false)
        }
    }
}

object Logging {
  def setup(fileName: String, foUserAgent: FOUserAgent):Unit = {
    foUserAgent.getEventBroadcaster().addEventListener(new SysOutEventListener(fileName))
  }
}
