package org.fathens.moon

object Logger {
  val sf = new java.text.SimpleDateFormat("yyyy-MM-DD HH:mm:ss.SSS")
  def output(currentLevel: LogLevel.Value)(outLevel: LogLevel.Value)(msg: => Any) {
    if (outLevel <= currentLevel) {
      val date = sf format new java.util.Date()
      val tag = f" ${outLevel}".takeRight(5)
      Console println f"${date}: ${tag}: ${msg}"
    }
  }
  object LogLevel extends Enumeration {
    val DEBUG = Value(4, "DEBUG")
    val TRACE = Value(3, "TRACE")
    val INFO = Value(2, "INFO")
    val WARN = Value(1, "WARN")
    val FATAL = Value(0, "FATAL")
  }
  def getLogLevel(clazz: Class[_], defaultLevel: LogLevel.Value) = {
    try {
      val value = System.getProperty(f"${clazz.getCanonicalName}.logLevel")
      LogLevel(Integer parseInt value)
    } catch {
      case ex: Exception => defaultLevel
    }
  }
  val defaultLogLevel = getLogLevel(getClass, LogLevel.DEBUG)
}
trait Logger {
  protected object Log {
    import Logger._
    val logLevel = getLogLevel(getClass, defaultLogLevel)

    val debug = output(logLevel)(LogLevel.DEBUG)_
    val trace = output(logLevel)(LogLevel.TRACE)_
    val info = output(logLevel)(LogLevel.INFO)_
    val warn = output(logLevel)(LogLevel.WARN)_
    val fatal = output(logLevel)(LogLevel.FATAL)_
  }
}
