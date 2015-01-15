package org.fathens.moon

object Logging {
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
      val name = clazz.getCanonicalName.reverse.dropWhile(_ == '$').reverse
      val value = System.getProperty(f"${name}.logLevel")
      Console println f"Setting logLevel of ${name}(${clazz.getCanonicalName}) to ${value}"
      LogLevel withName value
    } catch {
      case ex: Exception => defaultLevel
    }
  }
  val defaultLogLevel = getLogLevel(getClass, LogLevel.DEBUG)
  class Logger(clazz: Class[_]) {
    val LOG_LEVEL = getLogLevel(clazz, defaultLogLevel)
    val debug = output(LOG_LEVEL)(LogLevel.DEBUG)_
    val trace = output(LOG_LEVEL)(LogLevel.TRACE)_
    val info = output(LOG_LEVEL)(LogLevel.INFO)_
    val warn = output(LOG_LEVEL)(LogLevel.WARN)_
    val fatal = output(LOG_LEVEL)(LogLevel.FATAL)_
  }
}
trait Logging {
  protected val Log = new Logging.Logger(getClass)
}
