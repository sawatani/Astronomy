package org.fathens.moon

object Logger {
  object LogLevel extends Enumeration {
    val DEBUG = Value(4)
    val TRACE = Value(3)
    val INFO = Value(2)
    val WARN = Value(1)
    val FATAL = Value(0)
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
  object Log {
    import Logger._
    val logLevel = getLogLevel(getClass, defaultLogLevel)

    def debug(msg: => Any) = if (LogLevel.DEBUG <= logLevel) Console.println(msg)
    def trace(msg: => Any) = if (LogLevel.TRACE <= logLevel) Console.println(msg)
    def info(msg: => Any) = if (LogLevel.INFO <= logLevel) Console.println(msg)
    def warn(msg: => Any) = if (LogLevel.WARN <= logLevel) Console.println(msg)
    def fatal(msg: => Any) = if (LogLevel.FATAL <= logLevel) Console.println(msg)
  }
}
