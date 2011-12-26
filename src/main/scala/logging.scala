package dan

trait Logged {
  def log: Logger
}

trait ConsoleLogged extends Logged {
  def log: Logger = ConsoleLogger
}

trait SilentlyLogged extends Logged {
  def log: Logger = SilentLogger
}

trait Logger {
  def trace(msg: => String): Unit
  def debug(msg: => String): Unit
  def info(msg: => String): Unit
  def error(msg: => String, err: Option[Throwable] = None): Unit
}

object ConsoleLogger extends Logger {
  def trace(msg: => String) = ()
  def debug(msg: => String) = println(msg)
  def info(msg: => String) = println(msg)
  def error(msg: => String, err: Option[Throwable] = None) =
    Console.err.println(msg)
}

object SilentLogger extends Logger {
  def trace(msg: => String) = ()
  def debug(msg: => String) = ()
  def info(msg: => String) = ()
  def error(msg: => String, err: Option[Throwable] = None) = ()
}

