package dan

sealed trait Reply {
  def line: String
}

case class NICKCmd(name: String) extends Reply {
  def line = "NICK %s" format name
}

case class USERCmd(name: String) extends Reply {
  def line = "USER %s 8 * : 0.1.0" format name
}

case class PASSCmd(pwd: String) extends Reply {
  def line = "PASS %s" format pwd
}

case class JOINCmd(channel: String) extends Reply {
  def line = "JOIN %s" format channel
}

case class PONGCmd(r: String) extends Reply {
  def line = "PONG %s" format r
}

case class PRIVMSGCmd(who: String, msg: String) extends Reply {
  def line = "PRIVMSG %s :%s" format(who, msg)
}

case class PARTCmd(chan: String, why: Option[String] = None) extends Reply {
  def line = "PART %s%s" format(chan, why.map(" :%s" format _).getOrElse(""))
}

case class INVITECmd(who: String, where: String) extends Reply {
  def line = "INVITE %s :%s" format(who, where)
}

case class UNUSHCmd(chan: String, nick: String) extends Reply {
  def line = "MODE %s +v %s" format(chan, nick)
}

case class HUSHCmd(chan: String, nick: String) extends Reply {
  def line = "MODE %s -v %s" format(chan, nick)
}

case class UNBANCmd(chan: String, nick: String) extends Reply {
  def line = "MODE %s +b %s" format(chan, nick)
}

case class BANCmd(chan: String, nick: String) extends Reply {
  def line = "MODE %s -b %s" format(chan, nick)
}

case class PROMOTECmd(chan: String, nick: String) extends Reply {
  def line = "MODE %s +o %s" format(chan, nick)
}

case class DEMOTECmd(chan: String, nick: String) extends Reply {
  def line = "MODE %s -o %s" format(chan, nick)
}

case class TOPICCmd(chan: String, top: String) extends Reply {
  def line = "TOPIC %s :%s" format(chan, top)
}

// ban

object QUITCmd extends Reply {
  def line = "QUIT"
}

