package dan

trait Reply {
  def line: String
}

case class NICK(name: String) extends Reply {
  def line = "NICK %s" format name
}

case class USER(name: String) extends Reply {
  def line = "USER %s 8 * : 0.1.0" format name
}

case class PASS(pwd: String) extends Reply {
  def line = "PASS %s" format pwd
}

case class JOIN(channel: String) extends Reply {
  def line = "JOIN %s" format channel
}

case class PONG(r: String) extends Reply {
  def line = "PONG %s" format r
}

case class PRIVMSG(who: String, msg: String) extends Reply {
  def line = "PRIVMSG %s :%s" format(who, msg)
}

case class PART(chan: String, why: Option[String] = None) extends Reply {
  def line = "PART %s%s" format(chan, why.map(" :%s" format _).getOrElse(""))
}

case class INVITE(who: String, where: String) extends Reply {
  def line = "INVITE %s :%s" format(who, where)
}

case class UNUSH(chan: String, nick: String) extends Reply {
  def line = "MODE %s +v %s" format(chan, nick)
}

case class HUSH(chan: String, nick: String) extends Reply {
  def line = "MODE %s -v %s" format(chan, nick)
}

case class UNBAN(chan: String, nick: String) extends Reply {
  def line = "MODE %s +b %s" format(chan, nick)
}

case class BAN(chan: String, nick: String) extends Reply {
  def line = "MODE %s -b %s" format(chan, nick)
}

case class PROMOTE(chan: String, nick: String) extends Reply {
  def line = "MODE %s +o %s" format(chan, nick)
}

case class DEMOTE(chan: String, nick: String) extends Reply {
  def line = "MODE %s -o %s" format(chan, nick)
}

case class TOPIC(chan: String, top: String) extends Reply {
  def line = "TOPIC %s :%s" format(chan, top)
}


// ban

object QUIT extends Reply {
  def line = "QUIT"
}

