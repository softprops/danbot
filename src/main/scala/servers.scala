package dan

import java.nio.charset.Charset
import java.io._
import java.net.{ InetAddress, Socket, SocketException }
import java.util.Random
import java.util.concurrent.{ Callable, Executors, Future }

object Server {
  val DefaultCharset = Charset.forName("utf8")
  def default = new Server("chat.freenode.net", 6667)
}

/** Channels or Users are considered `named` things */
trait Named {
  def name: String
}

/** Talkative things are thing that you can say something to */
trait Talkative { self: Named =>
  def say(what: String): Unit
}

/** Sometaltive's will talk notice faster if you mention their name in a message.
 *  Mentionables provide a wrapper around say which includes their name */
trait Mentionable extends Talkative { self: Named =>
  def mention(what: String): Unit
}

/** Sender is the person that sent a message from a remote location. Senders have names
 *  and you may send them private messages by invoking their `say` method.
 *  You can also get their attention by sending the message via their `mention` method */
trait Sender extends Named with Mentionable

/** Self represents danbot's view of himself. You can request
 *  danbot perform actions on himself. Be nice. */
trait Self extends Named {
  def part(msg: Option[String] = None): Unit
  def quit: Unit
}

/** A Channel represents a room danbot is in. Channels have a name
 *  and you can say things in by invoking the Channel's `say` method
 *  You can also attempt to set the topic of the Channel by invoking the
 *  `topic` method. Note you may need special privileges to set the
 *   the topic. */
trait Channel extends Named with Talkative {
  def topic(of: String): Unit
}

/** Represents the things you can do with a connection */
trait Connection {
  def write(msg: Reply*): Unit
  def close: Unit
  def cancel: Unit
  def onLine(l: String): Unit
}

case class Server(
  name: String, port: Int = 6667, ssl: Boolean = false,
  login: Option[String] = None, password: Option[String] = None,
  charset: Charset = Server.DefaultCharset, log: Logger = ConsoleLogger) {

  def connect[T](nick: String, chans: Map[String, Bot.Responder]) =
    new Connection {
      /* todo; abstract away how we are connecting */
      val conn = new Socket(name, port) {
        setSoTimeout(300000/*timeout should be configurable*/)
      }
      val out  = new BufferedWriter(
        new OutputStreamWriter(conn.getOutputStream(), charset))
      val in   = new BufferedReader(
        new InputStreamReader(conn.getInputStream(), charset))

      def close() = cancel

      def write(rs: Reply*) = rs.foreach { r =>
        out.write(r.line)
        out.newLine
        out.flush
      }

      password.map(p => write(PASSCmd(p)))

      // login
      write(NICKCmd(nick), USERCmd(login.getOrElse(nick)))

      // join channels
      write(chans.keys.toSeq.map(JOINCmd(_)): _*)

      def senderInChannel(nick: String, ch: String) =
        new Sender {
          def name = nick
          def say(what: String) = write(PRIVMSGCmd(name, what))
          def mention(what: String) = 
            write(PRIVMSGCmd(ch, "%s, %s" format(name, what)))
        }

      def newChannel(ch: String) =
        new Channel {
          def name = ch
          def topic(of: String) = write(TOPICCmd(name, of))
          def say(what: String) = write(PRIVMSGCmd(name, what))
        }

      def selfInChannel(ch: String) =
        new Self {
          def name = nick
          def part(why: Option[String] = None) =
            write(PARTCmd(ch, why))
          def quit = write(QUITCmd)
        }

      /** handle requests of (from, cmd, data) -> result */
      def command: PartialFunction[(String, String, String), Either[String, SentMessage]] = {
        case (from, "NOTICE", rest) =>
          val NoticeLine = """(.*) :(.*)""".r
          rest match {
            case NoticeLine(fst, msg) =>
              Left("got a notice %s" format msg)
            case rst =>
              Left("malformed notice line %s" format rst)
          }

        case (_, "MODE", rest) =>
          val ModeLine = """(.*) :(.*)""".r
          rest match {
            case ModeLine(who, mode) =>
              Left("mode for %s changed to %s" format(
                who, mode))
            case rst =>
              Left("malformed modeline %s" format rst)
          }

        case (from, "TOPIC", rest) =>
          val TopicLine = """(#\w+) :(.*)""".r
          rest match {
            case TopicLine(room, topic) =>
              log.debug("'%s' has set the topic of '%s' to '%s'".
                                format(from, room, topic))
              Right(Topic(
                senderInChannel(from, room),
                newChannel(room),
                topic,
                selfInChannel(room)))
            case rst =>
              Left("topic set. couldn't parse %s" format rst)
          }

        case (from, "PRIVMSG", rest) =>
          val ChannelMsg = """(#\w+) :(.*)""".r
          val PersonalMsg = """(\w+) :(.*)""".r
          rest match {
            case ChannelMsg(ch, msg) =>
              log.debug("%s said %s in %s" format(from, msg, ch))
              Right(Message(
                senderInChannel(from, ch),
                newChannel(ch),
                msg, selfInChannel(ch)))
            case PersonalMsg(who, msg) =>
              log.debug("%s sent you a private msg, %s" format(
                from, msg))
              Right(PrivMsg(
                senderInChannel(from, ""),
                msg, selfInChannel("")) /*fixme; no channel*/)
            case rst =>
              Left("malformed privmsg %s" format rst)
          }

        case (from, "INVITE", rest) =>
          log.debug("invite %s" format rest)
          val InviteMsg = """(.*) :(#.*)""".r
          rest match {
            case InviteMsg(who, ch) =>
              log.debug("you (%s) were invited to %s" format(
                who, ch
              ))
              Right(Invite(
                senderInChannel(from, ch),
                ch, selfInChannel(ch)))
            case rst =>
              Left("malformed invite %s" format rst)
          }

        case (from, "KICK", rest) =>
          val KickMsg = """(#\w+) (.*) :(.*)""".r
          rest match {
            case KickMsg(ch, who, msg) =>
              log.debug("%s was kicked from %s with msg '%s'".
                        format(
                          who, ch, msg
                        ))
            Right(Kick(
              senderInChannel(from, ch),
              newChannel(ch),
              msg, selfInChannel(ch)))
            case rst =>
              Left("kick? %s" format rest)
          }

        case (from, "PART", rest) =>
          val PartMsg = """(#\w+) :"(.*)"""".r
          rest match {
            case PartMsg(room, why) =>
              log.debug("'%s' parted from '%s' saying '%s'" format(
                from,room,why))
              Right(Part(
                senderInChannel(from, room),
                newChannel(room), Some(why),
                selfInChannel(room)))
            case room =>
              log.debug("'%s' parted from  '%s'" format(
                from, room))
              Right(Part(
                senderInChannel(from, room),
                newChannel(room), None,
                selfInChannel(room)))
          }

        case (from, "JOIN", rest) =>
          if(from.equals(nick)) Left(
            "you joined %s" format rest)
          else {
            log.debug("'%s' joined '%s'." format(from, rest))
            Right(Join(
              senderInChannel(from, rest),
              newChannel(rest),
              selfInChannel(rest)))
          }

        case (_, "QUIT", rest) =>
          Left("quit %s" format rest)

        case (_, command, rest) =>
          Left("cmd '%s'?, rest '%s'" format(
            command, rest))
      }

      def dispatch(line: String) = {
        val (sender, cmd, rest) = line.split(" ").toList match {
          case head :: cmd :: rest => (head, cmd, rest.mkString(" "))
            case _ => sys.error("unexpected msg %s" format line)
        }
        val (bang, at) = (sender.indexOf("!"), sender.indexOf("@"))
          (if(sender.startsWith(":")) {
            if(bang >0 && at > 0 && bang < at) {
              Right((sender.substring(1, bang),
                     sender.substring(bang + 1, at),
                     sender.substring(at + 1)))
            } else {
              (try { Integer.parseInt(cmd) } catch { case _ => -1 }) match {
                case code if(code == -1) =>
                  Right((sender, cmd, ""))
                case code =>
                  // bunch of 372's from the server on startup
                  // todo: deal with it
                  Left((code, cmd))
              }
            }
          } else sys.error("%s Does not compute." format line)).fold({ svr =>
            () // do we care about codes?
          }, { _ match {
            case (fromNick, fromLogin, fromHost) =>

              log.trace("\n\n%s" format line)

              log.trace("fromNick '%s' fromLogin '%s' fromHost '%s' cmd %s rest %s" format(
                fromNick, fromLogin, fromHost, cmd, rest
              ))

              command(fromNick, cmd.toUpperCase, rest).fold({ err =>
                log.debug("cmd not parsed: %s" format err)
              }, { (_: SentMessage) match {

                case msg @ (_:ChannelSentMessage) =>
                  val cname = msg.channel.name
                  chans.filter(_._1 == cname).foreach({
                    case (c, r) if(r.isDefinedAt(msg)) => r(msg)
                    case _ => ()
                  })

                case msg =>
                  chans.foreach({
                    case (c, r) if(r.isDefinedAt(msg)) => r(msg)
                    case _ => ()
                  })
              }
            })
          }
        })
      }

      def onLine(line: String) {
        if(line.startsWith("PING")) write(PONGCmd(line.substring(5)))
        else dispatch(line)
      }

      val e = Executors.newFixedThreadPool(1)
      val f = e.submit(new Callable[Unit] {
        def call: Unit = 
          while(true) {
            in.readLine match {
              case null => ()
              case line => onLine(line)
            }
          }
        })

      def cancel = {
        write(chans.keys.toSeq.map(PARTCmd(_, Some("k, bai"))):_*)
        f.cancel(true)
        e.shutdownNow
        conn.close
      }
    }
}
