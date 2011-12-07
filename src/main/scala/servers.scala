package dan

import java.nio.charset.Charset
import java.io._
import java.net.{ InetAddress, Socket, SocketException }
import java.util.Random
import java.util.concurrent.{ Callable, Executors, Future }

object Server {
  val DefaultCharset = Charset.forName("utf8")
  def default = Server("chat.freenode.net", 6667)
}

trait Sender {
  def name: String
  def say(r: Reply): Unit
}

case class Server(
  name: String, port: Int = 6667, ssl: Boolean = false,
  login: Option[String] = None, password: Option[String] = None,
  charset: Charset = Server.DefaultCharset) {

  def connect[T](nick: String, chans: Map[String, Bot.Responder]) =
    new Connection {
      val conn = new Socket(name, port) {
        setSoTimeout(300000)
      }
      val out  = new BufferedWriter(new OutputStreamWriter(conn.getOutputStream(), charset))
      val in   = new BufferedReader(new InputStreamReader(conn.getInputStream(), charset))

      def close() = ()

      def write(msgs: Reply*) = msgs.foreach { m =>
        out.write(m.line)
        out.newLine
        out.flush
      }

      password.map(p => write(PASS(p)))

      write(
        NICK(nick),
        USER(login.getOrElse(nick))
      )
      write(chans.keys.toSeq.map(JOIN(_)): _*)

      val e = Executors.newFixedThreadPool(1)
      val f = e.submit(new Callable[Unit] {
        def call: Unit = 
          while(true) {
            in.readLine match {
              case null => ()
              case line =>
                if(line.startsWith("PING")) write(PONG(line.substring(5)))
                else {
                  val (sender, cmd, rest) = line.split(" ").toList match {
                    case head :: cmd :: rest => (head, cmd, rest.mkString(" "))
                    case _ => sys.error("unexpected msg %s" format line)
                  }
                  val (exclaim, at) = (sender.indexOf("!"), sender.indexOf("@"))
                  (if(sender.startsWith(":")) {
                    if(exclaim >0 && at > 0 && exclaim < at) {
                      Right((sender.substring(1, exclaim),
                        sender.substring(exclaim + 1, at),
                        sender.substring(at + 1)))
                     } else {
                       (try { Integer.parseInt(cmd) } catch { case _ => -1 }) match {
                         case n if(n == -1) =>
                           Right((sender, cmd, ""))
                         case n =>
                           // bunch of 372's from the server on startup
                           // todo: deal with it
                           Left((n, cmd))
                       }
                     }
                  } else sys.error("%s Does not compute." format line)).fold({ svr =>
                    ()
                  }, { _ match {
                    case (fromNick, fromLogin, fromHost) =>
                     println("\n\n%s" format line)
                     println("fromNick '%s' fromLogin '%s' fromHost '%s' cmd %s rest %s" format(
                       fromNick, fromLogin, fromHost, cmd, rest
                     ))
                     cmd.toUpperCase match {
                       case "TOPIC" =>
                         println("topic %s" format rest)
                       case "PRIVMSG" =>
                         val Room = """(#\w+) :(.*)""".r
                         val Msg = """(\w+) :(.*)""".r
                         rest match {
                           case Room(ch, msg) =>
                             println("%s said %s in %s" format(fromNick, msg, ch))
                           case Msg(who, msg) =>
                             println("%s sent you a private msg, %s" format(fromNick, msg))
                         }
                       case "INVITE" =>
                         println("invite %s" format rest)
                       // invite dougtangren!~dougtangr@cpe-74-73-169-51.nyc.res.rr.com cmd INVITE DANBOT :#DANBOTO line :dougtangren!~dougtangr@cpe-74-73-169-51.nyc.res.rr.com INVITE danbot :#danboto
                       case "KICK" =>
                         println("kick %s" format rest)
                       // kick :dougtangren!~dougtangr@cpe-74-73-169-51.nyc.res.rr.com KICK #danboto danbot :danbot
                       case "PART" =>
                         println("%s parted from %s" format(fromNick,rest))
                       // part :dougtangren!~dougtangr@cpe-74-73-169-51.nyc.res.rr.com PART #danboto :"l8r"
                       case "JOIN" =>
                         if(fromNick.equals(name)) println("you joined %s" format rest)
                         else println("%s joined %s" format(fromNick, rest))
                       // join :dougtangren!~dougtangr@cpe-74-73-169-51.nyc.res.rr.com JOIN #danboto
                       case "QUIT" =>
                         println("quit %s" format rest)
                       // quit :dougtangren!~dougtangr@cpe-74-73-169-51.nyc.res.rr.com QUIT :
                       case command => println("cmd %s, rest %s" format(command, rest))
                     }
                 }
               })
            }
          }
        }})

      def cancel = {
        write(chans.keys.toSeq.map(PART(_, Some("k, bai"))):_*)
        f.cancel(true)
        e.shutdownNow
        conn.close
      }

    }
}

trait Connection {
  def write(msg: Reply*): Unit
  def close: Unit
  def cancel: Unit
}
