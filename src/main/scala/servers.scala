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
                  } else sys.error("%s Does not compute.")).fold({ svr =>
                    ()
                  }, { _ match {
                    case (fromNick, fromLogin, fromHost) =>
                     println("\n\n%s" format line)
                     println("fromNick '%s' fromLogin '%s' fromHost '%s' cmd %s rest %s" format(
                       fromNick, fromLogin, fromHost, cmd, rest
                     ))
                     //println("from nick %s from login %s from host %s" format(fromNick, fromLogin,fromHost))
                     cmd.toUpperCase match {
                       case "TOPIC" =>
                       // topic :dougtangren!~dougtangr@cpe-74-73-169-51.nyc.res.rr.com TOPIC #danboto :danbot!
                       case "PRIVMSG" =>
                       // chan msg :dougtangren!~dougtangr@cpe-74-73-169-51.nyc.res.rr.com PRIVMSG #danboto :hi danbot
                       // priv msg :dougtangren!~dougtangr@cpe-74-73-169-51.nyc.res.rr.com PRIVMSG danbot :hi dan
                       case "INVITE" =>
                       // invite dougtangren!~dougtangr@cpe-74-73-169-51.nyc.res.rr.com cmd INVITE DANBOT :#DANBOTO line :dougtangren!~dougtangr@cpe-74-73-169-51.nyc.res.rr.com INVITE danbot :#danboto
                       case "KICK" =>
                       // kick :dougtangren!~dougtangr@cpe-74-73-169-51.nyc.res.rr.com KICK #danboto danbot :danbot
                       case "PART" =>
                       // part :dougtangren!~dougtangr@cpe-74-73-169-51.nyc.res.rr.com PART #danboto :"l8r"
                       case "JOIN" =>
                       // join :dougtangren!~dougtangr@cpe-74-73-169-51.nyc.res.rr.com JOIN #danboto
                       case "QUITE" =>
                       // quit :dougtangren!~dougtangr@cpe-74-73-169-51.nyc.res.rr.com QUIT :

                       case command => println("sender %s cmd %s" format(sender, command))
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
