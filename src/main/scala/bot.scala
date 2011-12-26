package dan

object Bot {

  // todo: make this chainable
  type Responder = PartialFunction[SentMessage, Unit]

  case class FilteredResponder(name: String, ifmentioned: Responder) {
    def apply: Responder = {
      case m@(_:SentMessage) if(m.text.contains(name) && ifmentioned.isDefinedAt(m)) =>
        ifmentioned(m)
      case _ => () 
    }
  }

  /** Provides some context for listening and responding to messages */
  case class Context(chans: Seq[String], bot: DanBot) {
    /** respond when any message is received */
    def listen(f: Responder) =
       bot.copy(channels = bot.channels ++ chans.map(_ -> f))
    /** respond only when this bot's name is mentioned */
    def mentions(f: Responder) =
      bot.copy(channels = bot.channels ++ chans.map(_ -> FilteredResponder(bot.name, f).apply))
  }

  /** A soft-spoken but well-intentioned robot just looking for a friend */
  case class DanBot(
    name: String,
    server: Server = Server.default,
    channels: Map[String, Responder] = Map.empty[String, Responder]) {

    /** Specifies which context to listen for messages */
    def in(chans: String*) = Context(chans, this)

    /** Specificies which server to connect to. Defaults to `freenode` */
    def server(name: String, port: Int = 6667, log: Logger = ConsoleLogger) =
      this.copy(server = Server(name, port))

    /** Call this method after configuring danbot to join the specified channels */
    def join[T](after: DanBot => T = { d: DanBot => println("beep boop. powering down."); }) =
      channels match {
        case cs if(!cs.isEmpty) =>
          lazy val connection = server.connect(name, cs)
          Thread.currentThread.getName() match {
            case "main" => 
              connection
            case _ =>
              connection
              def awaitInput {
                try { Thread.sleep(1000) } catch { case _: InterruptedException => () }
                if(System.in.available() <= 0) awaitInput
              }
              awaitInput
              connection.cancel
          }
          after(this)
        case _ =>
          println("no channels? this does not compute")
          after(this)
      }
  }

  /** Start by giving Dan a name */
  def apply(name: String) = DanBot(name)
}
