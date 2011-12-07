package dan

object Bot {

  type Responder = PartialFunction[Message, Unit]

  case class Context(chans: Seq[String], bot: DanBot) {
    def listen(f: Responder) =
      bot.copy(channels = bot.channels ++ chans.map(_ -> f))
  }

  case class DanBot(
    name: String, server: Server = Server.default,
    channels: Map[String, Responder] = Map.empty[String, Responder]) {

    def in(chans: String*) = Context(chans, this)

    def server(name: String, port: Int = 6667) =
      this.copy(server = Server(name, port))

    def join[T](after: DanBot => T) =
      channels match {
        case cs if(!cs.isEmpty) =>
          lazy val connection =
            server.connect(
              name, cs
            )
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

  def apply(name: String) = DanBot(name)
}
