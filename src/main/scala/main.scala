object Main {
  import dan._
  def main(args: Array[String]) {
    dan.Bot("danbot").in("#danboto").mentions {
      case Message(sender, chan, txt, _) =>
        chan.say("sure %s" format sender.name)
    }.join()
  }
}
