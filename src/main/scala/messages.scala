package dan

object Messages {

}

trait Message {
  def text: String
  def sender: Sender
}
