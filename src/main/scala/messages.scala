package dan

object Messages {
  def apply(protocol: String): Message = Raw(protocol)
}

trait Message {
  def text: String
}

case class Raw(text: String) extends Message
