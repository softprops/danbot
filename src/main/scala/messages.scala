package dan

sealed trait SentMessage {
  def text: String
  def sender: Sender
  def self: Self
}

trait ChannelSentMessage extends SentMessage {
  def channel: Channel
}

// types to patten match on

case class PrivMsg(sender: Sender, text: String, self: Self)
     extends SentMessage

case class Invite(sender: Sender, channel: String, self: Self)
     extends SentMessage {
  def text = ""
}

//case class Notice

//case class Mode

case class Topic(sender: Sender, channel: Channel, text: String, self: Self)
     extends ChannelSentMessage

case class Message(sender: Sender, channel: Channel, text: String, self: Self)
     extends ChannelSentMessage

case class Kick(sender: Sender, channel: Channel, text: String, self: Self)
     extends ChannelSentMessage

case class Part(sender: Sender, channel: Channel, msg: Option[String], self: Self)
     extends ChannelSentMessage {
  def text = msg.getOrElse("")
}

case class Join(sender: Sender, channel: Channel, self: Self)
     extends ChannelSentMessage {
  def text = ""
}
