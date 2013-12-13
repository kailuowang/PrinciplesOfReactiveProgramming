package kvstore

import akka.actor.Props
import akka.actor.Actor
import akka.actor.ActorRef
import scala.concurrent.duration._
import scala.language.postfixOps

object Replicator {
  case class Replicate(key: String, valueOption: Option[String], id: Long)
  case class Replicated(key: String, id: Long)

  case class Snapshot(key: String, valueOption: Option[String], seq: Long)
  case class SnapshotAck(key: String, seq: Long)

  def props(replica: ActorRef): Props = Props(new Replicator(replica))
}

class Replicator(val replica: ActorRef) extends Actor {
  import Replicator._
  import Replica._
  import context.dispatcher

  // map from sequence number to pair of sender and request
  var acks = Map.empty[Long, (ActorRef, Replicate)]

  // a sequence of not-yet-sent snapshots (you can disregard this if not implementing batching)
  var pending = Vector.empty[Snapshot]

  var _seqCounter = 0L
  def nextSeq = {
    val ret = _seqCounter
    _seqCounter += 1
    ret
  }


  def receive: Receive = {
    case r @ Replicate(key, valueOption, id) => {
      val seq = nextSeq
      acks += seq -> (sender, r)
      send(r, seq)
    }
    case SnapshotAck(key, seq) => {
      val (requester, r) = acks(seq)
      requester ! Replicated(key, r.id)
      acks -= seq
    }
  }

  def send(r: Replicate, seq: Long): Unit = {
    replica ! Snapshot(r.key, r.valueOption, seq)
  }

  def resend(): Unit = {
    acks.foreach {
      case (seq, (_, r)) => send(r, seq)
    }
  }

  override def preStart():Unit = {
    context.system.scheduler.schedule(0 milliseconds, 100 milliseconds)(resend)
  }

}
