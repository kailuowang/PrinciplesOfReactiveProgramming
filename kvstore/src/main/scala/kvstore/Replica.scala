package kvstore

import akka.actor.{ OneForOneStrategy, Props, ActorRef, Actor }
import kvstore.Arbiter._
import scala.collection.immutable.Queue
import akka.actor.SupervisorStrategy.Restart
import scala.annotation.tailrec
import akka.pattern.{ ask, pipe }
import akka.actor.Terminated
import scala.concurrent.duration._
import akka.actor.PoisonPill
import akka.actor.OneForOneStrategy
import akka.actor.SupervisorStrategy
import akka.util.Timeout

object Replica {
  sealed trait Operation {
    def key: String
    def id: Long
  }
  case class Insert(key: String, value: String, id: Long) extends Operation
  case class Remove(key: String, id: Long) extends Operation
  case class Get(key: String, id: Long) extends Operation

  sealed trait OperationReply
  case class OperationAck(id: Long) extends OperationReply
  case class OperationFailed(id: Long) extends OperationReply
  case class GetResult(key: String, valueOption: Option[String], id: Long) extends OperationReply

  def props(arbiter: ActorRef, persistenceProps: Props): Props = Props(new Replica(arbiter, persistenceProps))
}

class Replica(val arbiter: ActorRef, persistenceProps: Props) extends Actor {
  import Replica._
  import Replicator._
  import Persistence._
  import context.dispatcher



  var kv = Map.empty[String, String]
  var expectedSeq = 0L

  var secondaries = Map.empty[ActorRef, ActorRef]

  var replicators = Set.empty[ActorRef]

  def receive = {
    case JoinedPrimary   => context.become(leader)
    case JoinedSecondary => context.become(replica)
  }

  val leader: Receive = {
    case Insert(key, value, id) => {
      kv += key -> value
      sender ! OperationAck(id)
    }
    case Remove(key, id) => {
      kv -= key
      sender ! OperationAck(id)
    }
    case Get(key, id) => {
      sender ! GetResult(key, kv.get(key), id)
    }
  }

  val replica: Receive = {
    case Snapshot(key, value, seq) => {
      if(seq == expectedSeq)
        update(key, value)
      if(seq <= expectedSeq){
        expectedSeq = Math.max(seq + 1, expectedSeq)
        sender ! SnapshotAck(key, seq)
      }
    }
    case Get(key, id) => {
      sender ! GetResult(key, kv.get(key), id)
    }
  }

  def update(key: String, value: Option[String]) {
    if(value.isDefined)
      kv += key -> value.get
    else
      kv -= key
  }

  override def preStart() : Unit= {
    arbiter ! Join
  }

}
