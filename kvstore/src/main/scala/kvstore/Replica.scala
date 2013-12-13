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
import scala.language.postfixOps

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
  case class CheckPersistent(id: Long)
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

  var persistent: ActorRef = context.actorOf(persistenceProps)

  var persistenceAcks = Map.empty[Long, (ActorRef, Persist)] //id -> (requester, persist)

  def receive = {
    case JoinedPrimary   => context.become(leader)
    case JoinedSecondary => context.become(replica)
  }

  val leader: Receive = {
    case Insert(key, value, id) => {
      kv += key -> value
      persist(key, Some(value), id)
    }
    case Remove(key, id) => {
      kv -= key
      persist(key, None, id)
    }
    case Get(key, id) => {
      sender ! GetResult(key, kv.get(key), id)
    }

    case Persisted(key, id) => {
      persistenceAcks(id)._1 ! OperationAck(id)
      persistenceAcks -= id
    }

    case CheckPersistent(id) => {
      persistenceAcks.get(id).foreach {
        case (requester, p) => requester ! OperationFailed(id)
      }
    }
  }

  val replica: Receive = {

    case Snapshot(key, valueOption, seq) => {
      if(seq == expectedSeq){
        expectedSeq += 1
        update(key, valueOption)
        persist(key, valueOption, seq)
      } else if (seq < expectedSeq)
        sender ! SnapshotAck(key, seq)
    }

    case Get(key, id) => {
      sender ! GetResult(key, kv.get(key), id)
    }

    case Persisted(key, id) => {
      persistenceAcks(id)._1 ! SnapshotAck(key, id)
      persistenceAcks -= id
    }

  }

  def persist( key: String, valueOption: Option[String], id: Long) {
    val p = Persist(key, valueOption, id)
    persistenceAcks += id -> (sender, p)
    persistent ! p
    context.system.scheduler.scheduleOnce(1 second, self, CheckPersistent(id))
  }

  def update(key: String, valueOption: Option[String]) {
    if(valueOption.isDefined)
      kv += key -> valueOption.get
    else
      kv -= key
  }

  def repersist():Unit = {
    persistenceAcks.foreach {
      case (id, (_, p)) => persistent ! p
    }
  }

  override def preStart() : Unit= {
    arbiter ! Join
    context.system.scheduler.schedule(0 milliseconds, 100 milliseconds)(repersist)

  }

}
