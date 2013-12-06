/**
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package actorbintree

import akka.actor._
import scala.collection.immutable.Queue

object BinaryTreeSet {

  trait Operation {
    def requester: ActorRef
    def id: Int
    def elem: Int
  }

  trait OperationReply {
    def id: Int
  }

  /** Request with identifier `id` to insert an element `elem` into the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Insert(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to check whether an element `elem` is present
    * in the tree. The actor at reference `requester` should be notified when
    * this operation is completed.
    */
  case class Contains(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to remove the element `elem` from the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Remove(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request to perform garbage collection*/
  case object GC

  /** Holds the answer to the Contains request with identifier `id`.
    * `result` is true if and only if the element is present in the tree.
    */
  case class ContainsResult(id: Int, result: Boolean) extends OperationReply

  /** Message to signal successful completion of an insert or remove operation. */
  case class OperationFinished(id: Int) extends OperationReply

  /** Message to signal successful completion of an insert or remove operation. */
  case class OperationFailed(id: Int) extends OperationReply

}


class BinaryTreeSet extends Actor {
  import BinaryTreeSet._
  import BinaryTreeNode._

  def createRoot: ActorRef = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true))

  var root = createRoot

  // optional
  def receive = normal

  // optional
  /** Accepts `Operation` and `GC` messages. */
  val normal: Receive = {
    case op : Operation => runNext(Queue(op))
    case GC => context.become(garbageCollecting(createRoot))
  }

  def running(queue: Queue[Operation]): Receive = {
    case or: OperationReply =>
      queue.dequeue match {
        case (op, pq) => {
          if(pq.isEmpty)
            context.become(normal)
          else
            runNext(pq)
        }
      }
    case op : Operation => context.become(running(queue.enqueue(op)))
  }

  def runNext(queue: Queue[Operation]) {
    context become running(queue)
    root ! queue.head
  }



  // optional
  /** Handles messages while garbage collection is performed.
    * `newRoot` is the root of the new binary tree where we want to copy
    * all non-removed elements into.
    */
  def garbageCollecting(newRoot: ActorRef): Receive = ???

}

object BinaryTreeNode {
  trait Position

  case object Left extends Position
  case object Right extends Position

  case class CopyTo(treeNode: ActorRef)
  case object CopyFinished

  def props(elem: Int, initiallyRemoved: Boolean = false) = Props(classOf[BinaryTreeNode],  elem, initiallyRemoved)
}

class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {
  import BinaryTreeNode._
  import BinaryTreeSet._

  var subtrees = Map[Position, ActorRef]()
  var removed = initiallyRemoved

  def receive = normal

  /** Handles `Operation` messages and `CopyTo` requests. */
  val normal: Receive = {
    case op: Operation => {
      context.become(performing)

      if(op.elem < elem && subtrees.contains(Left))
        subtrees(Left) forward op
      else if (op.elem > elem && subtrees.contains(Right))
        subtrees(Right) forward op
      else
        perform(op)
    }

    case CopyTo(treeNode) => ???

  }

  val performing: Receive = {
    case of: OperationReply => {
      context.parent forward of
      context become normal
    }
  }

  def insert(position: Position, newElem: Int, id: Int) = {
    subtrees += position -> context.actorOf(BinaryTreeNode.props(newElem))
    OperationFinished(id)
  }

  def perform(op: Operation) {
    val reply = op match {
      case Insert(_, id, newElem) => {
        if (newElem < elem) insert(Left, newElem, id)
        else if (newElem > elem) insert(Right, newElem, id)
        else OperationFailed(id)
      }
      case Contains(_, id, testElem) =>
        ContainsResult(id, !removed && testElem == elem)
      case Remove(_, id, toRemove) =>
        if (toRemove == elem && !removed) {
          removed = true
          OperationFinished(id)
        } else OperationFailed(id)
    }
    op.requester ! reply
    self ! reply
  }

  // optional
  /** `expected` is the set of ActorRefs whose replies we are waiting for,
    * `insertConfirmed` tracks whether the copy of this node to the new tree has been confirmed.
    */
  def copying(expected: Set[ActorRef], insertConfirmed: Boolean): Receive = ???

}
