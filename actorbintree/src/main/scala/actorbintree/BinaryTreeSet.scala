/**
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package actorbintree

import akka.actor._
import scala.collection.immutable.Queue
import actorbintree.BinaryTreeNode.CopyFinished

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

}


class BinaryTreeSet extends Actor {
  import BinaryTreeSet._
  import BinaryTreeNode._

  def createRoot: ActorRef = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true))

  var root = createRoot

  var queue = Queue.empty[Operation]

  def receive = normal

  /** Accepts `Operation` and `GC` messages. */
  val normal: Receive = {
    case op : Operation => run(op)
    case GC => startGC
  }

  val running: Receive = enQueue orElse {
    case or: OperationReply => runNext

    case GC => context become scheduleGC
  }

  val scheduleGC: Receive = enQueue orElse {
    case or: OperationReply => startGC
  }


  def garbageCollecting(newRoot: ActorRef): Receive = enQueue orElse {
    case CopyFinished => {
      root = newRoot
      runNext
    }
  }

  def enQueue: Receive = {
    case op : Operation => queue = queue.enqueue(op)
  }

  def runNext {
    if(queue.isEmpty)
      context.become(normal)
    else {
      val op = queue.head
      queue = queue.tail
      run(op)
    }
  }

  def run(op: Operation) {
    context become running
    root ! op
  }

  def startGC {
    val newRoot = createRoot
    context.become(garbageCollecting(newRoot))
    root ! CopyTo(newRoot)
  }

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
      if(op.elem < elem && subtrees.contains(Left))
        subtrees(Left) forward op
      else if (op.elem > elem && subtrees.contains(Right))
        subtrees(Right) forward op
      else
        perform(op)
    }

    case CopyTo(treeNode) => {
      checkFinish(subtrees.values.toSet, removed)

      if(!removed)
        treeNode ! Insert(self, elem, elem ) //use elem as insert operation id

      subtrees.values.foreach(_ ! CopyTo(treeNode))
    }

    case of: OperationReply => {
      context become normal
      context.parent forward of
    }

  }


  def insert(position: Position, newElem: Int, id: Int) {
    subtrees += position -> context.actorOf(BinaryTreeNode.props(newElem))
  }

  def perform(op: Operation) {
    val reply = op match {
      case Insert(_, id, newElem) => {
        if (newElem < elem) insert(Left, newElem, id)
        else if (newElem > elem) insert(Right, newElem, id)
        else removed = false
        OperationFinished(id)
      }
      case Contains(_, id, testElem) =>
        ContainsResult(id, !removed && testElem == elem)
      case Remove(_, id, toRemove) =>  {
        if (toRemove == elem) removed = true
        OperationFinished(id)
      }
    }
    op.requester ! reply
    self ! reply
  }

  // optional
  /** `expected` is the set of ActorRefs whose replies we are waiting for,
    * `insertConfirmed` tracks whether the copy of this node to the new tree has been confirmed.
    */
  def copying(expected: Set[ActorRef], insertConfirmed: Boolean): Receive = {
    case CopyFinished => {
      val rest = expected - sender
      checkFinish(rest, insertConfirmed)
    }

    case of: OperationFinished => checkFinish(expected, true)

  }

  def checkFinish(expected: Set[ActorRef], insertConfirmed: Boolean) = {

    if(expected.isEmpty && insertConfirmed) {
      context.parent ! CopyFinished
      context.stop(self)
    }else {
      context become copying(expected, insertConfirmed)
    }
  }

}
