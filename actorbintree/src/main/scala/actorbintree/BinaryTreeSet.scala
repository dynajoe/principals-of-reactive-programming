/**
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package actorbintree

import actorbintree.BinaryTreeNode.CopyFinished
import actorbintree.BinaryTreeSet
import akka.actor._
import akka.event.LoggingReceive
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

}

class BinaryTreeSet extends Actor with Stash {
  import BinaryTreeSet._
  import BinaryTreeNode._

  def createRoot: ActorRef = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true))

  var root = createRoot

  // optional
  def receive = normal

  // optional
  /** Accepts `Operation` and `GC` messages. */
  val normal: Receive =  {
    case op: Operation => {
      root.forward(op)
    }
    case GC => {
      val newRoot = createRoot
      root ! CopyTo(newRoot)
      context.become(garbageCollecting(newRoot))
    }
  }

  // optional
  /** Handles messages while garbage collection is performed.
    * `newRoot` is the root of the new binary tree where we want to copy
    * all non-removed elements into.
    */
  def garbageCollecting(newRoot: ActorRef): Receive = {
    case GC => ()
    case CopyFinished => {
      root ! PoisonPill
      root = newRoot
      unstashAll()
      context.unbecome()
    }
    case _ => stash()
  }
}

object BinaryTreeNode {
  trait Position

  case object Left extends Position
  case object Right extends Position

  case class CopyTo(treeNode: ActorRef)
  case object CopyFinished

  def props(elem: Int, initiallyRemoved: Boolean) = Props(classOf[BinaryTreeNode],  elem, initiallyRemoved)
}

class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {
  import BinaryTreeNode._
  import BinaryTreeSet._

  var subtrees = Map[Position, ActorRef]()
  var removed = initiallyRemoved

  // optional
  def receive = normal

  def copyComplete() = {
    context.unbecome()
    context.parent ! CopyFinished
    subtrees.values.map(_ ! PoisonPill)
  }

  // optional
  /** Handles `Operation` messages and `CopyTo` requests. */
  val normal: Receive = {
    case CopyTo(newRoot) => {
      if (removed && subtrees.values.isEmpty) {
        context.parent ! CopyFinished
      }
      else {
        if (!removed) {
          newRoot ! Insert(self, -1, elem)
        }

        subtrees.values.foreach(_ ! CopyTo(newRoot))
        context.become(copying(subtrees.values.toSet, removed))
      }
    }
    case Insert(requester, id, value) => {
      if (value == elem) {
        removed = false
        requester ! OperationFinished(id)
      }
      else {
        val pos = position(value)

        if (subtrees.contains(pos)) {
          subtrees(pos) ! Insert(requester, id, value)
        }
        else {
          subtrees += pos -> context.actorOf(props(value, false))
          requester ! OperationFinished(id)
        }
      }
    }
    case Remove(requester, id, value) => {
      if (value == elem) {
        removed = true
        requester ! OperationFinished(id)
      }
      else {
        val pos = position(value)

        if (subtrees.contains(pos)) {
          subtrees(pos) ! Remove(requester, id, value)
        }
        else {
          requester ! OperationFinished(id)
        }
      }
    }
    case Contains(requester, id, value) ⇒ {
      if (value == elem) {
        requester ! ContainsResult(id, !removed)
      }
      else {
        val pos = position(value)
        if (subtrees.contains(pos)) {
          subtrees(pos) ! Contains(requester, id, value)
        }
        else {
          requester ! ContainsResult(id, false)
        }
      }
    }
  }

  def position(v: Int) = {
    if (v < elem) Left else Right
  }

  // optional

  def checkComplete(expected: Set[ActorRef], insertConfirmed: Boolean) = {
    if (expected.isEmpty && insertConfirmed) {
      context.parent ! CopyFinished
      subtrees.values.foreach(_ ! PoisonPill)
    }
    else {
      context.become(copying(expected, true))
    }
  }

  /** `expected` is the set of ActorRefs whose replies we are waiting for,
    * `insertConfirmed` tracks whether the copy of this node to the new tree has been confirmed.
    */
  def copying(expected: Set[ActorRef], insertConfirmed: Boolean): Receive = {
    case OperationFinished(-1) => {
      checkComplete(expected, true)
    }
    case CopyFinished => {
      checkComplete(expected - sender, insertConfirmed)
    }
  }

}
