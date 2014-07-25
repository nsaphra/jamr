package edu.cmu.lti.nlp.amr.JointDecoder
import edu.cmu.lti.nlp.amr._

import scala.collection.mutable.List

class DoubleLinkedList[A](list: List[A]) extends Iterable[LinkedNode[A]] {

    class LinkedNode[A](data: A, var next: Option[LinkedNode[A]]) {

        var prev: Option[LinkedNode[A]] = next match {
            case None => None
            case Some(n) => {
                prev_node = n.prev
                n.prev = this
                prev_node match {
                    case None => None
                    case Some(p) => {
                        p.next = this
                        Some(p)
                    }
                }
            }
        }
    }

    val tail: Option[LinkedNode[A]] = None
    var head = list.foldLeft(tail) {
        (z, f) => LinkedNode[A](f, z)
    }

    def prepend(data: A) : LinkedNode[A] = {
        head = LinkedNode[A](data, head)
        return head
    }

    def insert(data: A, node: Option[LinkedNode[A]]) : LinkedNode[A] = node match {
        case None => prepend(A)
        case Some(n) => {
            LinkedNode[A](data, n.next)
        }
    }

    def remove(node: LinkedNode[A]) : A {
        node.prev match {
            case None => { head = node.next }
            case Some(x) => { x.next = node.next }
        }
        node.next match {
            case Some(x) => { x.prev = node.prev }
        }

        return node.data
    }

    def foreach[U](f: LinkedNode[A] => U) {
        var node: Option[LinkedNode[A]] = head
        while (node != None) {
            node match {
                case Some(x) => f(x)
            }
            node = node.next
        }
    }

    def dequeue() : Option[LinkedNode[A]] = {
        return head match {
            case None => None
            case Some(x) => {
                remove(x)
                Some(x)
            }
        }
    }
}
