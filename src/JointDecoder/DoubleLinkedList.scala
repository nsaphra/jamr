package edu.cmu.lti.nlp.amr.JointDecoder
import edu.cmu.lti.nlp.amr._

class LinkedNode[A](val data: A, var next: Option[LinkedNode[A]]) {

    var prev: Option[LinkedNode[A]] = next match {
        case Some(n) => {
            val prev_node = n.prev
            n.prev = Some(this)
            prev_node match {
                case None => None
                case Some(p) => {
                    p.next = Some(this)
                    Some(p)
                }
            }
        }
        case None => None
    }
}

class DoubleLinkedList[A](list: List[A]) extends Iterable[LinkedNode[A]] {

    var top: Option[LinkedNode[A]] = list.foldLeft(None.asInstanceOf[Option[LinkedNode[A]]]) {
        (z, f) => Some(new LinkedNode[A](f, z))
    }

    def prepend(data: A) : LinkedNode[A] = {
        top = Some(new LinkedNode[A](data, top))
        return top.get
    }

    def insert(data: A, node: Option[LinkedNode[A]]) : LinkedNode[A] = node match {
        case Some(n) =>
            new LinkedNode[A](data, n.next)
        case None =>
            prepend(data)
    }

    def remove(node: LinkedNode[A]) : A = {
        node.prev match {
            case Some(x) => { x.next = node.next }
            case None => { top = node.next }
        }
        if (node.next.isDefined) {
            node.next.get.prev = node.prev
        }

        return node.data
    }

    def iterator = new Iterator[LinkedNode[A]] {
        var nextLink: Option[LinkedNode[A]] = top

        def hasNext = nextLink.isDefined

        def next: LinkedNode[A] = {
            nextLink match {
                case Some(x) => {
                    nextLink = x.next
                    x
                }
                case None => null
            }
        }
    }

    def dequeue() : Option[LinkedNode[A]] = {
        return top match {
            case Some(x) => {
                remove(x)
                Some(x)
            }
            case None => None
        }
    }
}
