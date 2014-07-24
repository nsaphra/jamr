package edu.cmu.lti.nlp.amr.JointDecoder
import edu.cmu.lti.nlp.amr._
import edu.cmu.lti.nlp.amr.FastFeatureVector._

import scala.collection.mutable.List
import scala.collection.mutable.Map
import scala.collection.mutable.Set

class EdgeQueue(nodes: Map[Node, Int],
                edges: Map[Edge, Int],
                eliminatedNodes: Map[Node, Set[Node]]) {

    class DoubleLinkedList[A](list) extends Traversable[LinkedNode[A]] {

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
        var head = list.foldRight(tail) { (z, f) => LinkedNode[A](f, z) }

        def prepend(data: A) : LinkedNode[A] = {
            head = LinkedNode[A](data, head)
            return head
        }

        def insertAfter(data: A, node: Option[LinkedNode[A]]) : LinkedNode[A] = node match {
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

    // Queue of edges sorted with highest scores on top
    var queue = new DoubleLinkedList[Edge](edges.sortWith((x, y) => x._2 > y._2))

    // Map of nodes to the Edge LinkedNodes connected to them
    var node2edges: Map[Node, Set[LinkedNode[Edge]]] = {
        m = new Map((for (node <- nodes)
                     yield node -> Set[LinkedNode[Edge]]()))
        queue.foreach(x =>
            m[x.data.node1] += x
            m[x.data.node2] += x)
        m
    }

    def remove(elt: DoubleLinkedList.LinkedNode[Edge]) : Edge {
        node2edges[e.data.node1] -= elt
        node2edges[e.data.node2] -= elt
        return queue.remove(elt)
    }

    def removeAllEdges(Node node) {
        for (e <- node2edges[node]) {
            remove(e)
        }
    }

    def insert(e: Edge,
               var after: Option[DoubleLinkedList.LinkedNode[Edge]]) :
        DoubleLinkedList.LinkedNode[Edge] = {
        var cur: Option[DoubleLinkedList.LinkedNode[Edge]] = after
        while (cur != None) {
            elt = cur match { case Some(x) => x }
            cur = elt.next match {
                case None => None
                case Some(next) => {
                    after = cur
                    if (next.weight <= e.weight) {
                        None
                    } else {
                        x.next
                    }
                }
            }
        }
        return queue.insertAfter(e, after)
    }

    def adjustEdges(node: Node) {
        // Subtracts node weight from its edges

        if (node.weight == 0) {
            return
        }

        val old_edge_elts: Set[LinkedNode[Edge]] = node2edges[node]
        node2edges[Node] = new Set[Edge]()
        for (elt <- old_edge_elts) {
            queue.remove(elt)
        }

        var sorted_edges: List[Edge] = {
            for (elt <- old_edge_elts) {
                edge = elt.data
                edge.weight -= node.weight
                yield edge
            }
        }.sortWith((x, y) => x.data.weight > y.data.weight)

        // Re-inserting in order means re-inserting all edges is linear in
        // the length of the list
        var cur_after: Option[DoubleLinkedList.LinkedNode[Edge]] = queue.head
        for (elt <- sorted_elts) {
            new_elt = insert(elt.data, cur_after)
            node2edges[node].add(new_elt)
            cur_after = Some(new_elt)
        }
    }

    def nodeAdded(node: Node) {
        removeAll(eliminatedNodes[node])
        adjustEdges(node)
    }

    def dequeue() : Option[Edge] = {
        return queue.head match {
            case None => None
            case Some(elt) => Some(remove(elt))
        }
    }
}
