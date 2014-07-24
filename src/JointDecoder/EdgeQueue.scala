package edu.cmu.lti.nlp.amr.JointDecoder
import edu.cmu.lti.nlp.amr._
import edu.cmu.lti.nlp.amr.FastFeatureVector._

import scala.collection.mutable.List
import scala.collection.mutable.Map
import scala.collection.mutable.Set

class EdgeQueue(nodes: Map[Node, Int],
                edges: Map[Edge, Int],
                eliminatedNodes: Node => Set[Node]) {

    class DoubleLinkedList[A](list) extends Iterable[LinkedNode[A]] {

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

    case class Fragment(edge: Edge,
                        totalScore: Int,
                        nodesWeighed: Int = 2) extends (Edge, Int, Int)(edge, totalScore, nodesWeighed)

    type Link = DoubleLinkedList.LinkedNode[Fragment]

    // Queue of edges sorted with highest scores on top
    var queue: DoubleLinkedList[Fragment] = {
        fragments: List[Fragment] =
            for (edge <- edges) yield Fragment(edge,
                                               edge.weight + edge.node1.weight + edge.node2.weight)
        fragments.sortWith((x, y) => x._2 > y._2)
    }

    // Map of nodes to the Edge LinkedNodes connected to them
    var node2edges: Map[Node, Set[Link]] = {
        m = (for (node <- nodes)
            yield node -> Set[Link]()))
        queue.foreach(x =>
            m[x.data.node1] += x
            m[x.data.node2] += x)
        m
    }

    def remove(elt: Link) : Fragment {
        node2edges[e.data.node1] -= elt
        node2edges[e.data.node2] -= elt
        return queue.remove(elt)
    }

    def removeAllEdges(Node node) {
        for (e <- node2edges[node]) {
            remove(e)
        }
    }

    def insert(e: Fragment,
               var after: Option[Link]) : Link = {
        var cur: Option[Link] = after
        while (cur != None) {
            elt = cur match { case Some(x) => x }
            cur = elt.next match {
                case None => None
                case Some(next) => {
                    after = cur
                    if (next.totalWeight <= e.totalWeight) {
                        None
                    } else {
                        x.next
                    }
                }
            }
        }
        return queue.insertAfter(e, after)
    }

    def adjustEdges(node: Node) : List[Fragment] {
        // Subtracts node weight from its edges
        // @return A list of fragments that can be added to graph immediately
        edges_to_confirm = new List[Edge]()

        if (node.weight == 0) {
            return
        }

        val old_edge_elts = node2edges[node]
        node2edges[Node] = new Set[Link]()
        for (elt <- old_edge_elts) {
            queue.remove(elt)
        }

        var sorted_edges: List[Fragment] = {
            for (elt <- old_edge_elts) {
                edge = elt.data
                edge.totalWeight -= node.weight
                edge.nodesWeighed--
                if (edge.nodesWeighed != 0) {
                    yield edge
                }
                else {
                    if (edge.totalWeight == 0) {
                        yield edge
                    } else if (edge.totalWeight > 0) {
                        edges_to_confirm.append(edge.edge)
                    } // else edge.totalWeight < 0; remove node
                }
            }
        }.sortWith((x, y) => x.data.totalWeight > y.data.totalWeight)

        // Re-inserting in order means re-inserting all edges is linear in
        // the length of the list
        var cur_after: Option[Link] = queue.head
        for (elt <- sorted_elts) {
            new_elt = insert(elt.data, cur_after)
            node2edges[node].add(new_elt)
            cur_after = Some(new_elt)
        }

        return edges_to_confirm
    }

    def nodeAdded(node: Node) : List[Fragment] {
        removeAll(eliminatedNodes(node)
        return adjustEdges(node)
    }

    def dequeue() : Option[Edge] = {
        return queue.head match {
            case None => None
            case Some(elt) => Some(remove(elt))
        }
    }
}
