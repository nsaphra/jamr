package edu.cmu.lti.nlp.amr.JointDecoder
import edu.cmu.lti.nlp.amr._
import edu.cmu.lti.nlp.amr.FastFeatureVector._

import scala.collection.mutable.List
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.util.Sorting.stableSort

class EdgeQueue(nodes: Map[Node, Int],
                edges: Map[Edge, Int],
                eliminatedNodes: Node => Set[Node]) {

    case class Fragment(edge: Edge,
                        totalScore: Int,
                        nodesWeighed: Int = 2) extends (Edge, Int, Int)(edge, totalScore, nodesWeighed)

    type Link = DoubleLinkedList.LinkedNode[Fragment]

    // Queue of edges sorted with highest scores on top
    var queue = new SortedDoubleLinkedList(
        for (edge <- edges) yield
            Fragment(edge, edge.weight + edge.node1.weight + edge.node2.weight),
        (x: Fragment) => x.totalWeight
    )

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

    def adjustEdges(node: Node) : List[Fragment] {
        // Subtracts node weight from its edges
        // @return A list of fragments that can be added to graph immediately
        edges_to_confirm = new List[Edge]()

        if (node.weight == 0) {
            return
        }

        val old_elts = node2edges[node]
        node2edges[Node] = new Set[Link]()
        for (elt <- old_elts) {
            queue.remove(elt)
        }

        var new_frags: List[Fragment] = {
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
        }

        // Re-inserting in order means re-inserting all edges is linear in
        // the length of the list
        for (elt <- queue.insertAll(new_frags)) {
            node2edges[node].add(new_elt)
        }

        return edges_to_confirm
    }

    def nodeAdded(node: Node) : List[Fragment] {
        for (enemy <- eliminatedNodes(node)) {
            removeAllEdges(enemy)
        }
        return adjustEdges(node)
    }

    def dequeue() : Option[Edge] = {
        return queue.head match {
            case None => None
            case Some(elt) => Some(remove(elt))
        }
    }
}
