package edu.cmu.lti.nlp.amr.JointDecoder
import edu.cmu.lti.nlp.amr._
import edu.cmu.lti.nlp.amr.FastFeatureVector._

import scala.collection.mutable.List
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.util.Sorting.stableSort

class EdgeQueue(nodeWeights: Array[Double],
                edgeWeights: Array[Array[Double]],
                eliminatedNodes: Int => Set[Int]) {

    case class Fragment(edge: Edge,
                        totalScore: Int,
                        nodesWeighed: Int = 2) extends (Edge, Int, Int)(edge, totalScore, nodesWeighed)

    type Link = DoubleLinkedList.LinkedNode[Fragment]

    // Queue of edges sorted with highest scores on top
    var queue = new SortedDoubleLinkedList(
        for (edgeweights.zipWithIndex.zipWithIndex for (edge <- edges) yield
            Fragment(edge, edge.weight + nodeWeights[edge._1] + nodeWeights[edge._2]),
        (x: Fragment) => x.totalWeight
    )

    // Map of nodes to the Edge LinkedNodes connected to them
    var node2links: Map[Node, Set[Link]] = {
        m = nodes.zipWithIndex foreach {
            case (node, w) => yield (node -> Set[Link]())
        }
        queue foreach { link => {
            m[link.data.edge._1] += link
            m[link.data.edge._2] += link
        } }
        m
    }

    def remove(link: Link) : Fragment {
        node2links[link.data.edge._1] -= link
        node2links[link.data.edge._2] -= link
        return queue.remove(link)
    }

    def removeAllEdges(Node node) {
        for (link <- node2links[node]) {
            remove(link)
        }
    }

    def adjustFragmentWeights(node: Node) : List[Fragment] {
        // Subtracts node weight from its edges
        // @return A list of fragments that can be added to graph immediately
        edges_to_confirm = new List[Edge]()

        if (node.weight == 0) {
            return
        }

        val old_links = node2links[node]
        node2links[Node] = new Set[Link]()
        for (link <- old_links) {
            queue.remove(link)
        }

        var new_frags: List[Fragment] = {
            for (link <- old_links) {
                frag = link.data
                frag.totalWeight -= node.weight
                frag.nodesWeighed--
                if (frag.nodesWeighed != 0) {
                    yield frag
                }
                else {
                    if (frag.totalWeight == 0) {
                        yield frag
                    } else if (frag.totalWeight > 0) {
                        edges_to_confirm.append(frag.edge)
                    } // else frag.totalWeight < 0; remove node
                }
            }
        }

        // Re-inserting in order means re-inserting all edges is linear in
        // the length of the list
        for (link <- queue.insertAll(new_frags)) {
            node2links[node] += link
        }

        return edges_to_confirm
    }

    def nodeAdded(node: Node) : List[Fragment] {
        for (enemy <- eliminatedNodes(node)) {
            removeAllEdges(enemy)
        }
        return adjustFragmentWeights(node)
    }

    def dequeue() : Option[Edge] = {
        return queue.head match {
            case None => None
            case Some(link) => Some(remove(link).edge)
        }
    }
}
