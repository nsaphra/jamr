package edu.cmu.lti.nlp.amr.JointDecoder
import edu.cmu.lti.nlp.amr._
import edu.cmu.lti.nlp.amr.FastFeatureVector._

import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.mutable.ArrayBuffer

class EdgeQueue(nodeWeights: Array[Double],
                edgeWeights: Array[Array[Double]],
                eliminatedNodes: Int => Set[Int]) {

    case class Edge(node1: Int, node2: Int)

    case class Fragment(edge: Edge,
                        totalWeight: Double,
                        nodesWeighed: Int = 2)

    type Link = LinkedNode[Fragment]

    // Queue of edges sorted with highest scores on top
    var queue = new SortedDoubleLinkedList[Fragment](
        // TODO do we want to include self-edges?
        for { (weight2, node2) <- nodeWeights.zipWithIndex
              (weight1, node1) <- nodeWeights.slice(0, node2).zipWithIndex } yield {
            Fragment(Edge(node1, node2), edgeWeights(node1)(node2) + weight1 + weight2)
        },
        (x: Fragment, y: Fragment) => x.totalWeight > y.totalWeight
    )

/*
    // Map of nodes to the Edge LinkedNodes connected to them
    var node2links: Map[Int, Set[Link]] = {
        // TODO make it an array like everything else
        val m: Map[Int, Set[Link]] = nodeWeights.zipWithIndex map { case (w, node) =>
            (node -> Set[Link]())
        }
        for (link <- queue) {
            m(link.data.edge.node1) += link
            m(link.data.edge.node2) += link
        }
        m
    }

    def remove(link: Link) : Fragment = {
        node2links(link.data.edge.node1) -= link
        node2links(link.data.edge.node2) -= link
        return queue.remove(link)
    }

    def removeAllEdges(node: Int) {
        for (link <- node2links(node)) {
            remove(link)
        }
    }

    def adjustFragmentWeights(node: Int) : Array[(Int, Int)] = {
        // Subtracts node weight from its edges
        // @return A list of fragments that can be added to graph immediately
        var edges_to_confirm = new Array[(Int, Int)]

        if (nodeWeights(node) == 0) {
            return edges_to_confirm
        }

        val old_links: Set[Link] = node2links(node)
        node2links(node) = new Set[Link]
        for (link <- old_links) {
            queue.remove(link)
        }

        var new_frags = new ArrayBuffer[Fragment]
        for (link <- old_links) {
            val frag = link.data
            frag.totalWeight -= node.weight
            frag.nodesWeighed -= 1

            // If an edge is connecting two nodes already in the graph,
            // we decide to include or toss it based on whether the weight
            // is positive, since we no longer consider whether to include its nodes.
            if (frag.nodesWeighed != 0) {
                new_frags.append(frag)
            } else if (frag.totalWeight > 0) {
                edges_to_confirm.append((frag.edge.node1, frag.edge.node2))
            }
        }

        // Re-inserting in order means re-inserting all edges is linear in
        // the length of the list
        for (link <- queue.insertAll(new_frags)) {
            node2links(node) += link
        }

        return edges_to_confirm
    }

    def nodeAdded(node: Int) : List[Fragment] = {
        for (enemy <- eliminatedNodes(node)) {
            removeAllEdges(enemy)
        }
        return adjustFragmentWeights(node)
    }

    def dequeue() : Option[(Int, Int)] = {
        return queue.head match {
            case None => None
            case Some(link) => Some(remove(link).edge)
        }
    }
*/
}
