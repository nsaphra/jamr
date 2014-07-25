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

    case class Fragment(edge: (Int, Int),
                        totalScore: Int,
                        nodesWeighed: Int = 2) extends ((Int, Int), Int, Int)(edge, totalScore, nodesWeighed)

    type Link = DoubleLinkedList.LinkedNode[Fragment]

    // Queue of edges sorted with highest scores on top
    var queue = new SortedDoubleLinkedList(
        edgeWeights.zipWithIndex foreach { case (node1, relns) =>
            relns.zipWithIndex foreach yield { case (node2, weight) =>
                Fragment((node1, node2), weight + nodeWeights[node1] + nodeWeights[node2])
            }
        },
        (x: Fragment) => x.totalWeight
    )

    // Map of nodes to the Edge LinkedNodes connected to them
    var node2links: Map[Int, Set[Link]] = {
        // TODO make it an array like everything else
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

    def removeAllEdges(node: Int) {
        for (link <- node2links[node]) {
            remove(link)
        }
    }

    def adjustFragmentWeights(node: Int) : List[Fragment] {
        // Subtracts node weight from its edges
        // @return A list of fragments that can be added to graph immediately
        edges_to_confirm = new List[(Int, Int)]()

        if (nodeWeights[node] == 0) {
            return
        }

        val old_links = node2links[node]
        node2links[Int] = new Set[Link]()
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

    def nodeAdded(node: Int) : List[Fragment] {
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
}
