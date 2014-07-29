package edu.cmu.lti.nlp.amr.JointDecoder
import edu.cmu.lti.nlp.amr._
import edu.cmu.lti.nlp.amr.FastFeatureVector._

import scala.collection.mutable.HashSet
import scala.collection.mutable.Map

class MaxSubGraph(stage1FeatureNames: List[String],
                  phraseConceptPairs: Array[ConceptInvoke.PhraseConceptPair],  // this is the concept table
                  stage2FeatureNames: List[String],
                  labelSet: Array[(String, Int)]) extends Decoder {

    val nodeWeights : Array[Double] = Array(.37, .2893)
    val edgeWeights : Array[Array[Double]] = Array(Array(),Array())
    val excludesNodes : Array[Set[Int]] = Array(Set(1), Set(1))

    val weights = FeatureVector(labelSet.map(_._1))
    val stage1Features = new ConceptInvoke.Features(stage1FeatureNames)
    val stage2Features = new GraphDecoder.Features(stage2FeatureNames, weights.labelset)

    val conceptInvoker = new ConceptInvoke.Concepts(phraseConceptPairs)

    def decode(input: Input) : FastFeatureVector.DecoderResult = {
        var graph = Graph.empty
        var nodesInGraph = new HashSet[Int]
        val queue = new EdgeQueue(nodeWeights, edgeWeights, x => excludesNodes[x])
        var feats = new FeatureVector(weights.labelset)
        // TODO actually modify feats

        def confirmEdge(edge: (Int, Int)) {
            def conditionalNodeAdd(node: Int) {
                if (!nodesInGraph.contains(node)) {
                    graph.addNode(node)
                    nodesInGraph += node
                    for (e <- queue.nodeAdded(node)) {
                        graph.addEdge(e)
                    }
                }
            }

            conditionalNodeAdd(edge._1)
            conditionalNodeAdd(edge._2)
            graph.addEdge(edge)
        }

        var x: Option[(Int, Int)] = queue.shift()
        while (x != None) {
            val edge = x match { case Some(e) => e }
            confirmEdge(edge)
            x = queue.dequeue()
        }

        return DecoderResult(graph, feats, weights.dot(feats))
    }
}