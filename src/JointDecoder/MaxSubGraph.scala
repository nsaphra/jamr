package edu.cmu.lti.nlp.amr.JointDecoder
import edu.cmu.lti.nlp.amr._
import edu.cmu.lti.nlp.amr.FastFeatureVector._

import scala.collection.mutable.List
import scala.collection.mutable.Map
import scala.collection.mutable.Set

class MaxSubGraph(stage1FeatureNames: List[String],
                  phraseConceptPairs: Array[ConceptInvoke.PhraseConceptPair)],  // this is the concept table
                  stage2FeatureNames: List[String],
                  labelSet: Array[(String, Int)]) extends Decoder {

    val weights = FeatureVector(labelSet.map(_._1))
    val stage1Features = new ConceptInvoke.Features(stage1FeatureNames)
    val stage2Features = new GraphDecoder.Features(stage2FeatureNames, weights.labelset)

    val conceptInvoker = new ConceptInvoke.Concepts(phraseConceptPairs)

    val eliminatedNodes = new Map[Node, Set[Node]]((
        for (node <- nodes)
        node -> Set()))
    val rootToFrag = new Map[Node, Concept]( // TODO eliminate root-only restriction
        for (node <- nodes)
        node -> Set())

    var graph = Graph.empty

    def addPotentialNodes() : List[(Node, Int)] = {
        // Only adds concept root nodes for now
        val sentence = input.sentence
        return for (i <- Range(0, sentence.size)) {
            var conceptList = conceptInvoker.invoke(input,i)
            for (concept <- conceptList) yield {
                val score = stage1Features.localScore(input, concept, i, i + concept.words.size)
                root = concept.graphFrag.getRoot()
                rootToFrag[root] = concept
                graph.addNode(sentence, i, i + concept.words.size, root)
                (concept, score)
            }
        }
        // TODO eliminatedNodes filling
    }

    def fillEliminatedNodes() = {
        // Fill out structure containing eliminated nodes
        throw new notImplementedError()
    }

    def addPotentialEdges() : List[(Edge, Int)] = {
        for { node1 <- graph.nodes
              (label, node2) <- node1.relations } {
        }

        // Edge weights are label weight + both node weights
        throw new notImplementedError()
    }

    def decode(input: Input) : FastFeatureVector.DecoderResult = {
        val nodes = addPotentialNodes()
        val edges = addPotentialEdges()
        fillEliminatedNodes()
        simplifyGraph()
        val queue = new EdgeQueue(nodes, edges, eliminatedNodes)

        def confirmEdge(edge: Edge) {
            def conditionalNodeAdd(n: Node) {
                if (!graph.hasNode(n)) {
                    graph.addNode(n)
                    queue.nodeAdded(n)
                }
            }

            conditionalNodeAdd(edge.node1)
            conditionalNodeAdd(edge.node2)
            graph.addEdge(edge)
        }

        var x: Option[Edge] = queue.shift()
        while (x != None) {
            val edge = x match { case Some(e) => e }
            addEdge(edge)
            x = queue.dequeue()
        }

        return DecoderResult()

        throw new notImplementedError()
    }
}