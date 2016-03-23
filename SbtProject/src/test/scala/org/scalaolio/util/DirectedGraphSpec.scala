package org.scalaolio.util

import org.scalatest.{Matchers, FlatSpec}

class DirectedGraphSpec extends FlatSpec with Matchers {
  val mapWithNoCycles =
    Map(
        "A" -> Set("B", "C", "D")
      , "D" -> Set("P", "Q", "R")
      , "E" -> Set("F", "G", "H")
      , "I" -> Set("J", "K", "E")
      , "Q" -> Set("Y", "Z", "T")
      , "W" -> Set("Y", "Z", "A")
    )

  val (mapWithOneCycle, mapWithOneCycleReduced) =
    (
        Map(
            "A" -> Set("B", "C", "D")
          , "D" -> Set("P", "Q", "R")
          , "E" -> Set("F", "G", "H")
          , "I" -> Set("J", "K", "E")
          , "Q" -> Set("Y", "Z", "A")
          , "W" -> Set("Y", "I", "A")
        )
      , Map(
            "A" -> Set("D")
          , "D" -> Set("Q")
          , "Q" -> Set("A")
        )
    )

  val (mapWithMultipleCycles, mapWithMultipleCyclesReduced) =
    (
        Map(
            "A" -> Set("B", "E", "J")
          , "B" -> Set("E", "F")
          , "C" -> Set("I", "G")
          , "D" -> Set("G", "L")
          , "E" -> Set("H")
          , "F" -> Set("G")
          , "G" -> Set("L")
          , "H" -> Set("J", "K")
          , "I" -> Set("K", "L")
          , "J" -> Set("B")
          , "K" -> Set("B")
        )
      , Map(
            "B" -> Set("E")
          , "E" -> Set("H")
          , "H" -> Set("J", "K")
          , "J" -> Set("B")
          , "K" -> Set("B")
        )
    )

  "mapWithNoCycles" should "return no cycles" in {
    assert(DirectedGraph.isCyclic(mapWithNoCycles) === false)
    assert(DirectedGraph.filterToJustCycles(mapWithNoCycles).isEmpty === true)
  }

  "mapWithOneCycle" should "return a cycle" in {
    assert(DirectedGraph.isCyclic(mapWithOneCycle) === true)
    val result =
      DirectedGraph.filterToJustCycles(mapWithOneCycle)
    assert(result.nonEmpty === true)
    assert(result === mapWithOneCycleReduced)
  }

  "mapWithMultipleCycles" should "return a cycle" in {
    assert(DirectedGraph.isCyclic(mapWithMultipleCycles) === true)
    val result =
      DirectedGraph.filterToJustCycles(mapWithMultipleCycles)
    assert(result.nonEmpty === true)
    assert(result === mapWithMultipleCyclesReduced)
  }

  class CustomNode (
      val equipmentIdAndType: String //"A387.Structure" - identity is embedded in a string and must be parsed out
    , val childrenNodes: List[String] //even through Set is implied, for whatever reason this implementation used List
    , val otherImplementationNoise: Option[Any] = None
  )

  object CustomNodeAdapter extends (CustomNode => CustomNodeAdapter){
    def apply(customNode: CustomNode): CustomNodeAdapter =
      new CustomNodeAdapter(fetchIdentity(customNode))(customNode) {}

    def fetchIdentity(customNode: CustomNode): String =
      fetchIdentity(customNode.equipmentIdAndType)
    def fetchIdentity(eiat: String): String =
      eiat.takeWhile(char => char.isLetter || char.isDigit)
  }
  abstract case class CustomNodeAdapter(identity: String)(customNode: CustomNode) extends DirectedGraph.Node[CustomNode, String] {
    val children =
      customNode.childrenNodes.map(CustomNodeAdapter.fetchIdentity).toSet
    val source =
      customNode
  }

  val customNodeDirectedGraphWithNoCycles =
    List(
        new CustomNode("A", List("B", "C", "D"))
      , new CustomNode("B", Nil)
      , new CustomNode("C", Nil)
      , new CustomNode("D", List("P", "Q", "R"))
      , new CustomNode("E", List("F", "G", "H"))
      , new CustomNode("F", Nil)
      , new CustomNode("G", Nil)
      , new CustomNode("H", Nil)
      , new CustomNode("I", List("J", "K", "E"))
      , new CustomNode("J", Nil)
      , new CustomNode("K", Nil)
      , new CustomNode("P", Nil)
      , new CustomNode("Q", List("Y", "Z", "Z"))
      , new CustomNode("R", Nil)
      , new CustomNode("W", List("Y", "Z", "A"))
      , new CustomNode("Y", Nil)
      , new CustomNode("Z", Nil)
    )
  val (customNodeDirectedGraphWithOneCycle, customNodeDirectedGraphWithOneCycleReduced) =
    (
        List(
            new CustomNode("A.x", List("B.a", "C.a", "D.a"))
          , new CustomNode("B.x", Nil)
          , new CustomNode("C.x", Nil)
          , new CustomNode("D.x", List("P.d", "Q.d", "R.d"))
          , new CustomNode("E.x", List("F.e", "G.e", "H.e"))
          , new CustomNode("F.x", Nil)
          , new CustomNode("G.x", Nil)
          , new CustomNode("H.x", Nil)
          , new CustomNode("I.x", List("J.i", "K.i", "E.i"))
          , new CustomNode("J.x", Nil)
          , new CustomNode("K.x", Nil)
          , new CustomNode("P.x", Nil)
          , new CustomNode("Q.x", List("Y.x", "Z.x", "A.x"))
          , new CustomNode("R.x", Nil)
          , new CustomNode("W.x", List("Y.w", "I.w", "A.w"))
          , new CustomNode("Y.x", Nil)
          , new CustomNode("Z.x", Nil)
        )
      , Map(
            CustomNodeAdapter(new CustomNode("Q.x", List("Y.x", "Z.x", "A.x"))) -> Set(CustomNodeAdapter(new CustomNode("A.x", List("B.a", "C.a", "D.a"))))
          , CustomNodeAdapter(new CustomNode("A.x", List("B.a", "C.a", "D.a"))) -> Set(CustomNodeAdapter(new CustomNode("D.x", List("P.d", "Q.d", "R.d"))))
          , CustomNodeAdapter(new CustomNode("D.x", List("P.d", "Q.d", "R.d"))) -> Set(CustomNodeAdapter(new CustomNode("Q.x", List("Y.x", "Z.x", "A.x"))))
        )
    )

  val (customNodeDirectedGraphWithMultipleCycles, customNodeDirectedGraphWithMultipleCyclesReduced) =
    (
        List(
            new CustomNode("A.x", List("B.a", "E.a", "J.a"))
          , new CustomNode("B.x", List("E.b", "F.b"))
          , new CustomNode("C.x", List("I.c", "G.c"))
          , new CustomNode("D.x", List("G.d", "L.d"))
          , new CustomNode("E.x", List("H.e"))
          , new CustomNode("F.x", List("G.f"))
          , new CustomNode("G.x", List("L.g"))
          , new CustomNode("H.x", List("J.h", "K.h"))
          , new CustomNode("I.x", List("K.i", "L.i"))
          , new CustomNode("J.x", List("B.j"))
          , new CustomNode("K.x", List("B.k"))
          , new CustomNode("L.x", Nil)
        )
      , Map(
            CustomNodeAdapter(new CustomNode("B.x", List("E.b", "F.b"))) -> Set(CustomNodeAdapter(new CustomNode("E.x", List("H.e"))))
          , CustomNodeAdapter(new CustomNode("E.x", List("H.e"))) -> Set(CustomNodeAdapter(new CustomNode("H.x", List("J.h", "K.h"))))
          , CustomNodeAdapter(new CustomNode("H.x", List("J.h", "K.h"))) -> Set(CustomNodeAdapter(new CustomNode("J.x", List("B.j"))), CustomNodeAdapter(new CustomNode("K.x", List("B.k"))))
          , CustomNodeAdapter(new CustomNode("J.x", List("B.j"))) -> Set(CustomNodeAdapter(new CustomNode("B.x", List("E.b", "F.b"))))
          , CustomNodeAdapter(new CustomNode("K.x", List("B.k"))) -> Set(CustomNodeAdapter(new CustomNode("B.x", List("E.b", "F.b"))))
        )
    )

  "customNodeDirectedGraphWithNoCycles" should "return no cycles" in {
    val properMapShape =
      DirectedGraph.toMap[CustomNode, String, CustomNodeAdapter](customNodeDirectedGraphWithNoCycles, customNode => CustomNodeAdapter(customNode))
    assert(DirectedGraph.isCyclic(properMapShape) === false)
    assert(DirectedGraph.filterToJustCycles(properMapShape).isEmpty === true)
  }

  "customNodeDirectedGraphWithOneCycle" should "return a cycle" in {
    val properMapShape =
      DirectedGraph.toMap[CustomNode, String, CustomNodeAdapter](customNodeDirectedGraphWithOneCycle, customNode => CustomNodeAdapter(customNode))
    assert(DirectedGraph.isCyclic(properMapShape) === true)
    val result =
      DirectedGraph.filterToJustCycles(properMapShape)
    assert(result.nonEmpty === true)
    assert(result === customNodeDirectedGraphWithOneCycleReduced)
  }

  "customNodeDirectedGraphWithMultipleCycles" should "return a cycle" in {
    val properMapShape =
      DirectedGraph.toMap[CustomNode, String, CustomNodeAdapter](customNodeDirectedGraphWithMultipleCycles, customNode => CustomNodeAdapter(customNode))
    assert(DirectedGraph.isCyclic(properMapShape) === true)
    val result =
      DirectedGraph.filterToJustCycles(properMapShape)
    assert(result.nonEmpty === true)
    assert(result === customNodeDirectedGraphWithMultipleCyclesReduced)
  }
}
