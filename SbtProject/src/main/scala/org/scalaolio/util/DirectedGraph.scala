package org.scalaolio.util

object DirectedGraph {
  trait Node[S, I] {
    def source: S
    def identity: I
    def children: Set[I]
  }

  def toMap[S, I, N <: Node[S, I]](ss: List[S], transformSToN: S => N): Map[N, Set[N]] = {
    val (ns, nByI) = {
      val iAndNs =
        ss.map(
          s => {
            val n =
              transformSToN(s)
            (n.identity, n)
          }
        )
      (iAndNs.map(_._2), iAndNs.toMap)
    }
    ns.map(n => (n, n.children.map(nByI(_)))).toMap
  }

  def isCyclic[N](nsByN: Map[N, Set[N]]) : Boolean = {
    def hasCycle(nAndNs: (N, Set[N]), visited: Set[N] = Set[N]()): Boolean =
      if (visited.contains(nAndNs._1))
        true
      else
        nAndNs._2.exists(
          n =>
            nsByN.get(n) match {
              case Some(ns) =>
                hasCycle((n, ns), visited + nAndNs._1)
              case None =>
                false
            }
        )
    nsByN.exists(hasCycle(_))
  }

  def filterToJustCycles[N](nsByN: Map[N, Set[N]]): Map[N, Set[N]] = {
    def recursive(nsByNRemaining: Map[N, Set[N]], referencedRootNs: Set[N] = Set[N]()): Map[N, Set[N]] = {
      val (referencedRootNsNew, nsByNRemainingNew) = {
        val referencedRootNsNewTemp =
          nsByNRemaining.values.flatten.toSet.intersect(nsByNRemaining.keySet)
        (
            referencedRootNsNewTemp
          , nsByNRemaining.collect {
              case (t, ts) if referencedRootNsNewTemp.contains(t) && referencedRootNsNewTemp.intersect(ts.toSet).nonEmpty =>
                (t, referencedRootNsNewTemp.intersect(ts.toSet))
            }
          )
      }
      if (referencedRootNsNew == referencedRootNs)
        nsByNRemainingNew
      else
        recursive(nsByNRemainingNew, referencedRootNsNew)
    }
    recursive(nsByN)
  }
}
