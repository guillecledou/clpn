package clpn.analysis

import clpn.{CLPN, SToken}


object Verification {

  /**
    * Verifies if property prop is satisfied at some point in the reachability graph of cLPN
    *
    * @param cLPN Causal loop net with some initial marking
    * @param v variable(place) over which we want to verify the property
    * @param prop the property to verify as a sequence of finite increases and decreases
    * @return whethere the reachability graph of v in cLPN satisfies prop
    */
  def exists(cLPN: CLPN,v:Int,prop:List[SToken]):Boolean = {
//    var res = false
    var found = false
    val rg = cLPN.behavior //.project(Set(v))
    var p = prop
//    var propPos = 0
    var visited:Set[Int] = Set()
    var toVisit:Set[Int] = Set(rg.s0)
    while (toVisit.nonEmpty && !found) {
      var currentSt = toVisit.head
      if (p.nonEmpty) {
        if (rg.m(currentSt).isDefinedAt(v) && (rg.m(currentSt)(v).st.contains(p.head))) {
          println("match")
          if (p.size == 1)
            found = true
          else {
            // check substring match
            println("first match checing subgraph")
            var nextSts = rg.post(currentSt).toIterator
            p.drop(0)
            while (nextSts.hasNext && !found) {
              found = found || checkSubstring(rg, v, nextSts.next(), p) //, visited)
            }
          }
        }else println(s"""not equal ${if (rg.m(currentSt).isDefinedAt(v)) rg.m(currentSt)(v).st else "not defined"}""")
      }else println("p is empty")
      visited += currentSt
      println(s"""visited: $visited""")
      toVisit ++= rg.post(currentSt)
      toVisit = toVisit -- visited
      println(s"""to visit: $toVisit""")
    }
    found
  }

  private def checkSubstring(rg:ReachGraph,v:Int,cSt: Int, cProp: List[SToken]):Boolean ={ //, visited: Set[Int]): Boolean = {
    var found = false
//    var nVisited = visited
    println("checking subgraph")
    var nProp = cProp
    if (nProp.nonEmpty)
      if (rg.m(cSt).isDefinedAt(v) && (rg.m(cSt)(v).st.contains(nProp.head)))
        if (nProp.size==1)
          found = true
        else {
//          nVisited += cSt
          nProp = nProp.drop(0)
          var nextSts = rg.post(cSt).toIterator
          while (nextSts.hasNext && !found)
//            var nSt = nextSts.
            found = found || checkSubstring(rg,v,nextSts.next(),nProp)//, nVisited)
        }
    else
      found = true
    found
  }


}
