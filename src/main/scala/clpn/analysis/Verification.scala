package clpn.analysis

import clpn.{CLPN, NC, PlaceMarking, Token}


object Verification {

  /**
    * Verifies if property prop is satisfied at some point in the reachability graph of cLPN
    *
    * @param cLPN Causal loop net with some initial marking
    * @param v variable(place) over which we want to verify the property
    * @param prop the property to verify as a sequence of finite increases and decreases
    * @return whethere the reachability graph of v in cLPN satisfies prop
    */
  def exists(cLPN: CLPN,v:Int,prop:List[Token]):Boolean = {
//    var res = false
    if (prop.isEmpty) true else {
      var found = false
      val rg = cLPN.behavior //.project(Set(v))
      var p = prop
      //    var propPos = 0
      var visited: Set[Int] = Set()
      var toVisit: Set[Int] = Set(rg.s0)
      while (toVisit.nonEmpty && !found) {
        var currentSt = toVisit.head
        if (p.nonEmpty) {
          var cpm: PlaceMarking = rg.m(currentSt).getOrElse(v, PlaceMarking(Set(NC)))
          //        println(s"""mid:$currentSt - mk:${cpm.stks.mkString("[",",","]")}""")
          //        println(s"""comparing ${cpm.stks.mkString("[",",","]")} with ${p.head}""")
          if (cpm.stks.contains(p.head)) {
            //        if (rg.m(currentSt)(v).stks.contains(p.head)) {
            //          println("match")
            if (p.size == 1)
              found = true
            else {
              // check substring match
              //            println("first match checing subgraph")
              var nextSts = rg.post(currentSt).toIterator
              //            p.drop(1)
              while (nextSts.hasNext && !found) {
                found = found || checkSubstring(rg, v, nextSts.next(), p.drop(1)) //, visited)
              }
            }
          } //else println(s"""not equal ${if (rg.m(currentSt).isDefinedAt(v)) rg.m(currentSt)(v).stks else "not defined"}""")
        } //else println("p is empty")
        visited += currentSt
        //      println(s"""visited: $visited""")
        toVisit ++= rg.post(currentSt)
        toVisit = toVisit -- visited
        //      println(s"""to visit: $toVisit""")
      }
      found
    }
  }

  private def checkSubstring(rg:ReachGraph,v:Int,cSt: Int, cProp: List[Token]):Boolean ={ //, visited: Set[Int]): Boolean = {
    var found = false
//    var nVisited = visited
//    println("checking subgraph")
    var nProp = cProp
    if (nProp.nonEmpty) {
      var cpm:PlaceMarking = rg.m(cSt).getOrElse(v, PlaceMarking(Set(NC)))
//      println(s"""mid:$cSt - mk:${cpm.stks.mkString("[",",","]")}""")
//      println(s"""comparing ${cpm.stks.mkString("[",",","]")} with ${nProp.head}""")
      if ((cpm.stks.contains(nProp.head))) {
//      if ((rg.m(cSt)(v).stks.contains(nProp.head))) {
        if (nProp.size == 1) {
//          println("prop size1")
          found = true
        }
        else {
          //          nVisited += cSt
          nProp = nProp.drop(1)
          var nextSts = rg.post(cSt).toIterator
          while (nextSts.hasNext && !found) {
//            println("checking in substring")
            //            var nSt = nextSts.
            found = found || checkSubstring(rg, v, nextSts.next(), nProp) //, nVisited)
          }
        }
      }
    } else {
        found = true
      }
    found
  }


}
