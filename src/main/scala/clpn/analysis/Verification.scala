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
    if (prop.isEmpty) true else {
      var found = false
      val rg = cLPN.behavior //.project(Set(v))
      var p = prop
      var visited: Set[Int] = Set()
      var toVisit: Set[Int] = Set(rg.s0)
      while (toVisit.nonEmpty && !found) {
        var currentSt = toVisit.head
        if (p.nonEmpty) {
          var cpm: PlaceMarking = rg.m(currentSt).getOrElse(v, PlaceMarking(Set(NC)))
          if (cpm.stks.contains(p.head)) {
            if (p.size == 1)
              found = true
            else {
              // check substring match
              var nextSts = rg.post(currentSt).toIterator
              while (nextSts.hasNext && !found) {
                found = found || checkSubstring(rg, v, nextSts.next(), p.drop(1)) //, visited)
              }
            }
          }
        }
        visited += currentSt
        toVisit ++= rg.post(currentSt)
        toVisit = toVisit -- visited
      }
      found
    }
  }

  private def checkSubstring(rg:ReachGraph,v:Int,cSt: Int, cProp: List[Token]):Boolean ={ //, visited: Set[Int]): Boolean = {
    var found = false
    var nProp = cProp
    if (nProp.nonEmpty) {
      var cpm:PlaceMarking = rg.m(cSt).getOrElse(v, PlaceMarking(Set(NC)))
      if ((cpm.stks.contains(nProp.head))) {
        if (nProp.size == 1) {
          found = true
        }
        else {
          nProp = nProp.drop(1)
          var nextSts = rg.post(cSt).toIterator
          while (nextSts.hasNext && !found) {
            found = found || checkSubstring(rg, v, nextSts.next(), nProp) //, nVisited)
          }
        }
      }
    } else found = true
    found
  }

  def always(cLPN: CLPN,v:Int,prop:List[Token]):Boolean = {
    if (prop.isEmpty) true else {
      var found = false
      val rg = cLPN.behavior //.project(Set(v))
      var p = prop
      var visited: Set[Int] = Set()
      var toVisit: Set[Int] = Set(rg.s0)
      while (toVisit.nonEmpty && !found) {
        var currentSt = toVisit.head
        if (p.nonEmpty) {
          var cpm: PlaceMarking = rg.m(currentSt).getOrElse(v, PlaceMarking(Set(NC)))
          if (cpm.stks.contains(p.head)) {
            if (p.size == 1)
              found = true
            else {
              // check substring match
              var nextSts = rg.post(currentSt).toIterator
              var all = true
              while (nextSts.hasNext && all) {
                all = all && checkASubstring(rg, v, nextSts.next(), p.drop(1)) //, visited)
//                found = found && checkASubstring(rg, v, nextSts.next(), p.drop(1)) //, visited)
              }
              found = all
            }
          }
        }
        visited += currentSt
        toVisit ++= rg.post(currentSt)
        toVisit = toVisit -- visited
      }
      found
    }
  }

  private def checkASubstring(rg:ReachGraph,v:Int,cSt: Int, cProp: List[Token]):Boolean ={ //, visited: Set[Int]): Boolean = {
    var found = false
    var nProp = cProp
    if (nProp.nonEmpty) {
      var cpm:PlaceMarking = rg.m(cSt).getOrElse(v, PlaceMarking(Set(NC)))
      if ((cpm.stks.contains(nProp.head))) {
        if (nProp.size == 1) {
          found = true
        }
        else {
          nProp = nProp.drop(1)
          var nextSts = rg.post(cSt).toIterator
          var all = true
          while (nextSts.hasNext && all) {
//            println(s"""state = $cSt with mk = ${cpm.stks.mkString(",")}""")
            all = all && checkASubstring(rg, v, nextSts.next(), nProp) //, nVisited)
          }
          found = all
        }
      }
    } else found = true
    found
  }


}
