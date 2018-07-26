package clpn.analysis

import clpn._

import scala.collection.mutable.ListBuffer


object Verification {

  def weakExists(cLPN: CLPN, v:Int, prop:List[Token]):(Boolean,List[Int]) = {
    if (prop.isEmpty) (true,List()) else {
      var trace:List[Int] = List()
      var found = false
      var rg = cLPN.behavior
      var visited:Set[Int] = Set()
      var toVisit:Set[Int] = Set(rg.s0)

      while (toVisit.nonEmpty && !found){
        var currentSt = toVisit.head
          var currentPM = rg.m(currentSt).mrk.getOrElse(v,PlaceMarking(Set(NC)))
          if (currentPM.stks.contains(prop.head)) {
            trace ++= List(currentSt)
              var nextSts = rg.post(currentSt).toIterator
              while (nextSts.hasNext && !found) {
                val (foundSub,subTrace) = checkWeakSubstring(rg,v,nextSts.next(),prop,Set(currentSt)) // nodrop in prop
                if (foundSub) {
                  trace ++= subTrace
                  found = true
                }
              }
          }

        if (!found) trace = List()
        visited += currentSt
        toVisit ++= rg.post(currentSt)
        toVisit = toVisit -- visited
      }
      (found, trace) //(found,trace.map(x => rg.m(x)))
    }
  }

  def checkWeakSubstring(rg:ReachGraph, v: Int, st: Int, prop: List[Token],visited:Set[Int]): (Boolean,List[Int]) = {
    var found = false
    var trace:List[Int] = List()
    var visitedSameProp:Set[Int] = visited

    if (prop.nonEmpty) {
      var currentPM = rg.m(st).mrk.getOrElse(v,PlaceMarking(Set(NC)))
      if (currentPM.stks.contains(prop.head) || currentPM.noChanged) {
        trace ++= List(st)
        visitedSameProp += st
        var nextSts = (rg.post(st) -- visitedSameProp).toIterator
        if (!nextSts.hasNext){
          if (prop.size == 1) found = true
        }else {
          while (nextSts.hasNext && !found) {
            val (foundSub, subTrace) = checkWeakSubstring(rg, v, nextSts.next(), prop,visitedSameProp)
            if (foundSub) {
              trace ++= subTrace
              found = true
            }
          }
        }
      }else
        if (prop.size == 1) found = true
        else if (currentPM.stks.contains(prop.drop(1).head)) {
          trace ++= List(st)
          visitedSameProp = Set(st)
          var nextSts = (rg.post(st) -- visited).toIterator
          if (!nextSts.hasNext){
            if (prop.size == 1) found = true
          }else {
            while (nextSts.hasNext && !found) {
              val (foundSub, subTrace) = checkWeakSubstring(rg, v, nextSts.next(), prop.drop(1), visitedSameProp)
              if (foundSub) {
                trace ++= subTrace
                found = true
              }
            }
          }
        }
    } else found = true
    (found,trace)
  }

  /**
    * Verifies if property prop is satisfied at some point in the reachability graph of cLPN
    *
    * @param cLPN Causal loop net with some initial marking
    * @param v variable(place) over which we want to verify the property
    * @param prop the property to verify as a sequence of finite increases and decreases
    * @return whethere the reachability graph of v in cLPN satisfies prop
    */
  def exists(cLPN: CLPN,v:Int,prop:List[Token]) :(Boolean,List[Int]) = {
    if (prop.isEmpty) (true,List()) else {
      var trace:List[Int] = List()
      var found = false
      val rg = cLPN.behavior //.project(Set(v))
      var visited: Set[Int] = Set()
      var toVisit: Set[Int] = Set(rg.s0)

      while (toVisit.nonEmpty && !found) {
        var currentSt = toVisit.head
        if (prop.nonEmpty) {
          var currentPM = rg.m(currentSt).mrk.getOrElse(v, PlaceMarking(Set(NC)))
          if (currentPM.stks.contains(prop.head)) {
            trace ++= List(currentSt)
            if (prop.size == 1)
              found = true
            else {
              // check substring match
              var nextSts = rg.post(currentSt).toIterator
              while (nextSts.hasNext && !found) {
                val (foundSub, subtrace) = checkSubstring(rg, v, nextSts.next(), prop.drop(1))
//                found = found || foundSub  //, visited)
                if (foundSub) {
                  trace ++= subtrace
                  found = true
                }
              }
            }
          }
        }
        if (!found) trace = List()
        visited += currentSt
        toVisit ++= rg.post(currentSt)
        toVisit = toVisit -- visited
      }
      (found, trace)//(found,trace.map(x => rg.m(x)).toList)
    }
  }

  private def checkSubstring(rg:ReachGraph,v:Int,cSt: Int, prop: List[Token]):(Boolean,List[Int])={ //, visited: Set[Int]): Boolean = {
    var found = false
    var trace: List[Int] = List()

    if (prop.nonEmpty) {
      var cpm:PlaceMarking = rg.m(cSt).mrk.getOrElse(v, PlaceMarking(Set(NC)))
      if ((cpm.stks.contains(prop.head))) {
        trace ++= List(cSt)
        if (prop.size == 1) {
          found = true
        } else {
          var nextSts = rg.post(cSt).toIterator
          while (nextSts.hasNext && !found) {
            val (foundSub, subtrace) = checkSubstring(rg, v, nextSts.next(), prop.drop(1)) //, nVisited)
            //              found = found ||
            if (foundSub) {
              trace ++= subtrace
              found = true
            }
          }
        }
      }
    } else found = true
    (found, trace)
  }

  /**
    * It verifies if a property is satisfied always, i.e. if whenever prop.head appears
    * it always follows prop.tail
    * @param cLPN
    * @param v
    * @param prop
    * @return
    */
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
          var cpm: PlaceMarking = rg.m(currentSt).mrk.getOrElse(v, PlaceMarking(Set(NC)))
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
      var cpm:PlaceMarking = rg.m(cSt).mrk.getOrElse(v, PlaceMarking(Set(NC)))
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
