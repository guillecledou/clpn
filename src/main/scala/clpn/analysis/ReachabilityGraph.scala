package clpn.analysis

import clpn._

import scala.collection.mutable.ListBuffer

object ReachabilityGraph {

  def apply(clpn:CLPN): ReachGraph = {
    var sts:Set[Int]= Set(0) // set of sates for the the RG, initialy state 0 associated to initial marking
    var stCounter:Int= 0 // last state id used
    var tr:Set[rgTrans] = Set() //set fo transitions between states of the RG
    var cMrk = clpn.m  // current marking
    var stMrk: Map[Int, Map[Int, PlaceMarking]] = Map(0 -> clpn.m) // mapping between states and markings
    // list of markings to visit as they are discovered (initially marking associated to state 0)
    var toVisit:ListBuffer[Int] = ListBuffer(0)
    var newMrk:Set[Map[Int,PlaceMarking]] = Set() // new markings(normalized) discovered after updating cMrk
    while (toVisit.nonEmpty) { //while there are markings to visit
      var st = toVisit.head // get the state id  of the marking to visit
      toVisit = toVisit.drop(1) // remove it from the list
      cMrk = stMrk(st) // get the marking associated to that state id
      newMrk = update(clpn,cMrk)
      var enabledT = enabled(cMrk,clpn).map(t => t.name).mkString("|")
      for (nm <- newMrk) {
        stMrk.find({case (a,b) => b == nm}) match {
          case Some((stn,marking)) => {
            tr += rgTrans(st,enabledT,stn)
          }
          case None => {
            stCounter+=1
            tr += rgTrans(st,enabledT,stCounter)
            stMrk += (stCounter -> nm)
            toVisit += stCounter
            sts+=stCounter
          }
        }
      }
    }
    ReachGraph(sts,0,tr,stMrk)
  }

   def update(clpn:CLPN,m:Map[Int,PlaceMarking]):Set[Map[Int,PlaceMarking]] ={//(Set[SToken],Set[DToken])]): Set[Map[Int,(Set[SToken],Set[DToken])]] = {
    var nm:Map[Int,PlaceMarking] = Map()
    // for each place pl with non-empty marking tks
    for ((pl,tks) <- m; if (m.isDefinedAt(pl))) {
      // for each outgoing transition from pl
      for (t <- clpn.trs; if (t.from == pl)) {
        // for each simple token in tks
        for (stk <- tks.stks) {
          // get the current tokens in the target of t
          var ct:PlaceMarking = nm.getOrElse(t.to,PlaceMarking(Set()))
          //fire t for the simple token
          var nt = t.fire(stk) // get token to add in target
          nm = nm + (t.to -> PlaceMarking(ct.tks + nt))
        }
      }
      // for each delay token in pl // TODO: fix, this has to be conducted once not for very transition (is a waste of computing)
      for (dtk <- m(pl).dtks ) {
        // get the current token in pl
        var ct:PlaceMarking = nm.getOrElse(pl, PlaceMarking(Set()))
        dtk match {
          case DInc(1) => nm = nm + (pl -> PlaceMarking(ct.tks + Inc)) //transform token to active if delay0
          case DDec(1) => nm = nm + (pl -> PlaceMarking(ct.tks + Dec)) //transform token to active if delay0
          case DInc(n) => nm = nm + (pl -> PlaceMarking(ct.tks + DInc(n - 1))) // decrease the token
          case DDec(n) => nm = nm + (pl -> PlaceMarking(ct.tks + DDec(n - 1))) // decrease the token
        }
      }
    }
    expand(nm,conflictPlaces(nm))
  }

  def enabled(m:Map[Int,PlaceMarking],cLPN: CLPN):Set[Transition] ={ //(Set[SToken],Set[DToken])],clpn:CLPN):Set[Transition] = {
    var res:Set[Transition] = Set()
    for ((pl,pmrk) <-m; if (pmrk.enabled))
      for (t <- cLPN.trs; if (t.from == pl)) res += t
    res
  }

  def conflictPlaces(m:Map[Int,PlaceMarking]): List[Int] = {
    (for ((p,pmrk) <- m; if pmrk.conflicting) yield p).toList
  }

  def expand(m:Map[Int,PlaceMarking], cpls:List[Int]): Set[Map[Int,PlaceMarking]] = {
    var res:Set[Map[Int,PlaceMarking]] = Set()
    if (cpls.length>0){
      var p = cpls.head
      var cp = m(p)
      var nlist = cpls.slice(1,cpls.length-1)
      m - p
      res = expand(m + (p -> PlaceMarking(cp.tks - Dec)), nlist) ++
        expand(m + (p -> PlaceMarking(cp.tks - Inc)), nlist)
    } else res = Set(m)
    res
  }
}

/**
  * Reachability Graph
  * @param sts states ids
  * @param tr transitions between states ids
  * @param m mapping between states ids and the marking associated to that state.
  */
case class ReachGraph(sts:Set[Int],s0:Int, tr:Set[rgTrans], m:Map[Int,Map[Int,PlaceMarking]]){

  def project(places:Set[Int]): ReachGraph =
    ReachGraph(sts,s0,tr,m.mapValues(mk => mk.filter(p => places.contains(p._1))))

  def post(st:Int):Set[Int] = tr.filter(_.from == st).map(t => t.to)

}

/**
  * Transitios for a reachability graph
  * @param from id of the origin state
  * @param name transition name. Corresponds to the name off all enabled transitions at the origin state
  * @param to id of the target state
  */
case class rgTrans(from:Int,name:String,to:Int){}