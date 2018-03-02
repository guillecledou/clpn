package clpn.analysis

import clpn._

import scala.collection.mutable.ListBuffer

object ReachabilityGraph {

  def apply(clpn:CLPN): ReachGraph = {
    var sts:Set[Int]= Set(0)
//    var tr: Set[(Int,Int)] = Set()
    var tr:Set[rgTrans] = Set()
    //var cSt:Int= 0
    var cStNewNumber:Int= 0
    var cMrk = clpn.m
    var stMrk: Map[Int, Map[Int, (Set[SToken],Set[DToken])]] = Map(0 -> clpn.m)
    var toVisit:ListBuffer[Int] = ListBuffer(0)
    var newMrk:Set[Map[Int, (Set[SToken],Set[DToken])]] = Set()
    while (toVisit.nonEmpty) {
      //      println(s"""To visit ${toVisit}""")
      var st = toVisit.head
      toVisit = toVisit.drop(1)
      cMrk = stMrk(st)
      newMrk = update(clpn,cMrk)
      var enabledT = enabled(cMrk,clpn).map(t => t.name).mkString("|")
      for (nm <- newMrk) {
        stMrk.find({case (a,b) => b == nm}) match {
          case Some((stn,marking)) => {
            //            println(s"""Existing state found ${nm}""")
            tr += rgTrans(st,enabledT,stn)
            //            println(s"""new transition to old state ${tr}""")
          }
          case None => {
            //            println(s"""New state found ${nm}""")
            cStNewNumber+=1
            tr += rgTrans(st,enabledT,cStNewNumber)
            stMrk += (cStNewNumber -> nm)
            //            println(s"""New state to visit ${cStNewNumber}""")
            toVisit += cStNewNumber
            sts+=cStNewNumber
          }
        }
      }
    }
    ReachGraph(sts,tr,stMrk)
  }

   def update(clpn:CLPN,m:Map[Int,(Set[SToken],Set[DToken])]): Set[Map[Int,(Set[SToken],Set[DToken])]] = {
    var nm:Map[Int,(Set[SToken],Set[DToken])] = Map()
//    var stoken: Set[Token] = Set()
    //var enabledt: Set[Transition] = Set()
    // for each place pl with non-empty marking tks
    for ((pl,tks) <- m; if (m.isDefinedAt(pl))) {
      // for each outgoing transition from pl
      for (t <- clpn.trs; if (t.from == pl)) {
        // for each simple token in (TODO:tks)
        for (stk <- m(pl)._1) {
          // get the current tokens in the target of t
          var ct:(Set[SToken],Set[DToken]) = nm.getOrElse(t.to,(Set(),Set()))
          //fire t for the simple token
          var nt = t.fire(stk) // get token to add in target
          nt match {
            case n:SToken     => nm = nm + (t.to -> (ct._1 ++ Set(n), ct._2)) // add simple token to target if it is simple
            case DToken(tk,d)  => nm = nm + (t.to -> (ct._1 , ct._2 ++ Set(DToken(tk,d)))) // add dtoken to target if it is delay
          }
        }
        // for each delay token in pl // TODO: fix, this has to be conducted once not for very transition (is a waist of computing)
        for (dtk <- m(pl)._2 ) {
          // get the current token in pl (TODO:this is tks)
          var ct:(Set[SToken],Set[DToken]) = nm.getOrElse(t.from, (Set(), Set()))
          dtk match {
            case DToken(tk, 1) => nm = nm + (t.from -> (ct._1 ++ Set(tk), ct._2)) //transform token to active if delay0
            case DToken(tk, n) => nm = nm + (t.from -> (ct._1, ct._2 ++ Set(DToken(tk, n - 1)))) // decrease the token
          }
        }
      }
    }
    expand(nm,conflictPlaces(nm))
  }

  def enabled(m:Map[Int,(Set[SToken],Set[DToken])],clpn:CLPN):Set[Transition] = {
    var res:Set[Transition] = Set()
    for ((pl,tks) <-m; if (m(pl)._1.contains(Inc) || m(pl)._1.contains(Dec)))
      for (t <- clpn.trs; if (t.from == pl)) res += t
    res
  }

  def conflictPlaces(m:Map[Int,(Set[SToken],Set[DToken])]): List[Int] = {
    (for (p <- m; if (p._2._1.size>1)) yield p._1).toList
  }

  def expand(m:Map[Int,(Set[SToken],Set[DToken])], cpls:List[Int]): Set[Map[Int,(Set[SToken],Set[DToken])]] = {
    var res:Set[Map[Int,(Set[SToken],Set[DToken])]] = Set()
    if (cpls.length>0){
      var p = cpls.head
      var cp = m(p)
      var nlist = cpls.slice(1,cpls.length-1)
      m - p
      res = expand(m + (p -> (Set(Inc:SToken),cp._2)), nlist) ++
        expand(m + (p -> (Set(Dec:SToken),cp._2)), nlist)
    } else res = Set(m)
    res
  }
}
case class ReachGraph(sts:Set[Int], tr:Set[rgTrans], m:Map[Int,Map[Int,(Set[SToken],Set[DToken])]]){}
case class rgTrans(from:Int,name:String,to:Int){}