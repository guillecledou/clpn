package clpn

//import scala.collection.mutable.ListBuffer

import clpn.analysis.ReachabilityGraph
import clpn.backend.Show
/**
  * Created by guille on 8/2/18.
  */



case class CLPN(pls:Set[Int], trs:Set[Transition],m:Map[Int,(Set[SToken],Set[DToken])]){
  //(pls:Set[Place],tr:Set[Transition],arcs:Set[Arcs])

  private def linkT(t:Transition):CLPN =
    CLPN(pls+t.from+t.to,trs+t,m)

  def ++(ts:Transition*): CLPN = {
    var res = this
    for (t <- ts) res = res linkT t
    res
  }

  def linkMrks(mk: Map[Int, (Set[SToken], Set[DToken])]) =
    CLPN(pls,trs,m++mk)

  def init(mks:Map[Int,(Set[SToken],Set[DToken])]*):CLPN ={
    var res = this
    for (mk <- mks) res = res linkMrks mk
    res
  }

  lazy val behavior = ReachabilityGraph(this)
//
//  def sToken(tks: Set[Token]):Set[Token] = {
//    var stks:Set[Token] = Set()
//    for (tk <- tks) {
//      tk match {
//        case n: SToken => stks + n
//        case DToken(_, _) => _
//      }
//    }
//    stks
//  }
//
//  def dToken(tks:Set[Token]):Set[Token] = {
//    var stks:Set[Token] = Set()
//    for (tk <- tks) {
//      tk match {
//        case n: SToken => _
//        case DToken(t, d) => stks + DToken(t,d)
//      }
//    }
//    stks
//  }

//  def update(m:Map[Int,(Set[SToken],Set[DToken])]): Set[Map[Int,(Set[SToken],Set[DToken])]] = {
//    var nm:Map[Int,(Set[SToken],Set[DToken])] = Map()
////    var stoken: Set[Token] = Set()
//    var enabledt: Set[Transition] = Set()
//
//    for ((pl,tks) <- m; if (m.isDefinedAt(pl))) {
//      for (t <- trs; if (t.from == pl)) {
//        for (stk <- m(pl)._1) {
//          var ct:(Set[SToken],Set[DToken]) = nm.getOrElse(t.to,(Set(),Set()))
//          var nt = t.fire(stk)
//          nt match {
//            case n:SToken     => nm = nm + (t.to -> (ct._1 ++ Set(n), ct._2))
//            case DToken(tk,d)  => nm = nm + (t.to -> (ct._1 , ct._2 ++ Set(DToken(tk,d))))
//          }
//        }
//        for (dtk <- m(pl)._2 ) {
//          var ct:(Set[SToken],Set[DToken]) = nm.getOrElse(t.from, (Set(), Set()))
//          dtk match {
//            case DToken(tk, 1) => nm = nm + (t.from -> (ct._1 ++ Set(tk), ct._2))
//            case DToken(tk, n) => nm = nm + (t.from -> (ct._1, ct._2 ++ Set(DToken(tk, n - 1))))
//          }
//        }
//      }
//    }
//    expand(nm,conflictPlaces(nm))
//  }
//
//  def conflictPlaces(m:Map[Int,(Set[SToken],Set[DToken])]): List[Int] = {
//    (for (p <- m; if (p._2._1.size>1)) yield p._1).toList
//  }
//
//  def expand(m:Map[Int,(Set[SToken],Set[DToken])], cpls:List[Int]): Set[Map[Int,(Set[SToken],Set[DToken])]] = {
//    var res:Set[Map[Int,(Set[SToken],Set[DToken])]] = Set()
//    if (cpls.length>0){
//      var p = cpls.head
//      var cp = m(p)
//      var nlist = cpls.slice(1,cpls.length-1)
//      m - p
//      res = expand(m + (p -> (Set(Inc:SToken),cp._2)), nlist) ++
//        expand(m + (p -> (Set(Dec:SToken),cp._2)), nlist)
//    } else res = Set(m)
//    res
//  }
//
//  def behavior: ReachGraph = {
//    var sts:Set[Int]= Set(0)
//    var tr: Set[(Int,Int)] = Set()
//    //var cSt:Int= 0
//    var cStNewNumber:Int= 0
//    var cMrk = m
//    var stMrk: Map[Int, Map[Int, (Set[SToken],Set[DToken])]] = Map(0 -> m)
//    var toVisit:ListBuffer[Int] = ListBuffer(0)
//    var newMrk:Set[Map[Int, (Set[SToken],Set[DToken])]] = Set()
//    while (toVisit.nonEmpty) {
////      println(s"""To visit ${toVisit}""")
//      var st = toVisit.head
//      toVisit = toVisit.drop(1)
//      newMrk = update(stMrk(st))
//      for (nm <- newMrk) {
//        stMrk.find({case (a,b) => b == nm}) match {
//          case Some((stn,marking)) => {
////            println(s"""Existing state found ${nm}""")
//            tr += (st -> stn)
////            println(s"""new transition to old state ${tr}""")
//          }
//          case None => {
////            println(s"""New state found ${nm}""")
//            cStNewNumber+=1
//            tr += (st -> cStNewNumber)
//            stMrk += (cStNewNumber -> nm)
////            println(s"""New state to visit ${cStNewNumber}""")
//            toVisit += cStNewNumber
//            sts+=cStNewNumber
//          }
//        }
//      }
//    }
//    ReachGraph(sts,tr,stMrk)
//  }

  override def toString: String = Show(this)
}

/*
case class Marking(id:Int,m:Map[Int,Set[Token]]) {

}
*/
//case class ReachGraph(sts:Set[Int], tr:Set[(Int,Int)], m:Map[Int,Map[Int,(Set[SToken],Set[DToken])]]){


//}


/*
case class Place(id:Int,t:Set[Token],dt:Set[DToken]) {

}
*/




case class Transition(from:Int,name:String,polarity:Polarity,delay:Int,to:Int){

  def fire(t:SToken):Token =
    (t,delay) match {
      case (n:SToken,0) => n * polarity
      case (n:SToken,d) => DToken(n * polarity,d)
      //case (DToken(t_,_),_) => throw new IllegalArgumentException("Delay tokens can not be fired")
    }

  def in(d:Int) = Transition(from,name,polarity,d,to)
  def by(s:String) = Transition(from,s,polarity,delay,to)
}
/*
case class Marking(m:Map[Int,Set[Token]]) {

}
*/

sealed trait Polarity{
  override def toString: String = Show(this)
}
case object Pos extends Polarity;
case object Neg extends Polarity;