package clpn

//import scala.collection.mutable.ListBuffer

import clpn.analysis.ReachabilityGraph
import clpn.backend.Show
/**
  * Created by guille on 8/2/18.
  */



case class CLPN(pls:Set[Int], trs:Set[Transition],m:Map[Int,(Set[SToken],Set[DToken])]){

  private def linkT(t:Transition):CLPN =
    CLPN(pls+t.from+t.to,trs+t,m)

  def ++(ts:Transition*): CLPN = {
    var res = this
    for (t <- ts) res = res linkT t
    res
  }

  private def linkMrks(mk: Map[Int, (Set[SToken], Set[DToken])]) =
    CLPN(pls,trs,m++mk)

  def init(mks:Map[Int,(Set[SToken],Set[DToken])]*):CLPN ={
    var res = this
    for (mk <- mks) res = res linkMrks mk
    res
  }

  lazy val behavior = ReachabilityGraph(this)

  override def toString: String = Show(this)
}

//
//case class PlaceMarking(st:Set[SToken],dt:Set[DToken]){
//
//  def enabled = st.nonEmpty
//
//  def conflicting = st.contains(Inc) && st.contains(Dec)
//}


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