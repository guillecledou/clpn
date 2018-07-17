package clpn

//import scala.collection.mutable.ListBuffer

import clpn.analysis.ReachabilityGraph
import clpn.backend.Show
/**
  * Created by guille on 8/2/18.
  */


/**
  * A causal loop petri net
  * @param pls places
  * @param trs transitions between places
  * @param m initial marking
  */
case class CLPN(pls:Set[Int], trs:Set[Transition],m:Marking){ //Map[Int, PlaceMarking]){ //(Set[SToken],Set[DToken])]){

  private def linkT(t:Transition):CLPN =
    CLPN(pls+t.from+t.to,trs+t,m)

  def ++(ts:Transition*): CLPN = {
    var res = this
    for (t <- ts) res = res linkT t
    res
  }

  private def linkMrks(mk:(Int, PlaceMarking)) = //(Set[SToken], Set[DToken])]) =
    CLPN(pls,trs,m+(mk)) //CLPN(pls,trs,m+(mk._1 -> mk._2))

  def initMark(mks:(Int, PlaceMarking)*):CLPN ={ //(Set[SToken],Set[DToken])]*):CLPN ={
    var res = this
    for (mk <- mks) res = res linkMrks mk
    res
  }

  lazy val behavior = ReachabilityGraph(this)

  override def toString: String = Show(this)
}


case class Marking(mrk:Map[Int,PlaceMarking]){

  def +(pm:(Int,PlaceMarking)): Marking = {
    Marking(this.mrk+(pm._1 -> pm._2))
  }

  override def toString: String = Show(this)
}

// Marking for a place
case class PlaceMarking(tks:Set[Token]){

  lazy val stks:Set[Token] = {
    var st:Set[Token] = tks.filter(t => (t == Inc) || (t == Dec) || (t == NC))
    if (st.isEmpty) Set(NC) else st
  }

  lazy val dtks:Set[Token] = tks.filterNot(t => stks.contains(t))

  def enabled = stks.contains(Inc) || stks.contains(Dec) //stks.nonEmpty

  def conflicting = stks.contains(Inc) && stks.contains(Dec)
}

/**
  * Transition of a causal loop petri net
  * @param from
  * @param name
  * @param polarity
  * @param delay
  * @param to
  */
case class Transition(from:Int,name:String,polarity:Polarity,delay:Int,to:Int){

  def fire(t:Token):Token =
    (t,delay) match {
      case (Inc,0) => Inc * polarity
      case (Dec,0) => Dec * polarity
      case (Inc,d) => DInc(d) * polarity
      case (Dec,d) => DDec(d) * polarity
      case (_,_) => throw new IllegalArgumentException("Delay tokens and no change tokens can not be fired")
    }

  def in(d:Int) = Transition(from,name,polarity,d,to)
  def by(s:String) = Transition(from,s,polarity,delay,to)
}

sealed trait Polarity{
  override def toString: String = Show(this)
}
case object Pos extends Polarity;
case object Neg extends Polarity;