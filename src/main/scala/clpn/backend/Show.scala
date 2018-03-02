package clpn.backend

import clpn._

object Show{
  def apply (tk:Token): String =
    tk match {
      case Inc => "1"
      case Dec => "-1"
      case DToken(t,d) => s"$t($d)"
    }

  def apply(p:Polarity): String =
    p match {
      case Pos => "+"
      case Neg => "-"
    }

  def apply(pn:CLPN): String = {
    s"""CLPN [${pn.pls.mkString("",",","")}] """ +
    Show(pn.m) +
    pn.trs.map(t => "\n " + Show(t)).mkString("")

  }

  private def trDelay(delay: Int) =
    if (delay==0) "" else s"(+${delay})"

  private def trName(name: String) =
    if (name.isEmpty) "" else s" by ${name}"

  def apply(tr:Transition) = {
    s"""${tr.from} -${trDelay(tr.delay)}->${tr.polarity} ${tr.to}${trName(tr.name)}"""
  }

//  def apply(mks:Map[Int, (Set[SToken],Set[DToken])]): String = {
//    mks.map(mk => mkPlaceMarking(mk._1,mk._2._1,mk._2._2)).mkString("["," | ","]")
//  }

  def apply(mks:Map[Int,PlaceMarking]):String ={
    mks.map(mk => mkPlaceMarking(mk._1,mk._2.st,mk._2.dt)).mkString("[","|","]")
  }

  def mkPlaceMarking(p:Int, stk:Set[SToken], dtk:Set[DToken]):String = {
    var tks:Set[Token] = stk ++ dtk
    s"$p -> " + tks.map(tk => Show(tk)).mkString("{",",","}")

  }
}