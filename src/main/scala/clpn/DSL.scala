package clpn

import clpn._
import clpn.analysis.{ReachGraph, Trace}
import clpn.Token

object DSL {

  class TPlace(i:Int) {
    def -->+(other:Int):Transition = Transition(i,"",Pos,0,other)
    def -->-(other:Int):Transition = Transition(i,"",Neg,0,other)
  }

  implicit def intToPMarking(tks:Set[Int]):PlaceMarking = {
    var ntks:Set[Token] = Set()
    for (tk <- tks) {
      tk match {
        case 1  => ntks += Inc
        case -1 => ntks += Dec
        case 0  =>
        case _ => throw new IllegalArgumentException("Only simple tokens can be assigned to an initial marking")
      }
    }
    new PlaceMarking(ntks)
  }

  implicit def intToTPlace(i:Int): TPlace = new TPlace(i)

  val newclpn = CLPN(Set(),Set(),Marking(Map()))

  def toDot(cLPN: CLPN) = clpn.backend.Dot(cLPN)
  def reach2dot(rg:ReachGraph) = clpn.backend.Dot(rg)
  def reach2dot(rg:ReachGraph,places:Set[Int]) = clpn.backend.Dot(rg,places)
  def trace2dot(rg:ReachGraph,trace:Trace) = clpn.backend.Dot(rg,trace)
//  def reach2dot(cLPN:CLPN) = clpn.backend.Dot(cLPN.behavior)
//  def reach2dot(cLPN:CLPN,places:Set[Int]) = clpn.backend.Dot.(cLPN.behavior,places)
//  def project(cLPN:CLPN,places:Int) = clpn.backend.Dot(cLPN,cLPN.behavior,places)

}