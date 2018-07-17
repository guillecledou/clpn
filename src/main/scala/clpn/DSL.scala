package clpn

import clpn._
import clpn.analysis.ReachGraph
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
  def reach2dot(cLPN:CLPN) = clpn.backend.Dot.toDotRG(cLPN)
  def reach2dot(cLPN:CLPN,places:Set[Int]) = clpn.backend.Dot.toDotRG(cLPN,places)
//  def project(cLPN:CLPN,places:Int) = clpn.backend.Dot(cLPN,cLPN.behavior,places)

}