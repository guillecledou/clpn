package clpn

import clpn._
import clpn.analysis.ReachGraph
import clpn.Token

object DSL {

  class TPlace(i:Int) {
    def -->+(other:Int):Transition = Transition(i,"",Pos,0,other)
    def -->-(other:Int):Transition = Transition(i,"",Neg,0,other)
  }

  def mk(tks:Set[Token]):PlaceMarking = {
    var st:Set[SToken] = Set()
    var dt:Set[DToken] = Set()
    for (tk <- tks) {
      tk match {
        case Inc => st = st + Inc
        case Dec => st = st + Dec
        case DToken(t,d) => dt = dt + DToken(t,d)
      }
    }
    new PlaceMarking(st,dt)
  }

  implicit def intToTPlace(i:Int): TPlace = new TPlace(i)

  val newclpn = CLPN(Set(),Set(),Map())

  def toDot(cLPN: CLPN) = clpn.backend.Dot(cLPN)
  def reach2dot(cLPN:CLPN) = clpn.backend.Dot.toDotRG(cLPN)
  def reach2dot(cLPN:CLPN,places:Set[Int]) = clpn.backend.Dot.toDotRG(cLPN,places)
//  def project(cLPN:CLPN,places:Int) = clpn.backend.Dot(cLPN,cLPN.behavior,places)

}