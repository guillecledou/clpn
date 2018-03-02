package clpn

import clpn._
import clpn.analysis.ReachGraph

object DSL {

  class TPlace(i:Int) {
    def -->+(other:Int):Transition = Transition(i,"",Pos,0,other)
    def -->-(other:Int):Transition = Transition(i,"",Neg,0,other)
  }

  implicit def setTkToPMarking(tks:Set[Token]):PlaceMarking = {
    var st:Set[SToken] = Set()
    var dt:Set[DToken] = Set()
    for (tk <- tks) {
      tk match {
        case t:SToken => st + t
        case t:DToken => dt + t
      }
    }
    new PlaceMarking(st,dt)
  }

  implicit def intToTPlace(i:Int): TPlace = new TPlace(i)

  val newclpn = CLPN(Set(),Set(),Map())

  def toDot(cLPN: CLPN) = clpn.backend.Dot(cLPN)
  def reach2dot(cLPN:CLPN) = clpn.backend.Dot(cLPN,cLPN.behavior)

}