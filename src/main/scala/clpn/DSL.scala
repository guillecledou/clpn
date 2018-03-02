package clpn

import clpn._
import clpn.analysis.ReachGraph

object DSL {

  class TPlace(i:Int) {
    def -->+(other:Int):Transition = Transition(i,"",Pos,0,other)
    def -->-(other:Int):Transition = Transition(i,"",Neg,0,other)
  }

  implicit def intToTPlace(i:Int): TPlace = new TPlace(i)

  val newclpn = CLPN(Set(),Set(),Map())

  def toDot(cLPN: CLPN) = clpn.backend.Dot(cLPN)
  def reach2dot(cLPN:CLPN) = clpn.backend.Dot(cLPN,cLPN.behavior)

}