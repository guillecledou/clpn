package clpn

object DSL {

  class TPlace(i:Int) {
    def -->+(other:Int):Transition = Transition(i,"",Pos,0,other)
    def -->-(other:Int):Transition = Transition(i,"",Neg,0,other)
  }

  implicit def intToTPlace(i:Int): TPlace = new TPlace(i)

  val newclpn = CLPN(Set(),Set(),Map())

  def beh2Dot = clpn.backend.Dot
}