package clpn

import clpn.backend.Show

sealed trait Token {

  override def toString = Show(this)
}

sealed trait SToken extends Token{

  def unary_! : SToken = this match {
    case Inc => Dec
    case Dec => Inc
  }

  def *(p:Polarity) = p match {
    case Pos => this
    case Neg => !this
  }
}

case object Inc extends SToken
case object Dec extends SToken

case class DToken(t:SToken,delay:Int) extends Token{

}

