package clpn

import clpn.backend.Show

sealed trait Token {

  override def toString = Show(this)

  def unary_! : Token = this match {
    case Inc => Dec
    case Dec => Inc
    case NC  => NC
    case DInc(d) => DDec(d)
    case DDec(d) => DInc(d)
  }

  def *(p:Polarity) = p match {
    case Pos => this
    case Neg => !this
  }

}

//sealed trait SToken extends Token{
//
//  def unary_! : SToken = this match {
//    case Inc => Dec
//    case Dec => Inc
//  }
//
//  def *(p:Polarity) = p match {
//    case Pos => this
//    case Neg => !this
//  }
//}

case object Inc extends Token
case object Dec extends Token
case object NC  extends Token

case class DInc(delay:Int) extends Token{}
case class DDec(delay:Int) extends Token{}
