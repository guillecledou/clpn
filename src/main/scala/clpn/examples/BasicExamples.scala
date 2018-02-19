package clpn.examples

import clpn.DSL._
import clpn._
/**
  * Basic examples of CLD diagrams taken from the literature
  */
object BasicExamples {

  // Bank balance and Intersts earned
  var bb = newclpn ++ (
    0 -->+ 1,
    1 -->+ 0
  ) //init(Map(0 -> (Set(Inc),Set())))

  // Random example
  var ex = newclpn ++ (
    1 -->+ 2 by "t1",
    2 -->+ 3 by "t2",
    2 -->- 1 by "t4",
    3 -->+ 2 by "t3"
  ) //init(Map(1 -> (Set(Inc),Set())))

  // Temperature
  var tmp = newclpn ++ (
    1 -->+ 2,
    2 -->- 3,
    3 -->+ 1
  ) //init(Map(1 -> (Set(Inc),Set())))

  // Temperature with delays
  var tmpd = newclpn ++ (
    1 -->+ 2 in 2,
    2 -->- 3,
    3 -->+ 1
  ) //init(Map(1 -> (Set(Inc),Set())))

  // Net increase
  var neti = newclpn ++ (
    1 -->+ 2,
    2 -->+ 1,
    2 -->- 3,
    3 -->+ 4,
    4 -->+ 1
  ) //init(Map(1 -> (Set(Inc),Set())))

  // Net increase with delays
  var netid = newclpn ++ (
    1 -->+ 2 by "ni->ss",
    2 -->+ 1 by "ss->ni",
    2 -->- 3 by "ss->ra" in 3,
    3 -->+ 4 by "ra->fnr" in 2,
    4 -->+ 1 by "fnr->ni"
  ) //init(Map(1 -> (Set(Inc),Set())))

  // Overshoot and collapse
  var oc = newclpn ++ (
    1 -->+ 2,
    2 -->- 3,
    3 -->+ 4,
    4 -->+ 5,
    5 -->+ 6,
    6 -->+ 1,
    1 -->- 4,
    1 -->+ 6
  ) //init(Map(1 -> (Set(Inc),Set())))
}