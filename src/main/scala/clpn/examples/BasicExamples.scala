package clpn.examples

import clpn.DSL._
import clpn._

/**
  * Basic examples of CLD diagrams taken from the literature
  */
object BasicExamples {

  // Bank balance and Interests earned
  var bb = newclpn ++ (
    0 -->+ 1,
    1 -->+ 0
  ) initMark((0, mk(Set(Inc))))

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
    1 -->+ 2 by "t1",
    2 -->+ 1 by "t2",
    2 -->- 3 by "t3",
    3 -->+ 4 by "t4",
    4 -->+ 1 by "t5"
  ) //init(Map(1 -> (Set(Inc),Set())))

  // Net increase with delays
  var netid = newclpn ++ (
    1 -->+ 2 by "t1",
    2 -->+ 1 by "t2",
    2 -->- 3 by "t3" in 1,
    3 -->+ 4 by "t4" in 1,
    4 -->+ 1 by "t5"
  ) //init(Map(1 -> (Set(Inc),Set())))

  // Overshoot and collapse
  var oc = newclpn ++ (
    1 -->+ 2,
    2 -->- 3,
    3 -->+ 4,
    4 -->+ 5,
    5 -->+ 6,
    6 -->+ 1 in 2,
    1 -->- 4,
    1 -->+ 6
  ) //init(Map(1 -> (Set(Inc),Set())))
}