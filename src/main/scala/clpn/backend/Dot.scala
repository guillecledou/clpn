package clpn.backend

import clpn._

object Dot {

  /**
    * Converts a ReachGraph into a dot graph (graphviz)
    * @param rg reachability graph
    * @return dot graph in string
    */
  def apply(pn:CLPN,rg:ReachGraph): String = {
    "digraph G {\n" +
      //"rankdir=LR\n" +
      toDotMarkings(pn.pls,rg.m) +
      rg.tr.map(t => s"${t._1} -> ${t._2}").mkString("\n") +
      "}"
  }

  def toDotMarkings(pls:Set[Int],stsMrks:Map[Int,Map[Int,(Set[SToken],Set[DToken])]]):String = {
    val res = new StringBuilder
    for ((st,mrk) <- stsMrks) {
      res append s"""{node [label=" """
//      for (p <- mrk) {
//        var stkn = p._2._1.toList.mkString("{", ",", "}")
//        var dtok = p._2._2.toList.mkString("{", ",", "}")
//        res append
//          s"""{${stkn},${dtok}}"""
//      }
      var strPlaces:List[String] = List()
      for (p <- pls.toList.sorted) {
        var cmrk = mrk.getOrElse(p, (Set(), Set()))
        strPlaces = strPlaces ++ List(toDotMarking(cmrk._1 ++ cmrk._2))
      }// ).mkString(",")
      res append strPlaces.mkString(",") + s""" "] ${st}}\n"""
//      res append s""" "] ${st}}\n"""
    }
    res.toString()
  }

  def toDotMarking(tks:Set[Token]):String = {
    var res = ""
    if (tks.isEmpty) res =  "0"
    else {
      var setres= tks.map(tk => Show(tk))
      if (res.size>1) res = res.mkString("{",",","}") else res = setres.mkString
    }
    res
  }
}