package clpn.backend

import clpn._
import clpn.analysis.ReachGraph

object Dot {

  /**
    * Converts a ReachGraph into a dot graph (graphviz)
    * @param rg reachability graph
    * @return dot graph in string
    */
  def apply(clpn:CLPN,rg:ReachGraph): String = {
    "digraph G {\n" +
      toDotMarkings(clpn.pls,rg.m) +
      rg.tr.map(t => s"""${t.from} -> ${t.to} [label="${t.name}"]""").mkString("\n") +
      "}"
  }

  def toDotTransitions(trs: Set[Transition]) = {
    var tnodes = new StringBuilder
    var tarcs = new StringBuilder
    var tname :Int = -1
    for (t <- trs){
      var name = if (t.name.isEmpty) {tname+=1; "_t"+tname} else t.name
      tnodes append
        s"""{node [label="${name}\\n${t.polarity}${if (t.delay>0) "\\n"+t.delay else ""}",""" +
        s"""shape="box",width=0.2, height=.4] ${name}}\n"""
      tarcs append s"""${t.from} -> ${name} -> ${t.to}\n"""
    }
    tnodes append tarcs
  }

//  def mkMarking(tks: (Set[SToken], Set[DToken])) = {
//    var tokens = tks._1 ++ tk_2
//
//  }

  def apply(clpn:CLPN): String = {
    "digraph G {\n" +
    "rankdir=LR\n" +
    clpn.pls.map(p => s"""{node [xlabel="${p}",label="",shape="circle"] ${p}}""").mkString("\n") +
    "\n" + toDotTransitions(clpn.trs) + "}"
  }

  def toDotMarkings(pls:Set[Int],stsMrks:Map[Int,Map[Int,(Set[SToken],Set[DToken])]]):String = {
    val res = new StringBuilder
    for ((st,mrk) <- stsMrks) {
      res append s"""{node [label=""""
      var strPlaces:List[String] = List()
      for (p <- pls.toList.sorted) {
        var cmrk = mrk.getOrElse(p, (Set(), Set()))
        strPlaces = strPlaces ++ List(toDotMarking(cmrk._1 ++ cmrk._2))
      }
      res append strPlaces.mkString(",") + s""""] ${st}}\n"""
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