package clpn.backend

import clpn._
import clpn.analysis.{ReachGraph, Trace}

object Dot {

  def apply(clpn:CLPN): String = {
    "digraph G {\n" +
      "rankdir=LR\n" +
      clpn.pls.map(p => s"""{node [xlabel="${p}",label="${mkClpnMarking(clpn,p)}",shape="circle"] ${p}}""").mkString("\n") +
      "\n" + toDotTransitions(clpn.trs) + "}"
  }

  /**
    * Converts a ReachGraph into a dot graph (graphviz)
    * @return dot graph in string
    */
  def apply(rg:ReachGraph):String = {
    "digraph G {\n" +
      toDotMarkings(rg.pls,rg.m) +
      rg.tr.map(t => s"""${t.from} -> ${t.to} [label="${t.name}"]""").mkString("\n") +
      "}"
  }

  def apply(rg:ReachGraph, t:Trace):String = {
    "digraph G {\n" +
      s"""label=<<B>Trace = ${t.sts.map(st => "st"+st).mkString(", ")}</B>>\n""" +
      "labelloc=top" +
      toDotMarkingsTrace(rg.pls,rg.m,t) +
      rg.tr.map(t => s"""${t.from} -> ${t.to} [label="${t.name}"]""").mkString("\n") +
    "}"
  }

  def mkClpnMarking(cLPN: CLPN,place:Int):String =
    cLPN.m.mrk.getOrElse(place,PlaceMarking(Set())).stks.mkString("{",",","}")


  def toDotMarkingsTrace(pls: Set[Int], stsMrks: Map[Int, Marking], t: Trace) = {
    val res = new StringBuilder
    for ((st,mrk) <- stsMrks) {
      if (t.sts.contains(st))
        res append s"""{node [color="red",xlabel="st${st}",label=""""
      else
        res append s"""{node [xlabel="st${st}",label=""""
      var strPlaces:List[String] = List()
      for (p <- pls.toList.sorted) {
        var cmrk = mrk.mrk.getOrElse(p, PlaceMarking(Set()))
        strPlaces = strPlaces ++ List(toDotMarking(cmrk.tks))
      }
      res append strPlaces.mkString(",") + s""""] ${st}}\n"""
    }
    res.toString()
  }

  /**
    * Converts a ReachGraph into a dot graph (graphviz) projecting only the variables in places
    * @param rg reachability graph
    * @param places places to include in the reachability graph
    * @return
    */
  def apply(rg:ReachGraph,places:Set[Int]): String = {
    "digraph G {\n" +
      toDotMarkings(rg.pls intersect places,rg.m) +
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

  def toDotMarkings(pls:Set[Int],stsMrks:Map[Int,Marking]):String = {
    val res = new StringBuilder
    for ((st,mrk) <- stsMrks) {
      res append s"""{node [xlabel="st${st}",label=""""
      var strPlaces:List[String] = List()
      for (p <- pls.toList.sorted) {
        var cmrk = mrk.mrk.getOrElse(p, PlaceMarking(Set()))
        strPlaces = strPlaces ++ List(toDotMarking(cmrk.tks))
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