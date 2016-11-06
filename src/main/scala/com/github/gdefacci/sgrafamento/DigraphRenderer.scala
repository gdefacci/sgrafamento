package com.github.gdefacci.sgrafamento

import scalaz.Show

object DigraphRenderer {

  import renderers._
  import scalaz.syntax.show._

  def apply[T](opts: DigraphOptions[T]): (String, Seq[T]) => String = { (graphName, nodes) =>
    val hd = (nodes.map { nd =>
      opts.idSelect(nd) + " [" + opts.nodeAttrs(nd).map(Show[GvAttr].show).mkString(", ") + "];"
    }).mkString("\n")
    
    val rankGroups = opts.rankings.foldLeft(nodes -> List.empty[(RankValue, List[String])]) { (acc, rank) =>
      val pred = rank.predicate
      val (currNodes, currentResult) = acc
      val (nodes, remaining) = currNodes.partition(pred)
      remaining -> ((rank.rank -> nodes.map(opts.idSelect andThen quoted).toList) :: currentResult)
    }._2

    val ranks = (rankGroups.filter(_._2.nonEmpty).map {
      case (rank, group) => s"{rank = ${rank.shows} ; ${group.mkString("; ")} };"
    }).mkString("\n")

//    def allLinks(nd:T):Seq[String] = {
//      val src = opts.idSelect(nd)
//      opts.linksSelect(nd).map(nd1 => s"$src -> $nd1;")
//    }
    
    val nodesMap = nodes.map( nd => opts.idSelect(nd) -> nd).toMap
    val graphLinks = nodes.flatMap( nd => opts.linksSelect(nd).map(nodesMap).map(nd1 => nd -> nd1 ) )
    val (remainingLinks1, subgraphGroups1) = opts.supbgraphs.foldLeft(graphLinks -> List.empty[(String, Seq[GraphAttr], Seq[(T,T)])]) { (acc, subgraph) =>
      val pred = subgraph.predicate
      val (currNodes, currentResult) = acc
      val (nodes, remaining) = currNodes.partition {
        case (nd1, nd2) => pred(nd1, nd2)
      }
      remaining -> ((subgraph.name, subgraph.attrs, nodes) :: currentResult)
    }
    val subgraphs1 = (subgraphGroups1.filter(_._3.nonEmpty).map {
      case (name, attrs, group) => 
        s"""subgraph ${quoted(name)} { 
          ${attrs.map( (Show[GvAttr].shows _ ).andThen( s => s+";\n") ).mkString("")}
          ${group.map { case (nd1, nd2) => s"${opts.idSelect(nd1)} -> ${opts.idSelect(nd2)};"}.mkString("\n")}
        };"""
    }).mkString("\n")
    
//    val (remainingLinks, subgraphGroups) = opts.supbgraphs.foldLeft(nodes -> List.empty[(String, Seq[GraphAttr], Seq[T])]) { (acc, subgraph) =>
//      val pred = subgraph.predicate
//      val (currNodes, currentResult) = acc
//      val (nodes, remaining) = currNodes.partition(pred)
//      remaining -> ((subgraph.name, subgraph.attrs, nodes) :: currentResult)
//    }
//    
//    val subgraphs = (subgraphGroups.filter(_._3.nonEmpty).map {
//      case (name, attrs, group) => 
//        s"""subgraph ${quoted(name)} { 
//          ${attrs.map( (Show[GvAttr].shows _ ).andThen( s => s+";\n") ).mkString("")}
//          ${group.flatMap(allLinks).mkString("\n")}
//        };"""
//    }).mkString("\n")
//
//    val lnks = remainingLinks.flatMap(allLinks).mkString("\n")
    val lnks = remainingLinks1.map { case (nd1, nd2) => s"${opts.idSelect(nd1)} -> ${opts.idSelect(nd2)};"}.mkString("\n")
    
    val grAttrs = opts.graphAttrs.map( (Show[GvAttr].shows _ ).andThen( s => s+";\n") ).mkString("\n")
    
    s"""
digraph $graphName {
  $grAttrs
  $hd
  $ranks
  $subgraphs1
  $lnks
}
"""
  }

}