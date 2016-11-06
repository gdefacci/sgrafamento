package com.github.gdefacci.sgrafamento

sealed trait GvAttr
sealed trait NodeAttr extends GvAttr
sealed trait GraphAttr extends GvAttr

case class Label(value: String) extends NodeAttr with GraphAttr
case class FontSize(value: Int) extends NodeAttr with GraphAttr

case class DefaultNode(attrs:List[NodeAttr]) extends GraphAttr
object DefaultNode {
  
  def apply(nds:NodeAttr*) = new DefaultNode(nds.toList)
}

sealed trait Shape extends NodeAttr
object Shape {
  case object Box extends Shape
  case object Ellipse extends Shape
  case object Plaintext extends Shape
}
sealed trait ColorValue
object ColorValue {
  object Red extends ColorValue
  object Yellow extends ColorValue
  object Magenta extends ColorValue
  object Cyan extends ColorValue
}
case class RGB(r: Int, g: Int, b: Int) extends ColorValue

case class Color(value: ColorValue) extends NodeAttr with GraphAttr
case class FillColor(value: ColorValue) extends NodeAttr with GraphAttr
case class FontColor(value: ColorValue) extends NodeAttr with GraphAttr

sealed trait Style extends NodeAttr with GraphAttr
object Style {

  object Filled extends Style
  object Solid extends Style
  object Dashed extends Style
  object Dotted extends Style
  object Bold extends Style
  object Invis extends Style

}

case class Ranksep(inches: BigDecimal) extends GraphAttr

sealed trait Rankdir extends GraphAttr
object Rankdir {
  object LeftToRight extends Rankdir
  object RightToLeft extends Rankdir
  object TopToBottom extends Rankdir
  object BottomToTop extends Rankdir
}

sealed trait RankValue
object RankValue {
  sealed trait RankingFactory { self: RankValue =>
    def apply[T](predicate: T => Boolean) = Ranking(this, predicate)
  }
  object Same extends RankValue with RankingFactory
  object Min extends RankValue with RankingFactory
  object Max extends RankValue with RankingFactory
  object Source extends RankValue with RankingFactory
  object Sink extends RankValue with RankingFactory
}

case class Ranking[T](rank: RankValue, predicate: T => Boolean)
case class SubGraph[T](name: String, predicate: (T,T) => Boolean, attrs:Seq[GraphAttr])

case class DigraphOptions[T](
    idSelect: T => String,
    linksSelect: T => Seq[String],
    graphAttrs: Seq[GraphAttr] = Nil,
    nodeAttrs: T => Seq[NodeAttr] = (nd:T) => Nil,
    rankings: Seq[Ranking[T]] = Nil,
    supbgraphs:Seq[SubGraph[T]] = Nil)