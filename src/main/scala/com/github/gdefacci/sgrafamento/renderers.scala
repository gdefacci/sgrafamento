package com.github.gdefacci.sgrafamento

import scalaz.Show

object renderers {
  
  def quoted(str: String) = "\"" + str + "\""

  import scalaz.syntax.show._

  implicit object StyleShow extends Show[Style] {
    override def shows(style: Style) = style match {
      case Style.Filled => "filled"
      case Style.Solid => "solid"
      case Style.Dashed => "dashed"
      case Style.Dotted => "dotted"
      case Style.Bold => "bold"
      case Style.Invis => "invis"
    }
  }

  implicit object ColorValueShow extends Show[ColorValue] {
    override def shows(color: ColorValue): String = color match {
      case ColorValue.Red => "red"
      case ColorValue.Yellow => "yellow"
      case ColorValue.Magenta => "magenta"
      case ColorValue.Cyan => "cyan"
      case RGB(r, g, b) => quoted("#" + r.toHexString.substring(0, 2) + g.toHexString.substring(0, 2) + b.toHexString.substring(0, 2)).toUpperCase
    }
  }

  implicit object ShapeShow extends Show[Shape] {
    override def shows(shape: Shape) = shape match {
      case Shape.Box => "box"
      case Shape.Ellipse => "ellipse"
      case Shape.Plaintext => "plaintext"
    }
  }

  implicit object RankdirShow extends Show[Rankdir] {
    override def shows(v: Rankdir) = v match {
      case Rankdir.LeftToRight => "LR"
      case Rankdir.RightToLeft => "RL"
      case Rankdir.TopToBottom => "TB"
      case Rankdir.BottomToTop => "BT"
    }
  }

  implicit object GvAttrShow extends Show[GvAttr] {
    def showAttr(nm:String, v:String):String = s"$nm = $v"
    
    override def shows(attr:GvAttr):String = attr match {
      case Color(color) => showAttr("color", color.shows)
      case FillColor(color) => showAttr("fillcolor", color.shows)
      case FontColor(color) => showAttr("fillcolor", color.shows)
      case FontSize(v) => showAttr("fontsize", v.toString)
      case Label(text) => showAttr("label", quoted(text))
      case shape: Shape => showAttr("shape", shape.shows)
      case style: Style => showAttr("style", style.shows)
      case Ranksep(inches) => showAttr("ranksep", inches.toString)
      case attr: Rankdir => showAttr("rankdir", attr.shows)
      case DefaultNode(attrs) => s"node[${attrs.map(this.shows).mkString(", ")}]"
    }
  }

  implicit object RankValueShow extends Show[RankValue] {
    import RankValue._
    override def shows(v: RankValue) = v match {
      case Same => "same"
      case Min => "min"
      case Max => "max"
      case Source => "source"
      case Sink => "sink"
    }
  }
  
}