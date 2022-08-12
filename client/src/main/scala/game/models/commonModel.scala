package client.game.models

import org.scalajs.dom.html._
import org.scalajs.dom._
import org.scalajs.dom
import scala.scalajs.js
import scala.math
//A class which could be used as vector or line segment, depending the way you using it.
//x 为横着的
case class Point(x: Double, y: Double) {
  def +(other: Point) = Point(x + other.x, y + other.y)
  def -(other: Point) = Point(x - other.x, y - other.y)
  def *(other: Point) = Point(x * other.x, y * other.y)
  def *(other: Double) = Point(x * other, y * other)
  def /(other: Point) = Point(x / other.x, y / other.y)
  def /(other: Double) = Point(x / other, y / other)
  def <(other: Point) = (x < other.x && y < other.y)
  def >(other: Point) = (x > other.x && y > other.y)
  def <=(other: Point) = (x <= other.x && y <= other.y)
  def >=(other: Point) = (x >= other.x && y >= other.y)
  def abs = Point(x.abs, y.abs)
  def length = math.sqrt(x * x + y * y)

  // using radian. anticlockwise(as usual)
  def rotate(theta: Double) = {
    val (cos, sin) = (math.cos(theta), math.sin(theta))
    Point(cos * x - sin * y, sin * x + cos * y)
  }

  def cos = if(length > 0)x / length else 0
  def sin = if(length > 0)y / length else 0
}

class Image(val img: HTMLImageElement) {
  private var ready: Boolean = false

  def this(url:String) = {
    this(document.createElement("img").asInstanceOf[HTMLImageElement])
    img.src = "/versionedAssets/images/" + url
    img.onload = (e: dom.Event) => ready = true
  }

  def isReady = ready
}

object Point {
  def within(a: Point, b: Point, checkPoint: Point) = 
    if((a.x < checkPoint.x && b.x < checkPoint.x) || (a.y < checkPoint.y && b.y < checkPoint.y) || (a.x > checkPoint.x && b.x > checkPoint.x) || (a.y > checkPoint.y && b.y > checkPoint.y))
      false
    else true
  
}

object commonModel {}
