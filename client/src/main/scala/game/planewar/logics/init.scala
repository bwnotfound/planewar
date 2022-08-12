package client.game.planewar.logics

import org.scalajs.dom.html._
import org.scalajs.dom._
import org.scalajs.dom
import scala.scalajs.js
import client.game.Controller.DataMemory

object Init {


  
  def setCanvas = {
    val canvas = document.getElementById("mainCanvas").asInstanceOf[Canvas]
    canvas.height = (DataMemory.borderHeight).toInt
    canvas.width = (DataMemory.borderWidth).toInt
    canvas.style = "border-style: solid; border-width: 3px; position: absolute;"
    canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
  }

}