package client.game.models

import org.scalajs.dom.html._
import org.scalajs.dom._
import org.scalajs.dom
import scala.scalajs.js

import client.game.models

object srcManager {

  def createImage(url: String) = {
    new models.Image(url)
  }

}