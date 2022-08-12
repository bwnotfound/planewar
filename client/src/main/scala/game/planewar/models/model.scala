package client.game.planewar.models

import scala.collection.mutable

import client.game.models.Point
import org.scalajs.dom._
import client.game.Controller.MyPlane
import scala.util.Random
import client.game.Controller.DataMemory

case class Bullet(var position: Point, var speed: Point, var angle: Double) {
  def render(ctx: CanvasRenderingContext2D) = {
    ctx.strokeStyle = "rgb(0,0,0)"
    ctx.lineWidth = 2
    ctx.beginPath()
    ctx.moveTo(position.x, position.y)
    ctx.lineTo(position.x + math.cos(angle) * 5, position.y + math.sin(angle) * (-5))
    ctx.closePath()
    ctx.stroke()
  }
}

class Bullets(val arr: mutable.ArrayBuffer[Bullet]) {

  def addOne(position: Point, speed: Point, angle: Double) = arr.append(Bullet(position, speed, angle))
  
  def deleteOne(index: Int) = arr.remove(index)

  def check = {
    var index = arr.length - 1
    arr.toArray.reverse.foreach { bullet =>
      if(!MyPlane.isInCanvas(bullet.position)) deleteOne(index)
      index -= 1
    }
  }

  def render(ctx: CanvasRenderingContext2D) = {
    arr.foreach(_.render(ctx))
  }

  def move = {
    arr.foreach(bullet => {
      bullet.position = Point(bullet.position.x + bullet.speed.x,bullet.position.y + bullet.speed.y)
    })
  }

  def update(ctx: CanvasRenderingContext2D) = {
    move
    check
  }

}
// 1代表矩形
case class Enemy(var position: Point, var speed: Point, var angle: Double, var level: Int, val shape: Int = 1) {
  val baseSize = 8

  def getSize = baseSize * level

  def within(other: Point): Boolean = {
    val size = getSize
    if(other.x >= position.x - size/2 && other.x <= position.x + size/2 && other.y >= position.y - size/2 && other.y <= position.y + size/2)
      true
    else false
  }

  def getHit = level -= 1
  def isExist: Boolean = level >= 1

  def move = {
    position = Point(position.x + speed.x, position.y + speed.y)
  }

  def render(ctx: CanvasRenderingContext2D) {
    ctx.fillStyle = "rgb(20,0,0)"
    val size = getSize
    ctx.fillRect(position.x-size/2,position.y-size/2,size,size)
  }
}

class Enemys(val arr: mutable.ArrayBuffer[Enemy]){

  val baseSpeedPerMs: Double = 0.2
  val baseLevel: Int = 10

  def addOne = {
    val angle = Random.nextDouble * math.Pi * 2 - math.Pi
    arr.append( 
      Enemy(
        Point(Random.nextInt(DataMemory.borderWidth), 0), 
        Point(0, (Random.nextDouble + 0.5) * DataMemory.transform(baseSpeedPerMs)),
        angle,
        baseLevel))
  }

  def deleteOne(index: Int) = arr.remove(index)

  def getHit(index: Int) = {
    val enemy = arr(index)
    enemy.getHit
    if(!enemy.isExist){
      deleteOne(index)
      oneSuccess
    }
  }
  def oneSuccess = {
    DataMemory.score += baseLevel
  }
  def oneFail = {
    DataMemory.score -= 1
  }

  def check = {
    arr.toArray.zipWithIndex.reverse.foreach{case (enemy, index) =>
      if(enemy.position.y > DataMemory.borderHeight) {
        oneFail
        deleteOne(index)
      }
    }
  }

  def move = {
    arr.foreach(_.move)
  }

  def render(ctx: CanvasRenderingContext2D) = {
    arr.foreach{enemy =>
      enemy.render(ctx)
    }
  }

  def update(ctx: CanvasRenderingContext2D) = {
    move
    check
  }
}

object CheckProcedure {
  def checkAndProcess(bullets: Bullets, enemys: Enemys) = {
    var status = false
    for((bullet, bindex) <- bullets.arr.toArray.zipWithIndex.reverse){
      status = false
      for((enemy, eindex) <- enemys.arr.toArray.zipWithIndex.reverse if !status){
        if(enemy.within(bullet.position)){
          println("in there")
          enemys.getHit(eindex)
          bullets.deleteOne(bindex)
          status = true
        }
      }
    }
  }
}