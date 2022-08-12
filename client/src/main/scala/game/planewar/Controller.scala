package client.game.Controller

import scala.collection.mutable

import org.scalajs.dom.html._
import org.scalajs.dom._
import org.scalajs.dom
import scala.scalajs.js

import shared.models.Implicits._
import scala.scalajs.js.Thenable.Implicits._

import client.game.planewar.logics.Init
import client.game.models
import client.game.models.Point
import client.game.planewar.models._

class MyPlane(val img: models.Image, var position: Point = Point(1, 1), var speed: Point = Point(0, 0), var angle: Double = 0) {

  private var (planeWidth, planeHeight, dx, dy) = (0, 0, 0, 0)
  private var (canvasWidth, canvasHeight) = (0, 0)
  
  private val ctx = document.getElementById("planeCanvas").asInstanceOf[Canvas].getContext("2d").asInstanceOf[CanvasRenderingContext2D]
  
  val tickms = DataMemory.tickms
  val resistFactor: Double = 0.1
  val accelerateFactor: Double = 0.1
  val stopFactor: Double = 0.02 * tickms
  private var frame = 0

  private var dAngle: Double = 0  //these should be generated rather than modified straightly

  private val baseDAnglePerMs: Double = math.Pi * 0.001
  private val baseBulletSpeedPerMs: Double = 0.5

  val bullets = new Bullets(mutable.ArrayBuffer.empty[Bullet])
  val enemys = new Enemys(mutable.ArrayBuffer.empty[Enemy])
  init
  def init = {
    val canvas = document.getElementById("planeCanvas").asInstanceOf[Canvas]
    canvas.height = (DataMemory.borderHeight).toInt
    canvas.width = (DataMemory.borderWidth).toInt
    canvas.style = "border-style: solid; border-width: 3px; position: absolute;"
    canvasWidth = canvas.width
    canvasHeight = canvas.height
    position = Point(canvasWidth / 2, canvasHeight / 2)
    dAngle = baseDAnglePerMs * tickms
  }

  def render = {
    if (img.isReady) {
      ctx.clearRect(0,0, canvasWidth,canvasHeight)
      planeWidth = img.img.width
      planeHeight = img.img.height
      dx = planeWidth / 2
      dy = planeHeight / 2
      ctx.save()
      ctx.translate(position.x,position.y)
      ctx.rotate(-angle)
      ctx.drawImage(img.img, -dx, -dy)
      ctx.restore()
    }
  }

  def getResist: Point = (speed / tickms) * resistFactor * tickms

  def getAccelerateAbs: Double = accelerateFactor * tickms

  def getFrameAndAdd = {
    val frameCopy = frame
    frame += 1
    frame %= 10000 
    frameCopy
  }
  
  def positionCorrect(newPosition: Point) = 
    if(!MyPlane.isInCanvas(position)){
      val x_/ = (newPosition.x.toInt.abs / DataMemory.borderWidth) * DataMemory.borderWidth
      val y_/ = (newPosition.y.toInt.abs / DataMemory.borderHeight) * DataMemory.borderHeight
      Point(if(newPosition.x < 0) newPosition.x + x_/ + DataMemory.borderWidth else newPosition.x - x_/,
            if(newPosition.y < 0) newPosition.y + y_/ + DataMemory.borderWidth else newPosition.y - y_/)
    }
    else newPosition

  def move(key: mutable.Set[Int]) = {
    position = positionCorrect(Point(position.x + speed.x, position.y + speed.y))
    var dSpeed = (getResist) * -1
    if(key.contains(KeyCode.Up)) {
      dSpeed += Point(getAccelerateAbs * math.cos(angle), getAccelerateAbs * math.sin(angle) * -1)
    }
    if(key.contains(KeyCode.Down)) {
      dSpeed -= Point(getAccelerateAbs * math.cos(angle), getAccelerateAbs * math.sin(angle) * -1)
    }
    speed = Point(speed.x + dSpeed.x, speed.y + dSpeed.y)
    
    if(key.contains(KeyCode.Left)) {
      angle += dAngle
    }
    if(key.contains(KeyCode.Right)) {
      angle -= dAngle
    }

    if(speed.length < stopFactor)speed = Point(0, 0)
  }

  def shoot = {
    bullets.addOne(position, 
                  Point(math.cos(angle) * baseBulletSpeedPerMs * tickms, math.sin(angle) * baseBulletSpeedPerMs  * tickms * (-1)),
                  angle)
  }

  def update(keys: mutable.Set[Int]) = {
    // 前置处理
    val start = System.nanoTime()
    while(angle>math.Pi*2)angle-=math.Pi*2
    while(angle<math.Pi*2*(-1))angle+=math.Pi*2

    // 更新
    if(getFrameAndAdd % 5 == 0 && keys.contains(KeyCode.Space))
      shoot
    move(keys)
    bullets.update(ctx)
    if(frame% 300 == 0 )enemys.addOne
    enemys.update(ctx)
    CheckProcedure.checkAndProcess(bullets,enemys)

    // 输出
    render
    bullets.render(ctx)
    enemys.render(ctx)

    val end = System.nanoTime()
    if(frame % (1000/tickms) == 0)println("Frame" + frame + ":"+ (end - start) / 1000.0)
  } 
}

object MyPlane {
  def getImage = models.srcManager.createImage("planeV3-25-80.png")

  def isInCanvas(p: Point): Boolean = 
    if(p.x >= 0 && p.x < DataMemory.borderWidth && p.y >= 0 && p.y < DataMemory.borderHeight )
      true
    else false
}

object DataMemory {
  val tickms = 5

  var isInit = false
  var (borderWidth, borderHeight) = (0, 0)
  def init = {
    borderHeight = (window.innerHeight * 0.85).toInt
    borderWidth = (window.innerWidth * 0.95).toInt
  }
  def transform(base: Double) = tickms * base
  
  val scoreBar = document.getElementById("score").asInstanceOf[Paragraph]
  var score = 0
  def update {
    scoreBar.innerText = score.toString
  }
}

object Controller {

  def init = {
    val ctx = Init.setCanvas
    DataMemory.init

    val keys = mutable.Set.empty[Int]
    window.onkeydown = (e: KeyboardEvent) => {
      keys += e.keyCode
    }
    window.onkeyup = (e: KeyboardEvent) => {
      keys -= e.keyCode
    }

    val img = MyPlane.getImage
    val myPlane = new MyPlane(img)
    window.setInterval(()=>{
      if(img.isReady){
        myPlane.update(keys)
        DataMemory.update
      }
    },DataMemory.tickms)


  }

}
