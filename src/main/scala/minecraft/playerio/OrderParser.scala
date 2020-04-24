package minecraft.playerio

import Orders.Order._
import minecraft.economy.OrderImpl.{itemstack, material, player}

import scala.util.matching.Regex
import org.bukkit.Material
object OrderParser {
  val mapdelim = "=>"
  val voldelim = "x"
  val quantitydelim = ":"
  val unitsell = new Regex(s"[0-9a-zA-Z]+${quantitydelim}[0-9a-zA-Z]+${mapdelim}[a-zA-Z]+")
  val unitbuy = new Regex(s"[a-zA-Z]+${mapdelim}[0-9a-zA-Z]+${quantitydelim}[0-9a-zA-Z]+")
  val items = new Regex(s"([0-9]+${quantitydelim}[A-Za-z]+)|([A-Za-z]+${quantitydelim}[0-9]+)")
  val volumes = new Regex(s"${voldelim}[0-9]+")
    def fromArgs(plyr:player,args:Array[String]):Option[Either[order[itemstack,material],order[material,itemstack]]] = {
      val concatargs = args.foldLeft("")(_ + _)
      //concatargs.foldLeft()//collectFirst({case x:_ if x ==  => concatargs.indexOf(x)})
      val buyOrder = unitbuy.findAllIn( concatargs).mkString("")
      val sellOrder = unitsell.findAllIn(concatargs).mkString("")
      println(buyOrder)
      println(sellOrder)
      val itemstring = items.findAllIn(concatargs).mkString("")
      val volumestring = volumes.findAllIn(concatargs).mkString("").replaceAll(s"${voldelim}","")
      val volume = if (!volumestring.isEmpty) volumestring.toInt else 0
      val itemstak = itemstack(Material.getMaterial(itemstring.replaceAll("[^A-Za-z]","").toUpperCase),itemstring.replaceAll("[^0-9]","").toInt)
      println(itemstak.getType().toString + itemstak.getAmount())
      val mtrl = Material.getMaterial(concatargs
        .replace(itemstring,"")
        .replace(mapdelim,"")
        .replace(voldelim,"")
        .replace(quantitydelim,"")
        .replace(volumestring,"")
        .toUpperCase)
      println(mtrl.toString)
      if (!buyOrder.isEmpty) return Some(Right(order[material,itemstack](material(mtrl),itemstak,if (volume > 0) volume else 1,plyr.id)))
      if (!sellOrder.isEmpty ) return Some(Left(order[itemstack,material](itemstak,material(mtrl),if (volume > 0) volume else 1,plyr.id)))
      None

    }
  //def fromArgs[material,itemstack](args:Array[String]):Option[order[material,itemstack]] = ???

  def main(args:Array[String]):Unit = {
//    val str = "dirt => diamond:10".replace(" ","")
//val str = "10:diamond => dirt x 100".replace(" ","")
//    println("running:" + str)
//    println(unitsell.toString())
//    println(unitbuy.findAllIn(str).mkString(""))
//    println(unitsell.findAllIn(str).mkString(""))
//    println(items.findAllIn(str).mkString(""))
//    println(fromArgs(player("0",null),Array(str)))
  }
}
