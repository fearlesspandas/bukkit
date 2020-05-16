package minecraft.playerio

import Orders.Order._
import minecraft.economy.OrderImpl
import minecraft.economy.OrderImpl.{itemstack, material, player}

import scala.util.matching.Regex
import org.bukkit.Material
object OrderParser {
  class unitbuyorder(override val p:material,override val i:itemstack,override val remaining:Int,override val owner:String) extends order(p,i,remaining,owner)
  class unitsellorder(override val p:itemstack,override val i:material,override val remaining:Int,override val owner:String) extends order(p,i,remaining,owner)
  val mapdelim = "=>"
  val voldelim = "#"
  val quantitydelim = ":"
  val unitsell = new Regex(s"[0-9a-zA-Z]+${quantitydelim}[0-9a-zA-Z]+${mapdelim}[a-zA-Z]+")
  val unitbuy = new Regex(s"[a-zA-Z]+${mapdelim}[0-9a-zA-Z]+${quantitydelim}[0-9a-zA-Z]+")
  val items = new Regex(s"([0-9]+${quantitydelim}[A-Za-z]+)|([A-Za-z]+${quantitydelim}[0-9]+)")
  val volumes = new Regex(s"${voldelim}[0-9]+")
    def fromArgs(plyr:player,args:Array[String]):Option[Either[unitsellorder,unitbuyorder]] = {
      val concatargs = args.foldLeft("")(_ + _).replace(" ", "")
      val buyOrder = unitbuy.findAllIn( concatargs).mkString("")
      val sellOrder = unitsell.findAllIn(concatargs).mkString("")
      println("Buy Order:" + buyOrder)
      println("Sell Order:" + sellOrder)
      val itemstring = items.findAllIn(concatargs).mkString("")
      val volumestring = volumes.findAllIn(concatargs).mkString("").replaceAll(s"${voldelim}","")
      val volume = if (!volumestring.isEmpty) scala.math.max(volumestring.toInt,1) else 1
      val itemstak = itemstack(Material.getMaterial(itemstring.replaceAll("[^A-Za-z]","").toUpperCase),itemstring.replaceAll("[^0-9]","").toInt)
      println("itemstack: "+ itemstak.toString)
      val mtrl = Material.getMaterial(
        concatargs
        .replace(itemstring,"")
        .replace(mapdelim,"")
        .replace(voldelim,"")
        .replace(quantitydelim,"")
        .replace(volumestring,"")
        .toUpperCase
      )
      println("Material:" + mtrl.toString)
      if (!buyOrder.isEmpty) return Some(Right(new unitbuyorder(material(mtrl),itemstak,if (volume > 0) volume else 1,plyr.id)))
      if (!sellOrder.isEmpty ) return Some(Left(new unitsellorder(itemstak,material(mtrl),if (volume > 0) volume else 1,plyr.id)))
      None

    }
  //def fromArgs[material,itemstack](args:Array[String]):Option[order[material,itemstack]] = ???

  def main(args:Array[String]):Unit = {
    import Typical.implicits.implicits._
    val str = "1: arrow => dirt # 5"//.replace(" ","")
    val str2 = "dirt => diamond:20 # 100".replace(" ","")
    println("running:" + str)
    println(unitsell.toString())
    println(unitbuy.findAllIn(str).mkString(""))
    println(unitsell.findAllIn(str).mkString(""))
    println(items.findAllIn(str).mkString(""))

    val ord1 = fromArgs(player("0",null),Array(str))
    val ord2 = fromArgs(player("0",null),Array(str2))
    println(ord1)
    println(ord2)
    //OrderImpl.updateDataModel(ord1.get.left.get)
    println("Map 1////////////////")
    //val res1 = OrderImpl.getOrderbook()
    //println(res1)
    //OrderImpl.updateDataModel(ord2.get.left.get)
    println("Map 2 /////////////////////////")
    //val res2 = OrderImpl.getOrderbook()
   //println(res2)

  }
}
