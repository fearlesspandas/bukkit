package minecraft.economy;


import org.bukkit.plugin.java.JavaPlugin;
import org.bukkit.command.CommandSender;
import org.bukkit.command.Command;
import org.bukkit.Material;
import org.bukkit.entity.Player;
import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemStack;
import org.bukkit.Bukkit;
import com.google.gson.Gson

import java.io._

import scala.reflect.runtime.universe._
import scala.reflect.ClassTag
import scala.collection.mutable.HashMap
import scala.io.Source

import minecraft.constants._


object OrderIO {
  var nextId = 0
  def nextid: Int = {
    import OrderBookConstants._
    val orderbook = readOrders(orderbookloc_)
    val lastid = orderbook.foldLeft(0)((a,c) => scala.math.max(a,c.orderid))
    lastid + 1
  }
  def writeOrder(order: Order)(implicit loc:String){
    val fw = new FileWriter(loc,true)
    fw.write(order.toJson + "\n")
    fw.close
  }
  def writeOrder(orderid:Int,player: Player, price: Material,item: ItemStack,buyOrSell:String)(implicit loc :String){
    writeOrder(Order(orderid,player,price,item,buyOrSell))
  }
  def readOrderBook(loc:String): OrderBook = {
    val ordersraw= Source.fromFile(loc).getLines.toArray
    val ordermap = HashMap[PlayerOrder,Order]()
    val orderarr =ordersraw.foreach(i => {
      val order = Order.deserialize(i)
      ordermap.put(PlayerOrderJson(i).fromJson,order)
    })
    val orderbookfunc = (po:PlayerOrder) => ordermap.getOrElse(po,null)
    OrderBook(orderbookfunc)
  }
  def readOrders(loc:String): List[Order] = {
    val ordersraw= Source.fromFile(loc).getLines.toArray
    //val ordermap = HashMap[Order,PlayerOrder]()
    val orderarr =ordersraw.map(i => {
      Order.deserialize(i)
    })
    orderarr.toList
  }

  def unitOrder(m:Material, i:ItemStack,buyOrSell:String):UnitOrder = {
    buyOrSell.toUpperCase match {
      case "BUY" =>
        {
        val ordermap = (t:Material) => if(t == m) i else null
        UnitBuy(ordermap)
      }
      case "SELL" => {
        val ordermap = (t:ItemStack) => if(t == i) m else null
        UnitSell(ordermap)
      }
      case _ => null
    }
  }
  def playerOrder(p:Player, u:UnitOrder): PlayerOrder = {
    val playerOrderMap = (t:UnitOrder) => if (t == u) p else null
    PlayerOrder(playerOrderMap)
  }
  def playerOrder(p:Player, m:Material,i:ItemStack,buyOrSell:String): PlayerOrder = {
    val u = unitOrder(m,i,buyOrSell)
    val playerOrderMap = (t:UnitOrder) => if (t == u) p  else null
    PlayerOrder(playerOrderMap)
  }
  def filledOrder(p:Player, po:PlayerOrder) : FilledOrder ={
    val filledOrderMap = (t:PlayerOrder) => if (t == po) p else null
    FilledOrder(filledOrderMap)
  }
  def playerMultiOrder(p:Player,u:UnitOrder,quantity:Int): List[PlayerOrder] = {
    (0 until quantity).map(i => playerOrder(p,u)).toList
  }
  def playerMultiOrder(p:Player,m:Material,i:ItemStack,quantity:Int,buyOrSell:String): List[PlayerOrder] = {
    playerMultiOrder(p,unitOrder(m,i,buyOrSell),quantity)
  }

}

abstract class FlatJson[A]{
  //require (str.count(c => c == '{') ==1,"String arg does not represent a flat json objet")
  //require (str.count(c => c == '}') ==1,"String arg does not represent a flat json objet")
  def fromJson(): A
}
  case class PlayerOrderJson(str:String) extends FlatJson[PlayerOrder] {
    require (str.count(c => c == '{') ==1,"String arg does not represent a flat json objet")
    require (str.count(c => c == '}') ==1,"String arg does not represent a flat json objet")
    def fromJson(): PlayerOrder = {
      val fields = if (str.count(i=> i == ',') > 0) str.replace("{","").replace("}","").split(",") else Array(str.replace("{","").replace("}",""))
      val fieldmap = HashMap[String,String]()
      fields.foreach( i => {
        val j = i.split(":")
        fieldmap.put(j(0),j(1))
      })
      val server = Bukkit.getServer()
      val player = server.getPlayer(fieldmap.getOrElse("player",""))
      val material = Material.getMaterial(fieldmap.getOrElse("material","").toUpperCase)
      val itemmaterial = Material.getMaterial(fieldmap.getOrElse("item","").toUpperCase)
      val itemstack = new ItemStack(itemmaterial,fieldmap.getOrElse("quantity","0").toInt)
      val buyOrSell = fieldmap.getOrElse("buyOrSell","")
      OrderIO.playerOrder(player,material,itemstack,buyOrSell)
    }
  }

trait UnitOrder
 case class UnitBuy( f: (Material) => ItemStack ) extends UnitOrder{
   val value = (m:Material) => f(m)
 }
 case class UnitSell( f: (ItemStack) => Material ) extends UnitOrder{
   val value = (i:ItemStack) => f(i)
 }
 object UnitOrder{
   def deserialize(str:String):UnitOrder={
     return null//FlatJson(str).str.
   }
 }
 case class PlayerOrder(f:UnitOrder => Player){

 }
 case class FilledOrder( f: PlayerOrder => Player)




 case class Order(orderid: Int,player: Player, material: Material,item: ItemStack,buyOrSell:String) {
  def toJson() = "{" + "orderid:"+ orderid +  ",player:"+ player.getPlayerListName()  + ",material:"+ material + ",item:" + item.getType()+"\u0001"+item.getAmount() + ",buyOrSell:" + buyOrSell + "}"//(new Gson).toJson(this)

}

object Order {
  def deserialize(raw: String): Order = {
    val server = Bukkit.getServer()
    val fields = raw.replace("{","").replace("}","").split(",")
    val args = fields.map(f => f.split(":"))
    val orderid = args(0)(1).toInt
    val player = server.getPlayer(args(1)(1))
    val price = Material.getMaterial(args(2)(1).split("\u0001")(0))
    val item = new ItemStack(Material.getMaterial(args(3)(1).split("\u0001")(0)),args(3)(1).split("\u0001")(1).toInt)
    val buyOrSell = args(4)(1)
    Order(orderid,player,price,item,buyOrSell)
  }
}

case class OrderBook(f: PlayerOrder => Order){
}

case class OrderMatch(f:UnitBuy,g:UnitSell,tradedMaterial:Material){
  // def funcMatch[A,B](func:A)(implicit tag: TypeTag[A],tagb:TypeTag[B]) = {
  //   val testtag = typeTag[B=>B]
  //   val functag = typeTag[A]
  //   if( testtag.tpe == functag.tpe) true else false
  // }
  // def funcWrapper(): Boolean{
  //   try{
  //     funcMatch[ItemStack=>ItemStack,ItemStack](g.f.compose(f.f))
  //   }catch{
  //     e:Exception => false
  //   }
  // }
  require(g.f(f.f(tradedMaterial)) == tradedMaterial) //check if the composition of the two order functions produces identity


}
