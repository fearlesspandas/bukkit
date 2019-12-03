package minecraft.economy;


import org.bukkit.command.CommandSender;
import org.bukkit.command.Command;
import org.bukkit.Material;
import org.bukkit.entity.Player;
import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemStack;
import org.bukkit.Bukkit;

import java.io._
import java.util.UUID
import scala.reflect.runtime.universe._
import scala.reflect.ClassTag
import scala.collection.mutable.HashMap
import scala.io.Source

import minecraft.constants._


object OrderIO {
  var nextId = 0
  def nextid: Int =  {
    import OrderBookConstants._
    val orderbook = readOrders(orderbookloc_)
    val lastid = orderbook.foldLeft(-1)((a,c) => scala.math.max(a,c.orderid))
    lastid + 1
  }
  def writeOrder(order: Order)(implicit loc:String):Boolean= {
    try{
      val fw = new FileWriter(loc,true)
      fw.write(order.toJson + "\n")
      fw.close
      true
    }catch{
      case e:Exception =>{
        e.printStackTrace
        false
      }
    }
  }
  def writeOrder(orderid:Int,player: Player, price: Material,item: ItemStack,buyOrSell:String)(implicit loc :String): Boolean = {
    writeOrder(Order(orderid,player,price,item,buyOrSell))
  }
  def readOrderBook(implicit loc:String): OrderBook =  {
    val ordersraw= Source.fromFile(loc).getLines.toArray
    val ordermap = HashMap[Int,Order]()
    val orderList = ordersraw.map(i => Order.fromJson(i)).filter( order => order != null )
    orderList.foreach( order => ordermap.put(order.orderid,order))
    val orderbookfunc = (po:PlayerOrder) => ordermap.getOrElse(po.id,null)
    OrderBook(orderbookfunc)
  }
  def swapOrderBook(orderbook:OrderBook)(implicit loc:String): Boolean =  {
    try{
      val replacementtext = orderbook.toJsonString
      val fw = new FileWriter(loc)
      fw.write(replacementtext)
      fw.close()
      return true
    }catch{
      case e:Exception => {
        e.printStackTrace
        return false
      }
    }
  }
  def readOrders(implicit loc:String): List[Order] =  {
    val ordersraw= Source.fromFile(loc).getLines.toArray
    //val ordermap = HashMap[Order,PlayerOrder]()
    val orderarr =ordersraw.map(i => {
      Order.fromJson(i)
    }).filter( order => order != null )
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
        val ordermap = (t:ItemStack) => if(t.getAmount() >= i.getAmount()) m else null
        UnitSell(ordermap)
      }
      case _ => null
    }
  }
  // def playerOrder(p:Player, u:UnitOrder): PlayerOrder = {
  //   val playerOrderMap = (t:UnitOrder) => if (t == u) p else null
  //   PlayerOrder(playerOrderMap)
  // }
  def playerOrder(p:Player, m:Material,i:ItemStack,buyOrSell:String,orderid:Int): PlayerOrder = {
    val u = unitOrder(m,i,buyOrSell)
    val playerOrderMap = buyOrSell match{
        case "SELL" => (t:UnitOrder) => if (t.asInstanceOf[UnitSell].f(i) == m) p  else null
        case "BUY" => (t:UnitOrder) => if (t.asInstanceOf[UnitBuy].f(m).getType() == i.getType() && t.asInstanceOf[UnitBuy].f(m).getAmount() == i.getAmount()) p  else null
    }
    PlayerOrder(playerOrderMap,orderid)
  }
  def filledOrder(p:Player, po:PlayerOrder) : FilledOrder ={
    val filledOrderMap = (t:PlayerOrder) => if (t == po) p else null
    FilledOrder(filledOrderMap)
  }

}

abstract class FlatJson[A]{
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
      val player = server.getPlayer( UUID.fromString( fieldmap.getOrElse("player","") ) )
      val material = Material.getMaterial(fieldmap.getOrElse("material","").toUpperCase)
      val itemmaterial = Material.getMaterial(fieldmap.getOrElse("item","").toUpperCase)
      val itemstack = new ItemStack(itemmaterial,fieldmap.getOrElse("amount","0").toInt)
      val buyOrSell = fieldmap.getOrElse("buyOrSell","")
      val orderid = fieldmap.getOrElse("orderid","-1").toInt
      OrderIO.playerOrder(player,material,itemstack,buyOrSell,orderid)
    }
  }

trait UnitOrder
 case class UnitBuy( f: (Material) => ItemStack ) extends UnitOrder{
   val value = (m:Material) => f(m)
   def flatMap(f:(Material=>ItemStack) => (Material=>ItemStack) ) = null
 }
 case class UnitSell( f: (ItemStack) => Material ) extends UnitOrder{
   val value = (i:ItemStack) => f(i)
 }
 object UnitOrder{
   def deserialize(str:String):UnitOrder={
     return null//FlatJson(str).str.
   }
 }
 case class PlayerOrder(f:UnitOrder => Player,i_d:Int = -1){
   val id = if(i_d == -1) OrderIO.nextid else i_d
   require(id >= 0,"Playerorder id less than zero")
 }


case class Order(orderid: Int,player: Player, material: Material,item: ItemStack,buyOrSell:String) extends Ordered[Order]{
  require(buyOrSell == "BUY" || buyOrSell == "SELL")
  def toJson() = {
    val fieldmap = new HashMap[String,String]()
    fieldmap.put("orderid",orderid.toString)
    fieldmap.put("player",player.getUniqueId().toString)
    fieldmap.put("material",material.toString())
    fieldmap.put("item",item.getType().toString)
    fieldmap.put("amount",item.getAmount().toString)
    fieldmap.put("buyOrSell",buyOrSell)
    fieldmap.keySet.toStream.foldLeft("{orderid:" + orderid.toString)( (a,c) => if (c!= "orderid") a + "," + c + ":" + fieldmap.getOrElse(c,"") else a+ "" ) + "}"
  }
  def toUnitOrder() = OrderIO.unitOrder(material,item,buyOrSell)
  def toPlayerOrder() = PlayerOrderJson(toJson).fromJson
  def toSymbol() = {
    if(buyOrSell == "BUY") item.getAmount() + "@" + item.getType()+" => " + material
    else material + " => " + item.getAmount() + "@" + item.getType()
  }
  def compare(that:Order): Int = {
    if (that.buyOrSell != buyOrSell) return if (buyOrSell == "BUY") 1 else -1
    buyOrSell match{
      case "BUY" => {
        if(filleditem.getType() == that.filleditem.getType()){
            return filleditem.getAmount().compare(that.filleditem.getAmount())
        }else{
          filleditem.getType().toString.compare(that.filleditem.getType().toString)
        }
      }
      case "SELL" => {
        if(escrowitem.getType() == that.escrowitem.getType()){
          escrowitem.getAmount().compare(that.escrowitem.getAmount())
        }else{
          escrowitem.getType().toString.compare(that.escrowitem.getType().toString)
        }
      }
      case _ => -1
    }
  }
  val filleditem = if (buyOrSell == "SELL") new ItemStack(material,1) else item
  val escrowitem = if (buyOrSell == "SELL") item else new ItemStack(material,1)
}

object Order {
  def fromJson(str:String): Order = {
    try{
      require (str.count(c => c == '{') ==1,"String arg does not represent a flat json objet")
      require (str.count(c => c == '}') ==1,"String arg does not represent a flat json objet")
      val fields = if (str.count(i=> i == ',') > 0) str.replace("{","").replace("}","").split(",") else Array(str.replace("{","").replace("}",""))
      val fieldmap = HashMap[String,String]()
      fields.foreach( i => {
        val j = i.split(":")
        fieldmap.put(j(0),j(1))
      })
      val server = Bukkit.getServer()
      val player = server.getPlayer( UUID.fromString( fieldmap.getOrElse("player","") ) )
      val orderid = fieldmap.getOrElse("orderid","0").toInt
      val material = Material.getMaterial(fieldmap.getOrElse("material","").toUpperCase)
      val itemmaterial = Material.getMaterial(fieldmap.getOrElse("item","").toUpperCase)
      val itemstack = new ItemStack(itemmaterial,fieldmap.getOrElse("amount",null).toInt)
      val buyOrSell = fieldmap.getOrElse("buyOrSell","")
      return Order(orderid,player,material,itemstack,buyOrSell)
    }catch{
      case e:Exception=>{
        e.printStackTrace
        return null
      }
    }
  }
}

case class OrderBook(f:PlayerOrder => Order){
  import minecraft.constants.OrderBookConstants._

  def toJsonString(implicit loc:String): String={
    //replace current order book with playerorders
    //mapped through this.f
    val oldorderbook = OrderIO.readOrders(loc).map(o => PlayerOrderJson(o.toJson).fromJson)
    oldorderbook.map(playerorder => f(playerorder)).filter(order => order != null).foldLeft("")( (a,c) => a + c.toJson + "\n")
  }
  def toList(implicit loc:String ): List[Order]={
    OrderIO.readOrders(loc).map( o => f(o.toPlayerOrder)).filter( o => o!= null)
  }
}

case class OrderPrice(f:UnitBuy,g:UnitSell,tradingprice: ItemStack){
  require(f.f(g.f(tradingprice)) == tradingprice) //check if the composition of the two order functions produces identity
}
case class OrderMatch(f:UnitBuy,g:UnitSell,tradedMaterial:Material){
  require(g.f(f.f(tradedMaterial)) == tradedMaterial) //check if the composition of the two order functions produces identity
}

object OrderMatch{
  import minecraft.constants.OrderBookConstants._
  private def fillOrder(pof:PlayerOrder,pog:PlayerOrder,ordermatch:OrderMatch)(implicit orderbookloc:String):OrderBook = {
    require(pof.f(ordermatch.f) != null && pog.f(ordermatch.g) != null)
    val orderbook = OrderIO.readOrderBook(orderbookloc)
    val validorders = (i:PlayerOrder ) => if (i.id != pof.id || i.id != pog.id) orderbook.f(i) else null
    OrderBook(validorders)
  }
  def fillOrder(a:Order,b:Order)(implicit loc : String): OrderBook = {
    require(orderMatch(a,b),"Orders do not match")
    val orderbook = OrderIO.readOrderBook(loc)
    val validorders = (po : PlayerOrder) => if(po.id == a.orderid | po.id == b.orderid) null else orderbook.f(po)
    OrderBook(validorders)
  }
  def commitOrderFill(a:Order,b:Order)(implicit loc : String): Boolean =  {
    require(orderMatch(a,b),"Orders do not match")
    val orderbook = OrderIO.readOrderBook(loc)
    val validorders = (po : PlayerOrder) => if(po.id == a.orderid | po.id == b.orderid) null else orderbook.f(po)
    OrderIO.swapOrderBook(OrderBook(validorders))(loc)
  }
  def orderMatch(a:Order,b:Order) = {
    a.item.getType() == b.item.getType() && a.item.getAmount() == b.item.getAmount() && a.material == b.material && a.buyOrSell != b.buyOrSell //&& a.player.getPlayerListName != b.player.getPlayerListName()
  }
  def findOrder(order:Order,rplc_orderbook:OrderBook = null)(implicit orderbookloc:String):List[Order] =  {
    val orderbook = if (rplc_orderbook != null) rplc_orderbook else OrderIO.readOrderBook(orderbookloc)
    val orderlist = if (rplc_orderbook != null) rplc_orderbook.toList else OrderIO.readOrders(orderbookloc)
    //list or OrderMatch's
    val matchingorders = orderlist.map( bookorder => {
        try{
          //OrderMatch(order.toUnitOrder.asInstanceOf[UnitBuy],bookorder.toUnitOrder.asInstanceOf[UnitSell],order.material)
          if(orderMatch(bookorder,order)) bookorder else null
        }catch{
          case e:Exception => {
            e.printStackTrace
            null
          }
        }
    }).filter(o => o != null)
    return matchingorders
  }
}
