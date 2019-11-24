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
import scala.io.Source



//
// import org.bukkit.inventory.Packet250CustomPayload;
// import org.bukkit.inventory.Packet100OpenWindow;
// import org.bukkit.inventory.MerchantRecipeList;
// import org.bukkit.inventory.MerchantRecipe;


object OrderIO {
  var nextId = 0
  def nextid = {
    nextId +=1
    nextId-1
  }
  def writeOrder(order: Order)(implicit loc:String){
    println("ORDERLOC=" + loc)
    val fw = new FileWriter(loc,true)
    fw.write(order.toJson + "\n")
    fw.close
  }
  def writeOrder(orderid:Int,player: Player, price: ItemStack,item: ItemStack,remaining: Int)(implicit loc :String){
    writeOrder(Order(orderid,player,price,item,remaining))
  }
  def readOrderBook(loc:String): OrderBook = {
    val ordersraw= Source.fromFile(loc).getLines.toArray
    val orderarr =ordersraw.map(i => Order.deserialize(i))
    OrderBook(orderarr:_*)
  }


}
case class Order(orderid: Int,player: Player, price: ItemStack,item: ItemStack,remaining: Int) {
  def toJson() = "{" + "orderid:"+ orderid +  ",player:"+ player.getPlayerListName()  + ",price:"+ price.getType() + "\u0001" + price.getAmount() + ",item:" + item.getType()+"\u0001"+item.getAmount() + ",remaining:" + remaining +  "}"//(new Gson).toJson(this)
}

object Order {
  def deserialize(raw: String): Order = {
    val server = Bukkit.getServer()
    val fields = raw.replace("{","").replace("}","").split(",")
    val args = fields.map(f => f.split(":"))
    val orderid = args(0)(1).toInt
    val player = server.getPlayer(args(1)(1))
    val price = new ItemStack(Material.getMaterial(args(2)(1).split("\u0001")(0)), args(2)(1).split("\u0001")(1).toInt)
    val item = new ItemStack(Material.getMaterial(args(3)(1).split("\u0001")(0)),args(3)(1).split("\u0001")(1).toInt)
    val remaining = args(4)(1).toInt
    Order(orderid,player,price,item,remaining)
  }

}


case class OrderBook(orders:Order*){
}
