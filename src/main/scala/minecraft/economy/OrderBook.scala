package minecraft.economy;


import org.bukkit.plugin.java.JavaPlugin;
import org.bukkit.command.CommandSender;
import org.bukkit.command.Command;
import org.bukkit.Material;
import org.bukkit.entity.Player;
import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemStack;

import com.google.gson.Gson

import java.io._

import scala.reflect.runtime.universe._
//
// import org.bukkit.inventory.Packet250CustomPayload;
// import org.bukkit.inventory.Packet100OpenWindow;
// import org.bukkit.inventory.MerchantRecipeList;
// import org.bukkit.inventory.MerchantRecipe;


object OrderWriter {
  var nextId = 0
  def nextid = {
    nextId +=1
    nextId-1
  }
  def writeOrder(order: Order)(implicit loc:String){
    val pw = new PrintWriter(new File(loc))
    pw.write(order.toJson)
    pw.close
  }
  def writeOrder(orderid:Int,player: Player, price: ItemStack,item: ItemStack,remaining: Int)(implicit loc :String){
    writeOrder(Order(orderid,player,price,item,remaining))
  }
  def readOrderBook(loc:String): OrderBook = {
    val fr = new FileReader(new File(loc))
    (new Gson).fromJson(fr,classOf[OrderBook])
  }


}
case class Order(orderid:Int,player:Player, price:ItemStack,item: ItemStack,remaining: Int) {
  def toJson() = (new Gson).toJson(this)
}


case class OrderBook(orders:Order*){
}
