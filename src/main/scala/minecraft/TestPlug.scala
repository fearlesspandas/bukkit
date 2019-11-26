package minecraft;

import java.util.List
import scala.collection.JavaConverters._
import collection.mutable._

import org.bukkit.plugin.java.JavaPlugin;
import org.bukkit.command.CommandSender;
import org.bukkit.command.Command;
import org.bukkit.Material;
import org.bukkit.entity.Player;
import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemStack;
//
// import org.bukkit.inventory.Packet250CustomPayload;
// import org.bukkit.inventory.Packet100OpenWindow;
// import org.bukkit.inventory.MerchantRecipeList;
// import org.bukkit.inventory.MerchantRecipe;


import minecraft.economy._
import minecraft.constants._
//import org.apache.spark.sql._

class Minecap extends JavaPlugin{

  override def onEnable() {
		getLogger().info("onEnable has been invoked!");
	}

	override def onDisable() {
		getLogger().info("onDisable has been invoked!");
	}

  override def onTabComplete(sender : CommandSender, cmd : Command, label : String, args : Array[String]) : java.util.List[String] = {
      val response = cmd.getName() match {
        case "$" => {
            args.size match {
              case 1 => {
                val res = Material.values()filter( m => m.isItem() && m.toString().contains(args(0).toUpperCase() ) )
                res.map( i => i.toString).toBuffer.asJava
                }
              case 3 =>{
                val res = Material.values()filter( m => m.isItem() && m.toString().contains(args(2).toUpperCase()) )
                res.map( i => i.toString).toBuffer.asJava
              }
              case _ => Array("<amount>").toBuffer.asJava
            }
        }
        case "buy" => {
          args.size match{
            case 1 => {
              val res = Material.values()filter( m => m.isItem() && m.toString().contains(args(0).toUpperCase() ) )
              res.map( i => i.toString).toBuffer.asJava
            }
            case _ => Array[String]().toBuffer.asJava
          }
        }
        case "sell" => {
          args.size match{
            case 1 => {
              val res = Material.values()filter( m => m.isItem() && m.toString().contains(args(0).toUpperCase() ) )
              res.map( i => i.toString).toBuffer.asJava
            }
            case _ => Array[String]().toBuffer.asJava
          }
        }
        case _ => Array[String]().toBuffer.asJava
    }
    return response
  }

	override def onCommand(sender : CommandSender, cmd : Command, label : String, args : Array[String]):Boolean = {
    import OrderBookConstants._
    def playerCommand(player: Player, cmd : Command, label : String, args : Array[String]):String ={
      val response = cmd.getName() match {
        case "$" => {
          try {
            val orderarr = args(0).split("=>")
            val buyOrSell = if (orderarr(0).contains("@")) "BUY" else if (orderarr(1).contains("@")) "SELL" else null
            val itemquantity = buyOrSell match {
              case "BUY" => orderarr(0).split("@")(0).toInt
              case "SELL" => orderarr(1).split("@")(0).toInt
            }
            val itemmaterial = buyOrSell match {
              case "BUY" =>  Material.getMaterial(orderarr(0).split("@")(1).toUpperCase)
              case "SELL" => Material.getMaterial(orderarr(1).split("@")(1).toUpperCase)
            }
            val unitmaterial = buyOrSell match {
              case "BUY" => Material.getMaterial(orderarr(1).toUpperCase)
              case "SELL" => Material.getMaterial(orderarr(0).toUpperCase)
            }
            val order = buyOrSell match {
              case "BUY" => Order(OrderIO.nextid,player,unitmaterial,new ItemStack(itemmaterial,itemquantity),"BUY")
              case "SELL" => Order(OrderIO.nextid,player,unitmaterial,new ItemStack(itemmaterial,itemquantity),"SELL")
            }
            OrderIO.writeOrder(order)
            return "Successfully placed order" //+ order.toJson
          }catch{
            case e:Exception =>
            {
              e.printStackTrace
              return "Error:No order Placed"
            }
          }
        }
        case "buy" => {
          try{
            val material = new ItemStack(Material.getMaterial(args(0).toUpperCase),args(1).toInt)
            val item = new ItemStack(Material.getMaterial(args(2).toUpperCase),args(3).toInt)
            (0 until material.getAmount()).foreach(i=> OrderIO.writeOrder(Order(OrderIO.nextid,player,material.getType(),item,"BUY")))
            return "Successfully Placed Order"
          }catch {
            case e:Exception =>{
              e.printStackTrace
              return "Something went wrong, order not placed"
            }
          }
        }
        case "sell" => {
          try {
            val material = new ItemStack(Material.getMaterial(args(0).toUpperCase),args(1).toInt)
            val item = new ItemStack(Material.getMaterial(args(2).toUpperCase),args(3).toInt)
            (0 until material.getAmount()).foreach(i=> OrderIO.writeOrder(Order(OrderIO.nextid,player,material.getType(),item,"SELL")))
            return "Successfully Placed Order"
          }catch {
            case e:Exception =>{
              e.printStackTrace
              return "Something went wrong, order not placed"
            }
          }
        }
        case _ => "no matching command"
      }
      return response
    }
	 var response : String =
	   sender match{
	    case p : Player => {
        implicit val player=p
        playerCommand(p,cmd,label,args)
      }
      case _ => {
	      null
	    }
	  }


    sender.sendMessage(response)
   return true;
	}
}
