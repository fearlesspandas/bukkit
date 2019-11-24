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
    //val spark = SparkSesitem.toString + messagesion.builder().getOrCreate()
    //import spark.implicits._
    val orderbookloc_ = "/Users/minecraft/Public/minecraft-server/plugins/testplug/orders.json"
    implicit val orderbookloc = orderbookloc_
    def playerCommand(sender : CommandSender, cmd : Command, label : String, args : Array[String])(implicit player:Player):String ={
      val response = cmd.getName() match {
        case "$" => {
          try {
            val price = new ItemStack(Material.getMaterial(args(0).toUpperCase),args(1).toInt)
            val item = new ItemStack(Material.getMaterial(args(2).toUpperCase),args(3).toInt)
            val order = Order(OrderIO.nextid,player,price,item,args(3).toInt)
            //order.toJson
            OrderIO.writeOrder(order)
            return "Successfully placed order:" + order.toJson
          }catch{
            case e:Exception =>
            {
              e.printStackTrace
              return "Error:No order Placed"
            }
          }


        }
        case "buy" => {
          val orderbook = OrderIO.readOrderBook(orderbookloc_)
          val item = new ItemStack(Material.getMaterial(args(0).toUpperCase),1)
          //val amount = args(1).toInt
          val matchingorders = orderbook.orders.filter(o => o.item.getType() == item.getType())
          matchingorders.map(order => order.price.getType().toString() + ":" + order.price.getAmount() +  ":" + order.player.getPlayerListName() +  "\n").foldLeft("")( (a,c) => a + c )
        }
        case "sell" => {
          val orderbook = OrderIO.readOrderBook(orderbookloc_)
          val item = new ItemStack(Material.getMaterial(args(0).toUpperCase),1)
          //val amount = args(1).toInt
          val matchingorders = orderbook.orders.filter(o => o.price.getType() == item.getType())
          matchingorders.map(order => order.item.getType().toString() + ":" + order.item.getAmount() +  ":" + order.player.getPlayerListName() + "\n").foldLeft("")( (a,c) => a + c )
        }
        case _ => "no matching command"
      }
      return response
    }

    var player : Player =
      sender match{
       case p : Player => p.asInstanceOf[Player]
       case _ => {
         null
       }
     }
	 var response : String =
	   sender match{
	    case p : Player => {
        implicit val player=p
        playerCommand(sender,cmd,label,args)
      }
      case _ => {
	      null
	    }
	  }


    player.sendMessage(response)




   return true;
	}
}
