package minecraft;

import scala.collection.JavaConverters._
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

	override def onCommand(sender : CommandSender, cmd : Command, label : String, args : Array[String]):Boolean = {
    //val spark = SparkSesitem.toString + messagesion.builder().getOrCreate()
    //import spark.implicits._
    implicit val orderbookloc = "/Users/minecraft/Public/minecraft-server/plugins/testplug/orders.json"
    def playerCommand(sender : CommandSender, cmd : Command, label : String, args : Array[String])(implicit player:Player):String ={
      val response = cmd.getName() match {
        case "$" => {
          try {
            val price = new ItemStack(Material.getMaterial(args(0).toUpperCase),args(1).toInt)
            val item = new ItemStack(Material.getMaterial(args(2).toUpperCase),args(3).toInt)
            val order = Order(OrderWriter.nextid,player,price,item,args(3).toInt)
            order.toJson
            //OrderWriter.writeOrder(order).toString
          }catch{
            case e:Exception =>
            {
              e.printStackTrace
              return "Error:No order Placed"
            }
          }


        }
        case _ => "no command"
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
