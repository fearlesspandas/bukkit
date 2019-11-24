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
    implicit val orderbookloc = "testplug/orderbook.json"
    def playerCommand(sender : CommandSender, cmd : Command, label : String, args : Array[String])(implicit player:Player):Boolean ={
      val response = cmd.getName() match {
        case "$" => {
          val price = new ItemStack(Material.getMaterial(args(0)),args(1).toInt)
          val item = new ItemStack(Material.getMaterial(args(2)),args(3).toInt)
          OrderWriter.writeOrder(OrderWriter.nextid,player,price,item,args(3).toInt).toString
        }
        case _ => "Unrecognized command"
      }
      return true
    }
	 var player : Player =
	   sender match{
	    case p : Player => p.asInstanceOf[Player]
	    case _ => {
	      null
	    }
	  }


    //player.sendMessage("isplayer")

    //val  inv = player.getInventory()
    //val item = inv.getItemInMainHand()
    //val test = Seq(0 to 10:_*)

    //var message = test.foldLeft("")((a,c) => a + c.toString )
    // test.forEach( (i) => {
    //   message += i
    // })
    //player.sendMessage(response)


   return true;
	}
}
