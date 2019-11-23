package minecraft;

import scala.collection.JavaConverters._
import org.bukkit.plugin.java.JavaPlugin;
import org.bukkit.command.CommandSender;
import org.bukkit.command.Command;
import org.bukkit.Material;
import org.bukkit.entity.Player;
import org.bukkit.entity.Player;

//
// import org.bukkit.inventory.Packet250CustomPayload;
// import org.bukkit.inventory.Packet100OpenWindow;
// import org.bukkit.inventory.MerchantRecipeList;
// import org.bukkit.inventory.MerchantRecipe;
 import org.bukkit.inventory.ItemStack;


class MyNewPlugin extends JavaPlugin{

  override def onEnable() {
		getLogger().info("onEnable has been invoked!");
	}

	override def onDisable() {
		getLogger().info("onDisable has been invoked!");
	}

	override def onCommand(sender : CommandSender, cmd : Command, label : String, args : Array[String]):Boolean = {

	 var player : Player =
	   sender match{
	    case p : Player => p.asInstanceOf[Player]
	    case _ => {
	      null
	    }
	  }

    player.sendMessage("isplayer")

    val  inv = player.getInventory()
    val item = inv.getItemInMainHand()
    val test = Seq(0 to 10:_*)

    var message = test.foldLeft("")((a,c) => a + c.toString )
    // test.forEach( (i) => {
    //   message += i
    // })
    player.sendMessage(item.toString + message)
   // if (cmd.getName().equalsIgnoreCase("basic")) { // If the player typed /basic then do the following, note: If you only registered this executor for one command, you don't need this
   //   sender.sendMessage("WARNING:THIS SHIT'S GONNA BLOW:");
   //   //val m = args.foldLeft("")( (a,c)=> a + " , " + c)
   //   //val t = args(0)
   //   //sender.sendMessage(t)
   //   return true;
   // }

   return true;
	}
}
