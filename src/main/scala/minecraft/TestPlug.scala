package minecraft;

import java.util.List

import java.io._
import java.util.UUID

import scala.collection.JavaConverters._
import collection.mutable._
import scala.io.Source


import org.bukkit.plugin.java.JavaPlugin;
import org.bukkit.command.CommandSender;
import org.bukkit.command.Command;
import org.bukkit.Material;

import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemStack;
import org.bukkit.inventory.PlayerInventory;
import org.bukkit.Bukkit;
import org.bukkit.event.Listener
// import org.bukkit.inventory.Packet250CustomPayload;
// import org.bukkit.inventory.Packet100OpenWindow;
// import org.bukkit.inventory.MerchantRecipeList;
// import org.bukkit.inventory.MerchantRecipe;


import minecraft.economy._
import minecraft.constants._
import minecraft.playerio._
//import org.apache.spark.sql._
trait MinecapListener extends Listener
class Minecap extends JavaPlugin{
  import OrderBookConstants._
  val escrow = Trade.escrow



  override def onEnable() {
    getServer().getPluginManager().registerEvents(MenuActions.menuActions(), this);
		getLogger().info("-----------------------On Enable Invoked!---------------------");
    // val server = Bukkit.getServer()
    // val escrowraw= Source.fromFile(OrderBookConstants.escrowDataLoc).getLines.toArray
    // escrowraw.foreach( e => {
    //   val args = e.split(",")
    //   val player = server.getPlayer(UUID.fromString(args(0)))
    //   val itemstack = new ItemStack(Material.getMaterial(args(1)), args(2).toInt)
    //   escrow.put(player,escrow.getOrElse(player,Array[ItemStack]()) :+ itemstack)
    // })
	}

	override def onDisable() {
		// getLogger().info("Backing up Escrow currently in memory");
    // val newEscrowData = escrow.keySet.foldLeft("")( (acc,p) => {
    //   val itemarray = escrow.getOrElse(p,Array[ItemStack]())
    //   val newplayerentry = itemarray.foldLeft("")( (acc,curr) => {
    //     val newentry = p.getUniqueId() + "," + curr.getType() + "," + curr.getAmount() + "\n"
    //     acc + newentry
    //   })
    //   acc + newplayerentry
    // })
    // val fw = new FileWriter(OrderBookConstants.escrowDataLoc)
    // fw.write(newEscrowData)
    // fw.close()
	}

  override def onTabComplete(sender : CommandSender, cmd : Command, label : String, args : Array[String]) : java.util.List[String] = {
      val response = cmd.getName() match {
        case "$" => {
            val concatargs = args.foldLeft("")( (a,c)=> a + c)
            val args_ = concatargs.split(OrderBookConstants.mapdelim)
            //val res = Material.values()filter( m => m.isItem() && m.toString().contains(args(0).toUpperCase() ) )
            return Array("What you want "+ OrderBookConstants.mapdelim + " What you're offering","<amount>" + OrderBookConstants.amountdelim+ "<item> "+ OrderBookConstants.mapdelim +" <item>","<item> "+ OrderBookConstants.mapdelim +" <amount>" + OrderBookConstants.amountdelim+ "<item>").toBuffer.asJava
        }
        case "sellers" => {
          args.size match{
            case 1 => {
              val res = Material.values().filter( m => m.isItem() && m.toString().contains(args(0).toUpperCase() ) )
              res.map( i => i.toString).toBuffer.asJava
            }
            case _ => Array[String]().toBuffer.asJava
          }
        }
        case "buyers" => {
          args.size match{
            case 1 => {
              val res = Material.values().filter( m => m.isItem() && m.toString().contains(args(0).toUpperCase() ) )
              res.map( i => i.toString).toBuffer.asJava
            }
            case _ => Array[String]().toBuffer.asJava
          }
        }
        case "lookup" => {
          args.size match{
            case 1 => {
              val res = Material.values().filter( m => m.isItem() && m.toString().contains(args(0).toUpperCase() ) )
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
    //todo: Use conversation class to prompt for orders
    def playerCommand(player: Player, cmd : Command, label : String, args : Array[String]):String ={
      val response = cmd.getName() match {
        //ToDo Errors: No @
        case "$" => { // 64@cobblestone=>dirt is a unitBuy of dirt
          try {       // dirt=>64@cobblestone is a unitSell of dirt
            Trade.introduceOrder(player,args)
          }catch{
            case e:Exception =>
            {
              e.printStackTrace
              return "Error:No order Placed. Check your item names"
            }
          }
        }//todo implement pagination on results
        case "sellers" => {
          Trade.orderListToString(Trade.filterSellers(player,args))("\n------SELL-ORDERS--------\n")
        }
        case "buyers" => {
          Trade.orderListToString(Trade.filterBuyers(player,args))("\n-------BUY-ORDERS--------\n")
        }
        case "claim" => {
          Trade.claim(player,args)
        }
        case "trade" => {
          import Trade._
          trade(player,args)
        }
        case _ => "no matching command"
      }
      return response
    }
	 val response : String =
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
