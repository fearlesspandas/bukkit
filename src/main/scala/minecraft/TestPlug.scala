package minecraft;

import Orders.Order._
import minecraft.economy.OrderImpl
import org.bukkit.command.{Command, CommandSender}
import org.bukkit.entity.Player
import org.bukkit.inventory.ItemStack
import org.bukkit.plugin.java.JavaPlugin

import scala.collection.JavaConverters._
import minecraft.economy.OrderImpl._
import minecraft.playerio.OrderParser

class Minecap extends JavaPlugin{



  override def onEnable() {
		getLogger().info("Enabling Typical Order Matching System!");
//    val server = Bukkit.getServer()
//    val escrowraw= Source.fromFile(OrderBookConstants.escrowDataLoc).getLines.toArray
//    escrowraw.foreach( e => {
//      val args = e.split(",")
//      val player = server.getPlayer(UUID.fromString(args(0)))
//      val itemstack = new ItemStack(Material.getMaterial(args(1)), args(2).toInt)
//      escrowIds.put(player,escrowIds.getOrElse(player,Array[ItemStack]()) :+ itemstack)
//    })
	}

	override def onDisable() {
		getLogger().info("Shutting down Typical Order Matching System");
//    val newEscrowData = escrowIds.keySet.foldLeft("")( (acc,p) => {
//      val itemarray = escrowIds.getOrElse(p,Array[ItemStack]())
//      val newplayerentry = itemarray.foldLeft("")( (acc,curr) => {
//        val newentry = p.getUniqueId() + "," + curr.getType() + "," + curr.getAmount() + "\n"
//        acc + newentry
//      })
//      acc + newplayerentry
//    })
//    val fw = new FileWriter(OrderBookConstants.escrowDataLoc)
//    fw.write(newEscrowData)
//    fw.close()
	}

  override def onTabComplete(sender : CommandSender, cmd : Command, label : String, args : Array[String]) : java.util.List[String] = {
      val response = cmd.getName() match {
        case "$" => {
            return Array("What you want "+ OrderParser.mapdelim + " What you're offering","<amount>" + OrderParser.voldelim+ "<item> "+ OrderParser.mapdelim +" <item>","<item> "+ OrderParser.mapdelim +" <amount>" + OrderParser.voldelim+ "<item>").toBuffer.asJava
        }
        case _ => Array[String]().toBuffer.asJava
    }
    return response
  }

	override def onCommand(sender : CommandSender, cmd : Command, label : String, args : Array[String]):Boolean = {
    //todo: Use conversation class to prompt for orders
    def playerCommand(plyr: Player, cmd : Command, label : String, args : Array[String]):String ={
      val response = cmd.getName() match {
        //ToDo Errors: No @
        case "$" => {
          try {
            val nextOrder = OrderParser.fromArgs(player(plyr.getUniqueId().toString,plyr),args)
            nextOrder match{
              case Some(x:Either[order[_,_],order[_,_]]) => {
                  x match {
                    case Left(_:order[_,_]) => {
                        val o = x.left.get.asInstanceOf[order[itemstack,material]]
                        val hasEscrow = plyr.getInventory.contains(o.i.m,o.remaining)
                        val success = if(hasEscrow) updateDataModel(o) else false
                        if (hasEscrow) {
                          val currinv = plyr.getInventory().getContents()
                          val (_,updatedInv) = currinv.foldLeft((o.remaining,Array[ItemStack]()))( (acc,curr) => {
                            val (need,inv) = acc
                            if(curr != null && curr.getType() == o.i.m && need > 0) (need - curr.getAmount(),inv :+ new ItemStack(curr.getType(),scala.math.max(curr.getAmount() - need,0)))
                            else if(curr == null)(need,inv)
                            else (need,inv :+ curr )
                          } )
                          plyr.getInventory().setContents(updatedInv)
                          plyr.updateInventory()
                          if (success) "Order Placed" else "No order placed"
                        }else s"Not enough ${o.i.m} in inventory to fill order"
                    }
                    case Right(_:order[_,_]) =>{
                        val o = x.right.get.asInstanceOf[order[material,itemstack]]
                        val hasEscrow = plyr.getInventory.contains(o.i.m,o.i.a * o.remaining)
                        val success = if(hasEscrow) updateDataModel(o) else false
                        if (hasEscrow) {
                          val currinv = plyr.getInventory().getContents()
                          val (_,updatedInv) = currinv.foldLeft((o.remaining,Array[ItemStack]()))( (acc,curr) => {
                            val (need,inv) = acc
                            if(curr != null && curr.getType() == o.i.m && need > 0) (need - curr.getAmount(),inv :+ new ItemStack(curr.getType(),scala.math.max(curr.getAmount() - need,0)))
                            else if(curr == null)(need,inv)
                            else (need,inv :+ curr )
                          } )
                          plyr.getInventory().setContents(updatedInv)
                          plyr.updateInventory()
                          if (success) "Order Placed" else "No order placed"
                        }else s"Not enough ${o.i.m} in inventory to fill order"
                    }
                  }
              }
              case _ => "Order could not be processed from args"
            }
          }catch{
            case e:Exception =>
            {
              e.printStackTrace
              return "Error:No order Placed. Check your item names"
            }
          }
        }//todo implement pagination on results
        case "browse" => {
          getOrderbook()
        }
        case "escrow" => {
          getEscrow(player(plyr.getUniqueId().toString,plyr))
        }
        case "claim" => {
          val inv = plyr.getInventory()
          val escrow =
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
