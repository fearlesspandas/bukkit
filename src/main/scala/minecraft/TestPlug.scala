package minecraft;

import java.io.{FileOutputStream, PrintWriter}

import Orders.Order._
import minecraft.economy.OrderImpl
import org.bukkit.command.{Command, CommandSender}
import org.bukkit.entity.Player
import org.bukkit.inventory.ItemStack
import org.bukkit.plugin.java.JavaPlugin

import scala.collection.JavaConverters._
import minecraft.economy.OrderImpl._
import minecraft.playerio.OrderParser
import org.bukkit.Material

import scala.collection.immutable.HashMap
import scala.io.Source
import scala.collection.JavaConverters._

class Minecap extends JavaPlugin{



  override def onEnable() {
		getLogger().info("Enabling Typical Order Matching System!");
    val defaultorderbookOpt = Source.fromFile("plugins/orderdata/orders.json").getLines().filter(l => l !=null && l != "" && l != "\n").toSeq.toOrderMap()
    val defaultorderbook = if(defaultorderbookOpt.isDefined) defaultorderbookOpt.get else HashMap[Any,Seq[order]]()
    OrderImpl.initializeData(defaultorderbook)
	}

	override def onDisable() {
		getLogger().info("Shutting down Typical Order Matching System");
    val outpath = "plugins/orderdata/orders.json"
    val res = getcurrentbook().jsonMap()
    val append = false
    try{
      val pw = new PrintWriter(new FileOutputStream(outpath,append))
      if (append) pw.append(res) else pw.write(res)
      pw.close()
    }catch{
      case e:Exception => e.printStackTrace()
    }
	}

  override def onTabComplete(sender : CommandSender, cmd : Command, label : String, args : Array[String]) : java.util.List[String] = {
      val response = cmd.getName() match {
        case "$" => {
            return Array(
              "What you want "+ OrderParser.mapdelim + " What you're offering",
              "<amount>" + OrderParser.quantitydelim+ "<item> "+ OrderParser.mapdelim + s" <item> ${OrderParser.voldelim} <num_orders>",
              "<item> "+ OrderParser.mapdelim +" <amount>" + OrderParser.quantitydelim + s"<item> ${OrderParser.voldelim} <num_orders>"
            ).toBuffer.asJava
        }
        case "market" => Material.values()
          .filter(_.toString.contains(args(0)))
          .map(_.toString)
          .toBuffer.asJava
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
              case Some(x:Either[OrderParser.unitsellorder,OrderParser.unitbuyorder]) => {
                  x match {
                    case Left(_:order) => {
                        val o = x.left.get
                        val hasEscrow = plyr.getInventory.contains(o.i.m,o.remaining)
                        if (hasEscrow) {
                          val currinv = plyr.getInventory().getContents()
                          val totalitems = new ItemStack(o.i.toItemstack.getType(),o.remaining * o.i.toItemstack.getAmount)
                          plyr.getInventory().removeItem(totalitems)
                          plyr.updateInventory()
                          addOrder(o)
                        }else s"Not enough ${o.i.m} in inventory to fill order"
                    }
                    case Right(_:order) =>{
                        val o = x.right.get
                        val hasEscrow = plyr.getInventory.contains(o.i.m,o.i.a * o.remaining)
                        if (hasEscrow) {
                          val totalitems = new ItemStack(o.i.toItemstack.getType(),o.remaining * o.i.toItemstack.getAmount)
                          plyr.getInventory().removeItem(totalitems)
                          plyr.updateInventory()
                          addOrder(o)
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
          """--------------ALL ORDERS-----------------""" +
          getOrderbook()
        }
        case "fullbook" => {
          getcurrentbook().toString
        }
        case "market" => {
          val input = args(0).toUpperCase()
          val book = getcurrentbook()
          s"""---------$input MARKET ORDERS----------- """ +
          book.values.flatMap(x => x)
            .filter(x => x.i.toString.contains(input) || x.p.toString.contains(input) )
            .toSeq
            .sortWith((o1,o2) => o1.p.compareAny( o2.p) >=0 && o1.i.compareAny(o2.i) >= 0 )
            .filter(_.remaining>0).foldLeft("")((acc,curr) => acc + s"\nItem:${curr.i},price:${curr.p},Remaining:${curr.remaining}")
            .toString
        }
        case "escrow" => {
          s""" ------Items pending----- do /claim to retrieve------""" +
          getEscrow().getOrElse(plyr.getUniqueId.toString,Seq())
            .sortWith((o1,o2) => o1.p.compareAny( o2.p) >=0)
            .foldLeft("")((acc,curr) => acc + s"\nItem:${curr.p},Remaining:${curr.amt}")
            .toString
        }
        case "claim" => {
          val playerinv = plyr.getInventory()
          val inv = plyr.getInventory()
          val esc = OrderImpl.getEscrow()
          val unallocatedMaps = esc.getOrElse(plyr.getUniqueId.toString,Seq()).map( x => x match {
            case Fill(m,amt) if m.isInstanceOf[material] =>
              val newitemstack = new ItemStack(m.asInstanceOf[material].m,amt)
              playerinv.addItem(newitemstack)
            case Fill(i,amt) if i.isInstanceOf[itemstack] =>
              val item = i.asInstanceOf[itemstack]
              val newitemstack = new ItemStack(item.m,amt*item.a)
              val unallocated = playerinv.addItem(newitemstack)
              unallocated
          })
          val totalunallocated = unallocatedMaps.map(x => x.asScala)
            .map(x => x.values)
            .flatMap(x => x)
            .map(i => Fill(itemstack(i.getType,i.getAmount),i.getAmount))
              .groupBy(_.p)
              .map({
                case (_,fills) => fills.foldLeft(fills.head)((a,c) => Fill(a.p,a.amt + c.amt))
              }).toSeq
            println(s"totalunallocated:$totalunallocated")
            val updatedescrow = esc.updated(plyr.getUniqueId().toString(),totalunallocated)
            println(s"UpdatedEscrow:$updatedescrow")
            addEscrow(updatedescrow)
          "Enjoy your items"
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
