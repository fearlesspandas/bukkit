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
import org.bukkit.Bukkit;
// import org.bukkit.inventory.Packet250CustomPayload;
// import org.bukkit.inventory.Packet100OpenWindow;
// import org.bukkit.inventory.MerchantRecipeList;
// import org.bukkit.inventory.MerchantRecipe;


import minecraft.economy._
import minecraft.constants._
//import org.apache.spark.sql._

class Minecap extends JavaPlugin{
  import OrderBookConstants._
  val escrowIds = HashMap[Player,Array[ItemStack]]()
  override def onEnable() {
		getLogger().info("Reading in escrow file!");
    val server = Bukkit.getServer()
    val escrowraw= Source.fromFile(OrderBookConstants.escrowDataLoc).getLines.toArray
    escrowraw.foreach( e => {
      val args = e.split(",")
      val player = server.getPlayer(UUID.fromString(args(0)))
      val itemstack = new ItemStack(Material.getMaterial(args(1)), args(2).toInt)
      escrowIds.put(player,escrowIds.getOrElse(player,Array[ItemStack]()) :+ itemstack)
    })
	}

	override def onDisable() {
		getLogger().info("Backing up Escrow currently in memory");
    val newEscrowData = escrowIds.keySet.foldLeft("")( (acc,p) => {
      val itemarray = escrowIds.getOrElse(p,Array[ItemStack]())
      val newplayerentry = itemarray.foldLeft("")( (acc,curr) => {
        val newentry = p.getUniqueId() + "," + curr.getType() + "," + curr.getAmount() + "\n"
        acc + newentry
      })
      acc + newplayerentry
    })
    val fw = new FileWriter(OrderBookConstants.escrowDataLoc)
    fw.write(newEscrowData)
    fw.close()
	}

  override def onTabComplete(sender : CommandSender, cmd : Command, label : String, args : Array[String]) : java.util.List[String] = {
      val response = cmd.getName() match {
        case "$" => {
            val concatargs = args.foldLeft("")( (a,c)=> a + c)
            val args_ = concatargs.split(OrderBookConstants.mapdelim)
            //val res = Material.values()filter( m => m.isItem() && m.toString().contains(args(0).toUpperCase() ) )
            return Array("What you want => What you're offering","<amount>@<item> => <item>","<item> => <amount>@<item>").toBuffer.asJava
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
        case _ => Array[String]().toBuffer.asJava
    }
    return response
  }
  def parseOrder(player:Player,args:Array[String]):Array[Order] = {
    val concatargs = args.foldLeft("")( (a,c) => a + c)
    val orderarr = concatargs.split(OrderBookConstants.volumedelim)(0).split(OrderBookConstants.mapdelim)
    val volume_ = concatargs.split(OrderBookConstants.volumedelim)
    val volume = if (volume_.size > 1) volume_(1).toInt else 1
    (0 until volume).foldLeft(Array[Order]())( (orderlist,index) => {
                            //1@dirt => cobblestone //selling unit cobblestone for one dirt
    val buyOrSell = if (orderarr(0).contains(OrderBookConstants.amountdelim)) "BUY" else if (orderarr(1).contains(OrderBookConstants.amountdelim)) "SELL" else null
    val itemquantity = buyOrSell match {
      case "BUY" => orderarr(0).split(OrderBookConstants.amountdelim)(0).toInt
      case "SELL" => orderarr(1).split(OrderBookConstants.amountdelim)(0).toInt
    }
    val itemmaterial = buyOrSell match {
      case "BUY" =>  Material.getMaterial(orderarr(0).split(OrderBookConstants.amountdelim)(1).toUpperCase)
      case "SELL" => Material.getMaterial(orderarr(1).split(OrderBookConstants.amountdelim)(1).toUpperCase)
    }
    val unitmaterial = buyOrSell match {
      case "BUY" => Material.getMaterial(orderarr(1).toUpperCase)
      case "SELL" => Material.getMaterial(orderarr(0).toUpperCase)
    }
    val order = buyOrSell match {
      case "BUY" => Order(OrderIO.nextid + index,player,unitmaterial,new ItemStack(itemmaterial,itemquantity),"BUY")
      case "SELL" => Order(OrderIO.nextid + index,player,unitmaterial,new ItemStack(itemmaterial,itemquantity),"SELL")
    }
    orderlist :+ order
  })
  }
  def introduceOrder(player:Player,args:Array[String]): String = {
    import OrderBookConstants._
    val orders = parseOrder(player,args)
    if (orders.size == 0) return "No orders placed"
    val playerinv = player.getInventory()
    //define escrow
    val escrowamount = if (orders.size > 0) orders(0).escrowitem.getAmount()*orders.size
                     else 0
    val escrowmaterial = if (orders.size > 0) orders(0).escrowitem.getType() else null
    if (!playerinv.contains(escrowmaterial,escrowamount)) return "Not enough " + escrowmaterial + " in inventory to supply order"

    //Do order fill escrow
    val matched = OrderMatch.findOrder(orders(0)) // find all orders matching this time
    val numFilledOrders = scala.math.min( matched.size,orders.size)
    val numNewOrders = scala.math.max(orders.size - matched.size,0)

    (0 until numFilledOrders).foreach(i => {
      val order = orders(0) //all orders being introduced are expected to be the same type
      val matchedOrder = matched(i)
      val buyerinventory = order.player.getInventory()
      val sellerinventory = matchedOrder.player.getInventory()
      buyerinventory.removeItem(order.escrowitem)
      buyerinventory.addItem(order.filleditem)
      escrowIds.put(matchedOrder.player,escrowIds.getOrElse(matchedOrder.player,Array[ItemStack]()) :+ matchedOrder.filleditem)
      if (i == 0) matchedOrder.player.sendMessage(OrderIO.scrubString("Order filled! Go to GE and do /claim to retrieve your " + matchedOrder.filleditem.getType()))
    })
    //build new orderbook
    val currentOrderBook = OrderIO.readOrderBook
    val filledOrders = matched.slice(0,numFilledOrders).map( o => o.orderid)
    val orderbookfunc = (po:PlayerOrder) => if (filledOrders.contains(po.id)) null else currentOrderBook.f(po)

    if (OrderIO.swapOrderBook(OrderBook(orderbookfunc))){
      val newOrders = orders.drop(numFilledOrders)
      newOrders.foreach( order => {
        val buyerinventory = order.player.getInventory()
        buyerinventory.removeItem(order.escrowitem)
        OrderIO.writeOrder(order)
      })
    }else{
      return "Something failed while writing orders"
    }

    return numFilledOrders + " Orders Filled, " + numNewOrders + "New Orders Placed"
    //return "found " + matchedorders.size.toString + " matching orders"
  }
	override def onCommand(sender : CommandSender, cmd : Command, label : String, args : Array[String]):Boolean = {
    //todo: Use conversation class to prompt for orders
    def playerCommand(player: Player, cmd : Command, label : String, args : Array[String]):String ={
      val response = cmd.getName() match {
        //ToDo Errors: No @
        case "$" => { // 64@cobblestone=>dirt is a unitBuy of dirt
          try {       // dirt=>64@cobblestone is a unitSell of dirt
            introduceOrder(player,args)
          }catch{
            case e:Exception =>
            {
              e.printStackTrace
              return "Error:No order Placed"
            }
          }
        }//todo implement pagination on results
        case "sellers" => {
          try{
            val material = Material.getMaterial(args(0).toUpperCase)
            val orderlist = OrderIO.readOrders
            val validorders = orderlist.filter(order => {
              val validsellorder = order.item.getType() == material && order.buyOrSell == "SELL"
              val validbuyorder = order.material == material && order.buyOrSell == "BUY"
              validbuyorder || validsellorder
            }).sorted.reverse
            validorders.foldLeft( ("\n------SELL-ORDERS--------\n",null.asInstanceOf[Order],0) )( (a,c)=> {
              if(c.compare(a._2) != 0){
                val str = if(a._2 != null) a._2.toSymbol + " /" + a._3 +"\n" else "" +
                          (if (c == validorders.last) c.toSymbol + "\n" else "")
                (a._1 + str,c,1)
              }else{
                val oldvolume = a._3
                val newvolume = oldvolume+1
                val appendOrder = if(c == validorders.last) c.toSymbol +" /" + newvolume + "\n" else ""
                (a._1 + appendOrder,c,a._3+1)
              }
            })._1
          }catch {
            case e:Exception =>{
              e.printStackTrace
              return "Something went wrong, order not placed"
            }
          }
        }
        case "buyers" => {
          try {
            val material = Material.getMaterial(args(0).toUpperCase)
            val orderlist = OrderIO.readOrders
            val validorders = orderlist.filter(order => {
              val validbuyorder = order.item.getType() == material && order.buyOrSell == "BUY"
              val validsellorder = order.material == material && order.buyOrSell == "SELL"
              validbuyorder || validsellorder
            }).sorted
            if (validorders.size > 0) {
              validorders.foldLeft( ("\n------BUY-ORDERS--------\n",null.asInstanceOf[Order],0) )( (a,c)=> {
                if(c.compare(a._2) != 0){
                  val str = if(a._2 != null) a._2.toSymbol + " /" + a._3 +"\n" else "" +
                            (if (c == validorders.last) c.toSymbol + "\n" else "")
                  (a._1 + str,c,1)
                }else{
                  val oldvolume = a._3
                  val newvolume = oldvolume+1
                  val appendOrder = if(c == validorders.last) c.toSymbol +" /" + newvolume + "\n" else ""
                  (a._1 + appendOrder,c,a._3+1)
                }
              })._1
            }else{
              return ""
            }
          }catch {
            case e:Exception =>{
              e.printStackTrace
              return "Something went wrong, order not placed"
            }
          }
        }
        case "claim" => {
          try{
            val itempool = escrowIds.getOrElse(player,Array[ItemStack]())
            val inventory = player.getInventory()
            itempool.foreach( i => {
              val success = try{
                  inventory.addItem(i)
                  true
                } catch{
                  case e:Exception => false
                }

              if (success) escrowIds.put(player,escrowIds.getOrElse(player,Array[ItemStack]()).filter( j => j == i) )
            })
            "Enjoy what you've earned"
          }catch{
            case e:Exception => {
              e.printStackTrace
              "Something went wrong"
            }
          }
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
