package minecraft;

import java.util.List
import scala.collection.JavaConverters._
import collection.mutable._

import org.bukkit.plugin.java.JavaPlugin;
import org.bukkit.command.CommandSender;
import org.bukkit.command.Command;
import org.bukkit.Material;

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
  import OrderBookConstants._
  val escrowIds = HashMap[Player,Array[ItemStack]]()
  override def onEnable() {
		getLogger().info("onEnable has been invoked!");
	}

	override def onDisable() {
		getLogger().info("onDisable has been invoked!");
	}

  override def onTabComplete(sender : CommandSender, cmd : Command, label : String, args : Array[String]) : java.util.List[String] = {
      val response = cmd.getName() match {
        case "$" => {

            val concatargs = args.foldLeft("")( (a,c)=> a + c)
            val args_ = concatargs.split("=>")
            //val res = Material.values()filter( m => m.isItem() && m.toString().contains(args(0).toUpperCase() ) )
            return Array("What you want => What you're offering","<amount>@<item> => <item>","<item> => <amount>@<item>").toBuffer.asJava
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
  def parseOrder(player:Player,args:Array[String]):Array[Order] = {
    val concatargs = args.foldLeft("")( (a,c) => a + c)
    val orderarr = concatargs.split("/")(0).split("=>")
    val volume_ = concatargs.split("/")
    val volume = if (volume_.size > 1) volume_(1).toInt else 1
    (0 until volume).foldLeft(Array[Order]())( (orderlist,index) => {
                            //1@dirt => cobblestone //selling unit cobblestone for one dirt
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
    orderlist :+ order
  })
  }
  def introduceOrder(player:Player,args:Array[String]): String = {
    import OrderBookConstants._
    val orders = parseOrder(player,args)
    val playerinv = player.getInventory()
    val escrowamount = if (orders.size > 0) orders(0).escrowitem.getAmount()*orders.size
                     else 0
    val escrowmaterial = if (orders.size > 0) orders(0).escrowitem.getType() else null
    if (!playerinv.contains(escrowmaterial,escrowamount)) return "Not enough " + escrowmaterial + " in inventory to supply order"

    //(filled orders, new orders)
    //var newOrders = 0
    var filledOrders = 0
    //val matchedorders = new HashMap[Order,Order]()
    val newOrders = ArrayBuffer[Order]()
    val neworderbook= orders.foldLeft(OrderIO.readOrderBook)( (orderbook,order) => {
      val matched = OrderMatch.findOrder(order,orderbook)
      if (matched != null) {
        filledOrders +=1
        val buyerinventory = order.player.getInventory()
        val sellerinventory = matched.player.getInventory()
        buyerinventory.removeItem(order.escrowitem)
        buyerinventory.addItem(order.filleditem)
        escrowIds.put(matched.player,escrowIds.getOrElse(matched.player,Array[ItemStack]()) :+ matched.filleditem)

        val orderbookfunc = (po:PlayerOrder) => if (po.id != matched.orderid) orderbook.f(po) else null
        OrderBook(orderbookfunc)
      }else{
        //if (OrderIO.writeOrder(order)){
        newOrders += order
        orderbook
      }
    })
    if (OrderIO.swapOrderBook(neworderbook)){
      newOrders.foreach( order => {
        val buyerinventory = order.player.getInventory()
        buyerinventory.removeItem(order.escrowitem)
        OrderIO.writeOrder(order)
      })
    }else{
      return "something went wrong"
    }
    // val metrics = orders.foldLeft((0,0))( (metrics,order) => {
    //   val matched = matchedorders.getOrElse(order,null)
    //   //attempt to fill order before making new one
    //   if(matched == null){
    //     //write order and move item to escrow
    //     OrderIO.writeOrder(order)
    //     val buyerinventory = order.player.getInventory()
    //     buyerinventory.removeItem(order.escrowitem)
    //     (metrics._1,metrics._2 + 1)
    //   }else{
    //     //fill order and move itemfills into escrow
    //     val success = OrderMatch.commitOrderFill(order,matched)
    //     if(success){
    //       val buyerinventory = order.player.getInventory()
    //       val sellerinventory = matched.player.getInventory()
    //       buyerinventory.removeItem(order.escrowitem)
    //       buyerinventory.addItem(order.filleditem)
    //       escrowIds.put(matched.player,escrowIds.getOrElse(matched.player,Array[ItemStack]()) :+ matched.filleditem)
    //       (metrics._1 + 1,metrics._2)
    //     }else{
    //       metrics
    //     }
    //   }
    // })
    return filledOrders + " Orders Filled, " + newOrders.size + "New Orders Placed"
    //return "found " + matchedorders.size.toString + " matching orders"
  }
	override def onCommand(sender : CommandSender, cmd : Command, label : String, args : Array[String]):Boolean = {

    def playerCommand(player: Player, cmd : Command, label : String, args : Array[String]):String ={
      val response = cmd.getName() match {
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
        }
        case "buy" => {
          try{
            val material = Material.getMaterial(args(0).toUpperCase)
            val orderlist = OrderIO.readOrders
            val validorders = orderlist.filter(order => {
              val validsellorder = order.item.getType() == material && order.buyOrSell == "SELL"
              val validbuyorder = order.material == material && order.buyOrSell == "BUY"
              validbuyorder || validsellorder
            })
            validorders.foldLeft("")( (a,c)=> {
              val str = c.toSymbol
              a + str + "\n"
            })
          }catch {
            case e:Exception =>{
              e.printStackTrace
              return "Something went wrong, order not placed"
            }
          }
        }
        case "sell" => {
          try {
            val material = Material.getMaterial(args(0).toUpperCase)
            val orderlist = OrderIO.readOrders
            val validorders = orderlist.filter(order => {
              val validbuyorder = order.item.getType() == material && order.buyOrSell == "BUY"
              val validsellorder = order.material == material && order.buyOrSell == "SELL"
              validbuyorder || validsellorder
            })
            validorders.foldLeft("")( (a,c)=> {
              val str = c.toSymbol
              a + str + "\n"
            })
          }catch {
            case e:Exception =>{
              e.printStackTrace
              return "Something went wrong, order not placed"
            }
          }
        }
        case "swap" => {
          try{
            // val order = parseOrder(player,args)
            // val playerinv = player.getInventory()
            // if (playerinv.contains(order.escrowitem)) return "Not enough " + order.escrowitem.getType() + " in inventory to supply order"
            // val matched = OrderMatch.findOrder(order)
            // val neworderbook = OrderMatch.fillOrder(order,matched)
            // val success = OrderIO.swapOrderBook(neworderbook)
            // if(success) "Order filled" else "Error: Order was not filled"
            ""
          }catch{
            case e:Exception => {
              e.printStackTrace
              "Something went wrong"
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

              if (success) escrowIds.put(player,escrowIds.getOrElse(player,Array[ItemStack]())
              .filter( j => j == i) )
            })
            "enjoy what you've earned"
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
