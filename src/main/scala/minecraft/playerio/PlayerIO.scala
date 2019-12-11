package minecraft.playerio

import scala.collection.mutable.HashMap
import java.util.UUID
import org.bukkit.inventory.{ItemStack,Inventory}
import org.bukkit.entity.{Player,HumanEntity}
import org.bukkit.Material
import org.bukkit.event._
import org.bukkit.event.inventory._
import org.bukkit.Bukkit
import minecraft.economy._
import minecraft.constants._
object Trade {
    import OrderBookConstants._
    val playerInvStorage = HashMap[UUID,Array[ItemStack]]()
    val escrow = HashMap[UUID,Array[ItemStack]]()
    def trade(player:Player,args:Array[String]):String = {
      try{
        if (args(0) == "off"){
          player.getInventory.setStorageContents(Array[ItemStack]())
          val stored = playerInvStorage.getOrElse(player.getUniqueId,Array[ItemStack]())
          val inventory = player.getInventory()
          if (stored.size == 0) return "No items in storage"
          stored.foreach(i => if(i != null) inventory.addItem(i))
          playerInvStorage.put(player.getUniqueId,Array[ItemStack]())
          MenuActions.menuState.put(player,"")
          return "Take your shit back"
        }
        else if (args(0) == "buy"){
          val items = filterSellers(player,args.drop(1))
          val inventory = player.getInventory
          playerInvStorage.put(player.getUniqueId,inventory.getStorageContents())
          inventory.setStorageContents(Array[ItemStack]())
          val itemstring = (i:ItemStack) => if (i == null) "" else i.getType().toString + " " + i.getAmount().toString + "\n"
          val escrowconfirm = "Items put into escrow: " + playerInvStorage.getOrElse(player.getUniqueId,Array[ItemStack]()).foldLeft("")( (a,c) => a + itemstring(c) )
          val itemString = (i:ItemStack) => OrderIO.scrubString(i.getType() + ":" + i.getAmount())
          val escrowitem_meta_price = (o:Order) => {
            val i = o.escrowitem
            val meta = i.getItemMeta()
            meta.setDisplayName(itemString(o.filleditem))
            if(i.setItemMeta(meta)) i else null.asInstanceOf[ItemStack]
          }
          items.foreach(o => if(o != null) inventory.addItem( escrowitem_meta_price(o) ))
          MenuActions.menuState.put(player,"BUY")
          return ""
        }
        else "Improper Args [on,off]"

      }catch{
        case e:Exception =>{
          e.printStackTrace
        }
        return "Error"
      }
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
        case "BUY" => Order(OrderIO.nextid + index,player.getUniqueId(),unitmaterial,new ItemStack(itemmaterial,itemquantity),"BUY")
        case "SELL" => Order(OrderIO.nextid + index,player.getUniqueId(),unitmaterial,new ItemStack(itemmaterial,itemquantity),"SELL")
      }
      orderlist :+ order
    })
    }
    def introduceOrder(player:Player,args:Array[String]): String = {
      import OrderBookConstants._
      val orders = parseOrder(player,args)
      //Do order fill escrow
      introduceOrders(player,orders)

    }
    def introduceOrders(player:Player,orders:Array[Order],inv: MockInventory = null): String = {
      implicit val player_ = player
      if (orders.size == 0) return "No orders placed"

      //val playerinv = if (inv == null) player.getInventory() else inv
      val currentinv = player.getInventory().getStorageContents().filter( i => i != null)

      val playerinv = if (inv == null) MockInventory(player,currentinv) else inv
      //player.sendMessage( playerinv.items.foldLeft("")( (a,c) => a + c.getType().toString() + c.getAmount().toString() + "\n"))
      //define escrow
      val escrowamount = if (orders.size > 0) orders(0).escrowitem.getAmount()*orders.size
                       else 0
      val escrowmaterial = if (orders.size > 0) orders(0).escrowitem.getType() else null.asInstanceOf[Material]
      // val notenough = if (inv != null) !playerinv.asInstanceOf[MockInventory].contains(escrowmaterial,escrowamount)
      //                 else !playerinv.asInstanceOf[Inventory].contains(escrowmaterial,escrowamount)
      val notenough = !playerinv.contains(escrowmaterial,escrowamount)

      if (notenough) return "Not enough " + escrowmaterial + " in inventory to supply order"
      val matched = OrderMatch.findOrder(orders(0)) // find all orders matching this time
      val numFilledOrders = scala.math.min( matched.size,orders.size)
      val numNewOrders = scala.math.max(orders.size - matched.size,0)

      val newinventory = (0 until numFilledOrders).foldLeft(currentinv)( (newinv,i) => {
        val order = orders(0) //all orders being introduced are expected to be the same type
        val matchedOrder = matched(i)
        val remaining = matchedOrder.escrowitem.getAmount - order.filleditem.getAmount
        val surplus = new ItemStack(matchedOrder.escrowitem.getType,remaining)//surplus in escrow goes back to player
        escrow.put(matchedOrder.player,escrow.getOrElse(matchedOrder.player,Array[ItemStack]()) :+ order.escrowitem :+ surplus)//add whatever filling player was willing to pay (at least what was asked)
        val receivingplayer = Bukkit.getServer().getPlayer(matchedOrder.player)
        if (i == 0 && receivingplayer != null) receivingplayer.sendMessage(OrderIO.scrubString("Order filled! Go to GE and do /claim to retrieve your " + matchedOrder.filleditem.getType()))
        MockInventory(player,newinv).removeItem(order.escrowitem) :+ order.filleditem
      })
      playerinv.refreshPlayerInventory(newinventory)
      //build new orderbook
      val currentOrderBook = OrderIO.readOrderBook
      val filledOrders = matched.slice(0,numFilledOrders).map( o => o.orderid)
      val orderbookfunc = (po:PlayerOrder) => if (filledOrders.contains(po.id)) null else currentOrderBook.f(po)

      if (OrderIO.swapOrderBook(OrderBook(orderbookfunc))){
        val newOrders = orders.drop(numFilledOrders)
        val postfillInventory = newOrders.foldLeft(newinventory)( (postfillinv,order) => {
          //val buyerinventory = order.player.getInventory()
          OrderIO.writeOrder(order)
          MockInventory(player,postfillinv).removeItem(order.escrowitem)
        })
        playerinv.refreshPlayerInventory(postfillInventory)
        }
      else{
          return "Something failed while writing orders"
        }
        return numFilledOrders + " Orders Filled, " + numNewOrders + "New Orders Placed"
        //return "found " + matchedorders.size.toString + " matching orders"
    }
    def filterSellers(player: Player,args: Array[String]): Array[Order] = {
      try{
        val filterargs = "" + args.foldLeft("")( (a,c) => a + c)
        val orderlist = OrderIO.readOrders
        orderlist.filter(order => {
          val validsellorder = order.item.getType().toString().contains(filterargs.toUpperCase) && order.player != player.getUniqueId() && order.buyOrSell == "SELL"
          val validbuyorder = order.material.toString().contains(filterargs.toUpperCase) && order.player != player.getUniqueId() && order.buyOrSell == "BUY"
          validbuyorder || validsellorder
        }).sorted.reverse.toArray

      }catch {
        case e:Exception =>{
          e.printStackTrace
        }
        return Array[Order]()
      }
    }
    def filterBuyers(player: Player,args: Array[String]): Array[Order] = {
      try {
        val filterargs =  "" + args.foldLeft("")( (a,c) => a + c)
        val orderlist = OrderIO.readOrders
        orderlist.filter(order => {
          val validbuyorder = order.item.getType().toString().contains(filterargs.toUpperCase) && order.player != player.getUniqueId() && order.buyOrSell == "BUY"
          val validsellorder = order.material.toString().contains(filterargs.toUpperCase) && order.player != player.getUniqueId() && order.buyOrSell == "SELL"
          validbuyorder || validsellorder
        }).sorted.toArray

      }catch {
        case e:Exception =>{
          e.printStackTrace
          return Array[Order]()
        }
      }
    }
    def filterPlayerOrders(player: Player,args: Array[String]): Array[Order] = {
      try {
        val orderlist = OrderIO.readOrders
        orderlist.filter(order => {
          order.player == player.getUniqueId()
        }).sorted.toArray

      }catch {
        case e:Exception =>{
          e.printStackTrace
          return Array[Order]()
        }
      }
    }
    def orderListToString(orders:Array[Order])(startstr:String): String = {
      orders.foldLeft( (startstr,null.asInstanceOf[Order],0) )( (a,c)=> {
        if(c.compare(a._2) != 0){
          val str = if(a._2 != null) a._2.toSymbol + " "+ OrderBookConstants.volumedelim +" " + a._3 +"\n" else "" +
                    (if (c == orders.last) c.toSymbol + "\n" else "")
          (a._1 + str,c,1)
        }else{
          val oldvolume = a._3
          val newvolume = oldvolume+1
          val appendOrder = if(c == orders.last) c.toSymbol +" "+ OrderBookConstants.volumedelim +" " + newvolume + "\n" else ""
          (a._1 + appendOrder,c,a._3+1)
        }
      })._1
    }
    def claim(player:Player,args:Array[String]):String = {
      try{
        val itempool = escrow.getOrElse(player.getUniqueId(),Array[ItemStack]())
        val inventory = player.getInventory()
        val successfullclaims = itempool.foldLeft(Array[ItemStack]())( (items,i) => {
          try{
              inventory.addItem(i)
              items :+ i
            } catch{
              case e:Exception => items
            }
        })
        val newescrow = escrow.getOrElse(player.getUniqueId(),Array[ItemStack]()).filter( j => !successfullclaims.contains(j) )
        escrow.put(player.getUniqueId(), newescrow)
        "Enjoy what you've earned"
      }catch{
        case e:Exception => {
          e.printStackTrace
          "Something went wrong"
        }
      }
    }
    def cancel(player:Player,args:Array[String]):Array[Order] = {
      val cancellations = parseOrder(player,args)
      val orderbook = OrderIO.readOrders
      if (cancellations.size == 0) return cancellations
      val cancelled = orderbook.foldLeft(Array[Order]())( (tobecancelled,order) => {
        if(tobecancelled.size < cancellations.size && order.compare(cancellations(0))== 0 && order.player == cancellations(0).player ) {
          tobecancelled :+ order
        }else {
          tobecancelled
        }
      })
      val cancelledids = cancelled.map(o=> o.orderid)
      val currentorderbook = OrderIO.readOrderBook

      val orderbookfunc = (po:PlayerOrder) => if (cancelledids.contains(po.id)) null else currentorderbook.f(po)
      if(OrderIO.swapOrderBook(OrderBook(orderbookfunc))){
        val inventory = player.getInventory()
        val playerescrow = (p:Player) => escrow.getOrElse(p.getUniqueId(),Array[ItemStack]())
        val addescrow = (item:ItemStack) => playerescrow(player) :+ item
        val newescrow = playerescrow(player) ++: cancelled.map( o => o.escrowitem)
        escrow.put(player.getUniqueId(),newescrow)
        cancelled
      }else{
        player.sendMessage("Error swapping order book during cancel")
        Array[Order]()
      }
    }

}
case class MockInventory(player:Player,items_arr:Array[ItemStack]){
  //Trade.playerInvStorage.put(player.getUniqueId(),items_arr)
  val items = items_arr.filter(i => i != null)
  def contains(m:Material,n:Int):Boolean = {
    val teststack = new ItemStack(m,n)
    val itemtotal = items.foldLeft(0)( (a,c) => if (c.getType() == teststack.getType() && c.getItemMeta() == teststack.getItemMeta()) a + c.getAmount() else a)
    itemtotal >= n
  }
  def addItem(i:ItemStack)(implicit player:Player): Array[ItemStack] = {
    items :+ i
  }
  def removeItem(item:ItemStack)(implicit player:Player): Array[ItemStack] = {
    val start = (items,item.getAmount())
    // val filterfunc = (i:ItemStack)
    val inventorystock = items.filter( i => i.getType() == item.getType() && item.getItemMeta() == i.getItemMeta())
    inventorystock.foldLeft(start)( (remaining,invitem) => {
      if(remaining._2 > invitem.getAmount()) (remaining._1.filter(j => j!= invitem),remaining._2 - invitem.getAmount())
      else (remaining._1.filter(j => j!= invitem) :+ new ItemStack(invitem.getType(),invitem.getAmount() - remaining._2),0)
    })._1
  }
  def refreshPlayerInventory(itemstack:Array[ItemStack]){
    val inventory = player.getInventory()
    inventory.setStorageContents(Array[ItemStack]())
    itemstack.foreach( i => inventory.addItem(i))
  }
}
object MenuActions {
  val menuState = new HashMap[Player,String]()
  def menuActions() = new MenuActions
  @EventHandler
  def onInventoryClick(ie:InventoryClickEvent){
    //left click
    //getitemeta to get price
    //put in order for item at price

    //handleLeftClick(ie)

    //right click
    //getitemeta to get price
    //put in order for item at price volume min(64,remaining)
  }

  def handleLeftClick(ie:InventoryClickEvent){

     val player = ie.getWhoClicked().asInstanceOf[Player]
     menuState.getOrElse(player,"") match{
      case "BUY" => {
        player.sendMessage("Event clicked")
        Trade.playerInvStorage.getOrElse(player.getUniqueId,Array[ItemStack]()).foreach(i => player.sendMessage(i.getType.toString))
        val escrowitem = ie.getCurrentItem()
        val itemname = if (escrowitem!= null ) escrowitem.getItemMeta.getDisplayName else null
        if(itemname == null) return
        val filleditem_name = itemname.split(":")(0)
        val filleditem_amount = itemname.split(":")(1).toInt
        //player.sendMessage("item " + filleditem_name + " amount "  + filleditem_amount)
        val filleditem = new ItemStack(Material.getMaterial(filleditem_name.toUpperCase),filleditem_amount)
        val order = Order(OrderIO.nextid,player.getUniqueId(),escrowitem.getType,filleditem,"SELL")
        //player.sendMessage(order.toSymbol)
        val mockInv = MockInventory(player,Trade.playerInvStorage.getOrElse(player.getUniqueId,null.asInstanceOf[Array[ItemStack]]))
        player.sendMessage(Trade.introduceOrders(player,Array(order), mockInv ) )
        player.sendMessage(order.toSymbol)
      }
      case _ => {}
    }
  }

}
import org.bukkit.event.Listener
class MenuActions extends Listener {
  import MenuActions._
}
