package minecraft.economy
import Orders.Order._

import Typical.impl.{data, myprovider}
import Typical.implicits.implicits._
import org.bukkit.Material
import org.bukkit.entity.Player
import org.bukkit.inventory.ItemStack

object OrderImpl {
  class unitSellQueue extends dataQueue[itemstack,material]
  class unitSell extends order[itemstack,material](it,mat,10,"me")
  case class itemstack(m:Material,a:Int) extends ItemStack(m,a) with GloballyOrdered[itemstack]{
    override def toString = m.toString + ":" + a
    def compareAny(that:Any): Int = {
      that match {
        case t:itemstack => {
          t.m match{
            case this.m => {
              t.a match{
                case this.a => 0
                case _ if this.a < t.a => 1
                case _ if this.a > t.a => -1
              }
            }
            case _ => -2
          }
        }
        case _ => -2
      }
    }
  }
  case class material(m:Material) extends GloballyOrdered[material] {
    override def compareAny(that: Any): Int = {
      that match{
        case tht:material if tht.m == this.m => 0
        case  _=> -1
      }
    }
  }
  case class player(id:String,p:Player) extends GloballyOrdered[player]{
    override def compare(that: player): Int = {
      if (that.id == this.id) 0 else -1
    }

    override def compareAny(that: Any): Int = ???
  }
  val diamond = Material.getMaterial("DIAMOND")
  val dirt = Material.getMaterial("DIRT")
  val it = itemstack(diamond,10)
  val it2 = itemstack(diamond,5)
  val mat = material(Material.getMaterial("DIRT"))
  var dat = data[
    orderbook[itemstack,material]
      with matching[itemstack,material]
      with matching[material,itemstack]
      with orderbook[material,itemstack]
      with escrowadd[itemstack,material]
      with escrowadd[material,itemstack]
  ](myprovider
      .register[orderbook[itemstack,material]]
      .register[orderbook[material,itemstack]]
      .register[escrowadd[material,itemstack]]
      .register[escrowadd[itemstack,material]]
  ).dataset


  def updateDataModel(o:order[_,_]) = {
    OrderImpl.dat = o match {
      case order(_:material,_:itemstack,_,_) => rawType[material,itemstack](OrderImpl.dat,o.asInstanceOf[order[material,itemstack]])
      case order(_:itemstack,_:material,_,_) => rawType[itemstack,material](OrderImpl.dat,o.asInstanceOf[order[itemstack,material]])
      case _ => OrderImpl.dat
    }
  }

  def main(args: Array[String]): Unit = {
    updateDataModel(order[itemstack,material](it,mat,10,"me"))
    println(OrderImpl.dat.fetch[orderbooktype[itemstack,material],orderbook[itemstack,material]].typedInitVal)
    println(OrderImpl.dat.fetch[orderbooktype[material,itemstack],orderbook[material,itemstack]].typedInitVal)
    println("////////////////////////////////////////////////////////////////////////////////")
    updateDataModel(order[material,itemstack](mat,it,15,"you"))
    println(OrderImpl.dat.fetch[orderbooktype[itemstack,material],orderbook[itemstack,material]].typedInitVal)
    println(OrderImpl.dat.fetch[orderbooktype[material,itemstack],orderbook[material,itemstack]].typedInitVal)
    println("////////////////////////////////////////////////////////////////////////////////")
    updateDataModel(order[itemstack,material](it,mat,10,"me"))
    println(OrderImpl.dat.fetch[orderbooktype[itemstack,material],orderbook[itemstack,material]].typedInitVal)
    println(OrderImpl.dat.fetch[orderbooktype[material,itemstack],orderbook[material,itemstack]].typedInitVal)
    println("ESCROW//////////////////////////////////")
    println(OrderImpl.dat.fetch[escrowtype[itemstack,material],escrowadd[itemstack,material]].typedInitVal)
    println(OrderImpl.dat.fetch[escrowtype[material,itemstack],escrowadd[material,itemstack]].typedInitVal)

  }
}
