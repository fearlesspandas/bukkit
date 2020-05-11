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
        case mat:material => {
          mat.m match{
            case this.m if(this.a == 1) => 0
            case _ => -1
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
        case tht:itemstack if (tht.m == this.m && tht.a > 0) => 0
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
  val diamond = Material.getMaterial("ARROW")
  val dirt = Material.getMaterial("DIRT")
  val it = itemstack(diamond,1)
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


  def updateDataModel(o:order[_,_]):Boolean = {
//    try{
        OrderImpl.dat = o match {
        case order(_:material,_:itemstack,_,_) => rawType[material,itemstack](OrderImpl.dat,o.asInstanceOf[order[material,itemstack]])
        case order(_:itemstack,_:material,_,_) => rawType[itemstack,material](OrderImpl.dat,o.asInstanceOf[order[itemstack,material]])
        case _ => OrderImpl.dat
      }
      true
    //}
//    catch{
//      case e:Exception => {
//        e.printStackTrace()
//        false
//      }
//    }

  }


  implicit class MapSerializer(m:Map[_,_]) {
    def serialize(a:Any):String = {
      a match {
        case m:Map[_,_] => { jsonMap(m)}
        case l:List[_] => "[" + l.foldLeft("")(_ + "," +  serialize(_)) + "]"
        case _ => a.toString()
      }
    }
    def jsonMap(mm:Map[_,_]):String = {
      mm.keys.foldLeft("{")((acc,curr) => {
        acc + ",\n" + curr + ":" + serialize(curr)
      }) + "}"
    }
    def jsonMap():String = jsonMap(m)
  }
 implicit class Deserializer(s:String) {
   def toMap[A,B]():Option[Map[A,B]] = ???
 }

  def getOrderbook():String = {
    OrderImpl.dat.fetch[orderbooktype[itemstack,material],orderbook[itemstack,material]].typedInitVal.toString()
  }
  def getEscrow(plyr:player):String = {
    val escrowmap = OrderImpl.dat.fetch[escrowtype[itemstack,material],escrowadd[itemstack,material]].typedInitVal
    escrowmap.get(plyr.id).getOrElse(Seq()).toString()
  }
//  def getAllEscrow(plyr:player):escrowtype = {
//    val escrowmap = OrderImpl.dat.fetch[escrowtype[itemstack,material],escrowadd[itemstack,material]].typedInitVal
//
//  }
  def setEscrow[A,B](p:player,plyrescrow:order[A,B]*) = {
    val oldescrow = OrderImpl.dat.fetch[escrowtype[A,B],escrowadd[A,B]].typedInitVal
    OrderImpl.dat = OrderImpl.dat.include[escrowtype[A,B],escrowadd[A,B]](oldescrow.updated(p.id,plyrescrow))
  }

  def main(args: Array[String]): Unit = {
    updateDataModel(order[itemstack,material](it,mat,10,"me"))
    println(OrderImpl.dat.fetch[orderbooktype[itemstack,material],orderbook[itemstack,material]].typedInitVal)
    println(OrderImpl.dat.fetch[orderbooktype[material,itemstack],orderbook[material,itemstack]].typedInitVal)
    println("////////////////////////////////////////////////////////////////////////////////")
//    updateDataModel(order[material,itemstack](mat,it,8,"you"))
//    println(OrderImpl.dat.fetch[orderbooktype[itemstack,material],orderbook[itemstack,material]].typedInitVal)
//    println(OrderImpl.dat.fetch[orderbooktype[material,itemstack],orderbook[material,itemstack]].typedInitVal)
//    println("////////////////////////////////////////////////////////////////////////////////")
//    updateDataModel(order[material,itemstack](mat,it,8,"you"))
//    println(OrderImpl.dat.fetch[orderbooktype[itemstack,material],orderbook[itemstack,material]].typedInitVal)
//    println(OrderImpl.dat.fetch[orderbooktype[material,itemstack],orderbook[material,itemstack]].typedInitVal)
//    println("////////////////////////////////////////////////////////////////////////////////")
    updateDataModel(order[itemstack,material](it,mat,10,"me"))
    println(OrderImpl.dat.fetch[orderbooktype[itemstack,material],orderbook[itemstack,material]].typedInitVal)
    println(OrderImpl.dat.fetch[orderbooktype[material,itemstack],orderbook[material,itemstack]].typedInitVal)
    println("ESCROW//////////////////////////////////")
    println(OrderImpl.dat.fetch[escrowtype[itemstack,material],escrowadd[itemstack,material]].typedInitVal)
    println(OrderImpl.dat.fetch[escrowtype[material,itemstack],escrowadd[material,itemstack]].typedInitVal)

  }
}
