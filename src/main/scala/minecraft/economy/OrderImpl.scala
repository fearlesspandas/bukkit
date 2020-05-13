package minecraft.economy
import Orders.Order._
import Typical.core.Typeable.{InitialType, dataset}
import Typical.impl.{data, myprovider, recSim}
import Typical.implicits.implicits._
import org.bukkit.Material
import org.bukkit.entity.Player
import org.bukkit.inventory.ItemStack

object OrderImpl {

  class unitSell extends order(it,mat,10,"me")
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
    orderbook with matching with escrow //with flatbook with flatescrow
  ](
    myprovider
      .register[orderbook]
      .register[escrow]
//      .register[flatbook]
//      .register[flatescrow]
  ).dataset


  def updateDataModel(o:order):Boolean = ??? //{


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
    OrderImpl.dat.fetch[orderbooktype,orderbook].typedInitVal.toString()
  }

  def main(args: Array[String]): Unit = {
    val neworder = order(it,mat,20,"me")
    val neworder2 = order(it,mat,20,"me2")
    val otherorder = order(mat,it,30,"you")
    val orderseq = Seq(neworder,neworder2,otherorder)
    val orderadder = OrderImpl.dat.toFlat[orderbook,order,Map[Any,Seq[order]]].typedInitVal
    val escrowadder = OrderImpl.dat.toFlat[escrow,order,Map[Any,Seq[Fill]]].typedInitVal
    //val orderadder = OrderImpl.dat.flatfetch[flatbook].typedInitVal
    //val escrowadder = OrderImpl.dat.flatfetch[flatescrow].typedInitVal
    orderseq.foldLeft(dat)((src,ord) => escrowadder(orderadder(src,ord), ord))
    println("New orderbook: " + orderadder(orderadder(orderadder(OrderImpl.dat,neworder),neworder2),otherorder).fetch[orderbooktype,orderbook].typedInitVal(null))


  }
}
