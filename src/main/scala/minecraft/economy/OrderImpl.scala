package minecraft.economy
import Orders.Order._
import Typical.core.Typeable.{InitialType, dataset}
import Typical.impl.{baseprovider, data}
import Typical.implicits.implicits._
import com.fasterxml.jackson.databind.annotation.JsonDeserialize
import org.bukkit.Material
import org.bukkit.enchantments.Enchantment
import org.bukkit.entity.Player
import org.bukkit.inventory.ItemStack

import scala.collection.immutable.HashMap
import scala.io.Source
import java.lang.reflect.{ParameterizedType, Type}

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.{DefaultScalaModule, ScalaObjectMapper}
import com.fasterxml.jackson.annotation.JsonProperty
import com.fasterxml.jackson.core.`type`.TypeReference;

object OrderImpl {

  val mapper = new ObjectMapper() with ScalaObjectMapper
  mapper.registerModule(DefaultScalaModule)


  case class enchantment(e:Enchantment,lvl:Int) extends GloballyOrdered[enchantment]{
    override def compareAny(that: Any): Int = that match{
      case en:Enchantment if (en.getKey == e.getKey && en.getItemTarget == e.getItemTarget) => 0
    }
  }
  case class itemstack(m:Material,a:Int) extends ItemStack(m,a) with GloballyOrdered[itemstack]{
    override def toString = m.toString + ":" + a
    def toItemstack = this.asInstanceOf[ItemStack]
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
  case class material(m:Material) extends ItemStack(m,1) with GloballyOrdered[material] {
    def toItemstack = this.asInstanceOf[ItemStack]
    override def compareAny(that: Any): Int = {
      that match{
        case tht:material if tht.m == this.m => 0
        case tht:itemstack if (tht.m == this.m && tht.a > 0) => 0
        case  _=> -1
      }
    }
    override def toString = s"$m:1"
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
    orderbook with matching with escrow
  ](
    baseprovider
      .register[orderbook]
      .register[escrow]
      .register[matching]
//      .register[flatbook]
//      .register[flatescrow]
  ).dataset

  def initializeData(defaultorderbook:Map[Any,Seq[order]]) = {
    OrderImpl.dat = OrderImpl.dat.include[orderbooktype,orderbook](_ => defaultorderbook)
  }

  def updateDataModel(o:order):Boolean = ??? //{

  @JsonDeserialize(as = classOf[raworder])
  case class raworder (
                       @JsonProperty("buyOrSell") buyOrSell:String,
                        @JsonProperty("price") price:String,
                        @JsonProperty("item") item:String,
                        @JsonProperty("vol") vol:Int,
                        @JsonProperty("remaining") remaining:Int,
                        @JsonProperty("owner") owner:String
                      ){
    def toJsonString = s"""{ "buyOrSell": "$buyOrSell","price": "$price","item": "$item","vol": $vol,"remaining": $remaining }"""
    def apply():raworder = null
    def apply(buyOrSell: String, price: String, item: String, vol: Int, remaining: Int, owner: String): raworder = new raworder(buyOrSell, price, item, vol, remaining, owner)
  }
  implicit class MapSerializer(m:Map[Any,Seq[order]]) {
    def serializeOrder(o:order) = (o.p,o.i) match {
  case (i:itemstack,m:material) => raworder("UNIT_SELL",i.m.toString,m.m.toString,i.a,o.remaining,o.owner).toJsonString
  case (m:material,i:itemstack) => raworder("UNIT_BUY",m.m.toString,i.m.toString,i.a,o.remaining,o.owner).toJsonString
}
    def jsonMap(mm:Map[Any,Seq[order]]):String = {
      val res = mm.values.flatMap(x => x).foldLeft("")((acc,curr) => {
        acc + "\n" + serializeOrder(curr)
      }) //+  "}"
      //val appended = if(res.size > 0) "}" else ""
      res //+// appended
    }
    def jsonMap():String = jsonMap(m)
  }
 implicit class Deserializer(orderdata:Seq[String]) {

   def toOrderMap():Option[Map[Any,Seq[order]]] = {

     try{
       Some(
         orderdata
           .map(mapper.readValue(_,classOf[raworder]))
           .map(raw => raw.buyOrSell.toUpperCase() match {
             case "UNIT_BUY" =>
               {
                 val pricemat = Material.getMaterial(raw.price)
                 val itemmat =  Material.getMaterial(raw.item)
                 val rawitemstack = itemstack(itemmat,raw.vol)
                 val rawmaterial = material(pricemat)
                 order(rawmaterial,rawitemstack,raw.remaining,raw.owner)
               }
             case "UNIT_SELL" =>
             {
               val pricemat = Material.getMaterial(raw.price)
               val itemmat =  Material.getMaterial(raw.item)
               val rawitemstack = itemstack(pricemat,raw.vol)
               val rawmaterial = material(itemmat)
               order(rawitemstack,rawmaterial,raw.remaining,raw.owner)
             }
           })
           .groupBy(_.owner)
       )
     }catch{
       case e:Exception =>
         e.printStackTrace()
         None
     }

   }
 }

  def getOrderbook():String = {
    case class plyrorder(i:GloballyOrdered[_],p:GloballyOrdered[_],remaining:Int)
    val res = OrderImpl.dat.fetch[orderbooktype,orderbook].value(null)
      .values
      .flatMap(x => x)
      .filter(_.remaining>0)
      .toSeq
      .sortWith((o1,o2) => o1.p.compareAny( o2.p) >=0 && o1.i.compareAny(o2.i) >= 0 )
      .foldLeft("")((acc,curr) => acc + s"\nItem:${curr.i},price:${curr.p},Remaining:${curr.remaining}")
    pretty(res)
  }
  def pretty(s:String) = s.replace("LEGACY_","")
  def getcurrentbook():Map[Any,Seq[order]] = {
    OrderImpl.dat.fetch[orderbooktype,orderbook].value(null)
  }
  def addOrder(o:order): String ={
    try{
      OrderImpl.dat = OrderImpl.dat
        .flatCalc[matching,order,(order,Seq[order])](o)
        .flatCalc[orderbook,order,Map[Any,Seq[order]]](o)
        .flatCalc[escrow,escrowinput, Map[Any,Seq[Fill]]](escrowinput(ADD(),o))
      "Order Placed"
    }catch{
      case e:Exception => {
        e.printStackTrace()
        "Something failed, order not placed"
      }
    }
  }

  def addEscrow(e:Map[Any,Seq[Fill]]) = {
    OrderImpl.dat = OrderImpl.dat.flatCalc[escrow,escrowinput, Map[Any,Seq[Fill]]](escrowinput(INJECT[Map[Any,Seq[Fill]]](e),null))
  }

  def getmatchbook(o:order) = {
    val res = OrderImpl.dat.fetch[matchtype,matching].value(o).toString()
    pretty(res)
  }
  def getEscrow() = OrderImpl.dat.fetch[escrowtype,escrow].value(null)

  def main(args: Array[String]): Unit = {
    val neworder = order(it,mat,20,"me")
    val neworder2 = order(it,mat,20,"me2")
    val otherorder = order(mat,it2,30,"you")
    val orderseq = Seq(neworder,neworder2,otherorder)
    val updatedbook = orderseq.foldLeft(dat)((src,ord) =>
      src
      .flatCalc[matching,order,(order,Seq[order])](ord)
      .flatCalc[orderbook,order,Map[Any,Seq[order]]](ord)
      //.calc[escrowtype,escrow]
      .flatCalc[escrow,escrowinput, Map[Any,Seq[Fill]]](escrowinput(ADD(),ord))
    )
    println("New orderbook: " + updatedbook.fetch[orderbooktype,orderbook].value(null))
    println("Escrow:" + updatedbook.fetch[escrowtype,escrow].value(null))


  }
}
