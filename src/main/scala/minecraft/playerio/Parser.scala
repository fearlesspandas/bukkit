// package minecraft.playerio
//
// import scala.collection.mutable.HashMap
// import org.bukkit.inventory.ItemStack
// import org.bukkit.entity.Player
// import org.bukkit.Material
// import minecraft.economy._
// import minecraft.constants._
// object Parser {
//   def tryInt(i:String):Boolean = {
//     try{
//       i.toInt
//       return true
//     }catch{
//       case e:Exception => false
//     }
//   }
//   def trySymbol(s:String):Boolean = {
//     if (OrderBookConstants.delims.contains(s)) true else false
//   }
//   def parseSymbol(s:String):
//   def grammarMatch(prev:String,curr:String)
//   def parseOrder(player:Player,args:Array[String],curr:Array[String]):Array[Order] = {
//     if(args.size == 0) return Array[Order]
//     args(0) match{
//       case if tryInt(args(0)) => {
//
//       }
//       case if trySymbol(args(0))
//     }
//   }
// }
//
// object Dropper {
//
//
// }
