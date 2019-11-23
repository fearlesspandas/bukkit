package minecraft.plugin.economy;

import java.util.HashMap;
import java.util.Map;
import java.util.ArrayList;
import java.util.List;

import org.bukkit.command.Command;
import org.bukkit.command.CommandSender;
import org.bukkit.plugin.java.JavaPlugin;
import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemStack;
import org.bukkit.inventory.PlayerInventory;


public class Order {
  public ItemStack price;
  public ItemStack item;
  public String owner;
  public int remaining;
  public Order(ItemStack price, ItemStack item, String owner,int remaining){
    this.price = price;
    this.item = item;
    this.remaining = remaining;
    this.owner      = owner     ;
  }
  public String toCSV(){
    return price.getType().getKey().getKey() + "," + item.getType().getKey().getKey() + "," + price.getAmount() + "," + item.getAmount() + "," + owner + "\n";
  }

}
