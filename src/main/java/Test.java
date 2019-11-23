package minecraft.plugin;


import minecraft.plugin.economy.*;

import org.bukkit.command.Command;
import org.bukkit.command.CommandSender;
import org.bukkit.plugin.java.JavaPlugin;
import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemStack;
import org.bukkit.inventory.PlayerInventory;
//import org.bukkit.block.BlockData;

import org.bukkit.Material;


import java.util.HashMap;
import java.util.Map;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.Arrays;
import java.io.FileWriter;
import java.io.PrintWriter;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.BufferedWriter;
import java.io.BufferedReader;
import org.json.simple.JSONArray;
import org.json.simple.parser.JSONParser;
import org.json.simple.JSONObject;
//imporListt com.fasterxml.jackson.core.JsonProcessingException;
//import com.fasterxml.jackson.databind.ObjectMapper;

public final class Test extends JavaPlugin {

	HashMap playermap = new HashMap<String,Double>();
	//HashMap<ItemStack,ItemStack> ordermap = new HamountPriceashMap<ItemStack,ItemStack>();
	//ObjectMapper objectMapper = new ObjectMapper();
	JSONParser jp = new JSONParser();
	@Override
	public void onEnable() {
		getLogger().info("onEnable has been invoked!");
	}

	@Override
	public void onDisable() {
		getLogger().info("onDisable has been invoked!");
	}

	private boolean placeOrder(String item1, int quantity1, String item2, int quantity2,String owner) {
		try{
			//HashMap<>
			String fileloc = "/Users/minecraft/Public/minecraft-server/plugins/testplug/orders.csv";
			FileWriter fw = new FileWriter(fileloc,true);

			ItemStack itemstack1 = new ItemStack(Material.getMaterial(item1.toUpperCase()),quantity1);
			ItemStack itemstack2 = new ItemStack(Material.getMaterial(item2.toUpperCase()),quantity2);

			//ordermap.put(itemstack1,itemstack2);
			String order = item1 + "," + item2 + "," + quantity1 + "," + quantity2 + "," + owner + "\n";
			fw.write(order);
			fw.close();
			//writer.close();
			return true;
		} catch (Exception e){
			e.printStackTrace();
			return false;
		}

	}
	private boolean placeOrder(ItemStack itemstack1,ItemStack itemstack2, String owner, boolean refresh) {
		try{
			String fileloc = "/Users/minecraft/Public/minecraft-server/plugins/testplug/orders.csv";
			FileWriter fw = new FileWriter(fileloc,!refresh);
			PrintWriter pw = new PrintWriter(fw, false);
			if(refresh) pw.flush();
			pw.close();
			String order = itemstack1.getType().getKey().getKey() + "," + itemstack2.getType().getKey().getKey() + "," + itemstack1.getAmount() + "," + itemstack2.getAmount() + "," + owner + "\n";
			fw.write(order);
			fw.close();
			//writer.close();
			return true;
		} catch (Exception e){
			e.printStackTrace();
			return false;
		}
	}
	public boolean placeOrders(Order[] orders){
		Arrays.asList(orders).forEach((i) -> placeOrder(i.price,i.item,i.owner,true));
		return true;
	}
	public Map<ItemStack,ItemStack> getOrderMap() {
		try {
			String fileloc = "/Users/minecraft/Public/minecraft-server/plugins/testplug/orders.csv";
			FileReader fr = new FileReader(fileloc);
			BufferedReader buff = new BufferedReader(fr);
			HashMap<ItemStack,ItemStack> orders = new HashMap<ItemStack,ItemStack>();
			String orderraw = "";
			while ((orderraw = buff.readLine()) != null){
				String[] orderFields = orderraw.split(",");
				ItemStack item1 = new ItemStack(Material.getMaterial(orderFields[0].toUpperCase()),Integer.parseInt(orderFields[2]));
				ItemStack item2 = new ItemStack(Material.getMaterial(orderFields[1].toUpperCase()),Integer.parseInt(orderFields[3]));
				orders.put(item1,item2);
			}
				return orders;
		}catch(Exception e){
			e.printStackTrace();
			return null;
		}
	}

	public List<Order> getOrders(){
		try {
			String fileloc = "/Users/minecraft/Public/minecraft-server/plugins/testplug/orders.csv";
			FileReader fr = new FileReader(fileloc);
			BufferedReader buff = new BufferedReader(fr);
			List<Order> orders = new ArrayList<Order>();
			String orderraw = "";
			while ((orderraw = buff.readLine()) != null){
				String[] orderFields = orderraw.split(",");
				ItemStack item1 = new ItemStack(Material.getMaterial(orderFields[0].toUpperCase()),Integer.parseInt(orderFields[2]));
				ItemStack item2 = new ItemStack(Material.getMaterial(orderFields[1].toUpperCase()),Integer.parseInt(orderFields[3]));
				orders.add(new Order(item1,item2,orderFields[4],Integer.parseInt(orderFields[5])));
			}
				return orders;
		}catch(Exception e){
			e.printStackTrace();
			return null;
		}
	}

	public boolean fillOrder(Order order){
		List<Order> fillableOrders = getOrders().stream()
		.filter(i -> i.price == order.price && i.remaining > 0)
		.collect(Collectors.toList());
		int marketVolume = 0;
		fillableOrders.forEach( i -> marketVolume += i.remaining);
	}

	public List<ItemStack> getBuys(String item){
		ItemStack teststack = new ItemStack(Material.getMaterial(item.toUpperCase()),1);
		List<ItemStack> res = new ArrayList<ItemStack>();
		Map<ItemStack,ItemStack> ordermap = getOrderMap();
		ordermap.keySet().stream().forEach( (i) -> {
			if (teststack.getType() == ordermap.get(i).getType()){
				res.add(i);
			}
		});

		return res;
	}
	public List<ItemStack> getSells(String item){
		ItemStack teststack = new ItemStack(Material.getMaterial(item.toUpperCase()),1);
		List<ItemStack> res = new ArrayList<ItemStack>();
		Map<ItemStack,ItemStack> ordermap = getOrderMap();
		ordermap.keySet().stream().forEach( (i) -> {
			if (teststack.getType() == i.getType()){
				res.add(ordermap.get(i));
			}
		});

		return res;
	}
  @Override
	public List<String> onTabComplete(CommandSender sender, Command cmd, String label, String[] args){
		if(cmd.getName().equalsIgnoreCase("$")){

			if (args.length == 1){
				List<String> res = new ArrayList<String>();
				for (Material m: Material.values()){
					if(m.isItem() && m.toString().contains(args[0].toUpperCase())) res.add(m.toString());
				}
				return res;
			}
			if (args.length == 3){
				List<String> res = new ArrayList<String>();
				for (Material m: Material.values()){
					if(m.isItem() && m.toString().contains(args[2].toUpperCase())) res.add(m.toString());
				}
				return res;
			}
			if (args.length == 2 || args.length == 4){
				List<String> res = new ArrayList<String>();
				res.add("<amount>");
				return res;
			}

		}
		return null;
	}

  @Override
public boolean onCommand(CommandSender sender, Command cmd, String label, String[] args){
	if (cmd.getName().equalsIgnoreCase("set")) {

		Player player = (Player) sender;
		player.sendMessage("testcommand:"+ player.getPlayerListName() + args.length);
		playermap.put(player.getPlayerListName(),10.0);
		return true;
	} else if (cmd.getName().equalsIgnoreCase("get")) {

			Player player = (Player) sender;
			player.sendMessage(player.getPlayerListName() + " " + playermap.getOrDefault(player.getPlayerListName(),0.0));
			return true;
		}else if (cmd.getName().equalsIgnoreCase("$") && args.length > 0) {

				Player player = (Player) sender;
				String item1 = args[0];
				int quantity1 = Integer.parseInt(args[1]);
				String item2 = args[2];
				int quantity2 = Integer.parseInt(args[3]);
				boolean ordersuccess = placeOrder(item1,quantity1,item2,quantity2,player.getPlayerListName());

				sender.sendMessage("order placed:" + ordersuccess );
				return ordersuccess;
			}else if (cmd.getName().equalsIgnoreCase("buy") && args.length > 0) {

				Player player = (Player) sender;
				String item = args[0];

				getBuys(item).forEach((i) -> sender.sendMessage(i.toString()));
				//getOrderMap().keySet().stream().forEach((i) -> sender.sendMessage(i.toString()));
				//sender.sendMessage(objectmapper.writeValueAsString());
				return true;
			}else if (cmd.getName().equalsIgnoreCase("sell") && args.length > 0) {

				Player player = (Player) sender;
				String item = args[0];
				getSells(item).forEach((i) -> sender.sendMessage(i.toString()));
				//sender.sendMessage("Items available");
				return true;
					}

	return false;
}
}
