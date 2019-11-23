package minecraft.bsx

import org.bukkit.permissions.PermissionDefault

import xyz.janboerman.scalaloader.plugin.ScalaPluginDescription.{Command => SPCommand, Permission => SPPermission}
import xyz.janboerman.scalaloader.plugin.{ScalaPlugin, ScalaPluginDescription}
import xyz.janboerman.scalaloader.plugin.description.{Scala, ScalaVersion}

@Scala(version = ScalaVersion.v2_13_0) //TODO change according to the scala version.
object bsxPlugin
    extends ScalaPlugin(new ScalaPluginDescription("bsx", "1")
        .description("kapital")
        .addCommand(new SPCommand("home") permission "demo.home" usage "/home set|tp")
        .permissions(new SPPermission("demo.home") permissionDefault PermissionDefault.TRUE)) {

    override def onEnable(): Unit = {
        getServer.getPluginManager.registerEvents(PlayerJoinListener, this)
        getCommand("home").setExecutor(HomeExecutor)
    }

}
