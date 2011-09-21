package bootstrap.liftweb

import net.liftweb._
import util._
import Helpers._

import common._
import http._
import sitemap._
import Loc._
import org.nfolkert.fssc.model.MongoSetup
import org.scalafoursquare.call.App
import org.scalafoursquare.call.App.CallLogger

/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot {
  def boot {
    MongoSetup.init


    App.logger = APICallLogger

    LiftRules.htmlProperties.default.set((r: Req) =>new Html5Properties(r.userAgent))

    // where to search snippet
    LiftRules.addToPackages("org.nfolkert")

    // Build SiteMap
    val entries = List(
      Menu.i("Home") / "index", // the simple way to declare a menu

      Menu.i("Map") / "map",

      Menu.i("TouchMap") / "touch" / "map",

      // more complex because this menu allows anything in the
      // /static path to be visible
      Menu(Loc("Static", Link(List("static"), true, "/static/index"),
	       "Static Content")))

    // set the sitemap.  Note if you don't want access control for
    // each page, just comment this line out.
    LiftRules.setSiteMap(SiteMap(entries:_*))

    //Show the spinny image when an Ajax call starts
    LiftRules.ajaxStart =
      Full(() => LiftRules.jsArtifacts.show("ajax-loader").cmd)

    // Make the spinny image go away when it ends
    LiftRules.ajaxEnd =
      Full(() => LiftRules.jsArtifacts.hide("ajax-loader").cmd)

    // Force the request to be UTF-8
    LiftRules.early.append(_.setCharacterEncoding("UTF-8"))

  }
}

object APICallLogger extends CallLogger with Loggable {
  def call(msg: => String, timeMillis: Long) {logger.info(msg + " took: " + timeMillis + " ms")}
  def extract(msg: => String, timeMillis: Long) {logger.info(msg + " took: " + timeMillis + " ms")}
  def trace(msg: => String) {}
  def debug(msg: => String) {}
  def info(msg: => String) {logger.info(msg)}
  def warn(msg: => String) {logger.warn(msg)}
  def error(msg: => String) {logger.error(msg)}
}
