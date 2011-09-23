package org.nfolkert.fssc.model

import net.liftweb.mongodb.record.{MongoMetaRecord, MongoRecord}
import org.scalafoursquare.response.{UserDetail}
import org.nfolkert.lib.Util
import net.liftweb.record.field.{DoubleField, LongField, StringField}

trait ModelConstant[T] {
  def name: String
}

case class ViewType(name: String, desc: String) extends ModelConstant[ViewType]
case object ViewType {
  val touch = ViewType("touch", "Touch Style")
  val web = ViewType("web", "Web Style")
  val calculate = ViewType("calculate", "Find From Device")
  val values = List(touch, web, calculate)
  val defaultValue = calculate
  def byName(str: String) = values.find(_.name == str).getOrElse(defaultValue)
}

case class PlayLevel(name: String, desc: String, gridSize: Int) extends ModelConstant[PlayLevel]
case object PlayLevel {
  val easy = PlayLevel("easy", "Beginner - Neighborhood", 1500)
  val medium = PlayLevel("medium", "Intermediate", 500)
  val advanced = PlayLevel("advanced", "Advanced", 250)
  val expert = PlayLevel("expert", "Expert - Block", 100)
  val values = List(easy, medium, advanced, expert)
  val defaultValue = medium
  def byName(str: String) = values.find(_.name == str).getOrElse(defaultValue)
}

case class RecommendationType(name: String, desc: String) extends ModelConstant[RecommendationType]
object RecommendationType {
  val none = RecommendationType("none", "No Recommendations")
  val food = RecommendationType("food", "Food")
  val drinks = RecommendationType("drinks", "Drinks")
  val coffee = RecommendationType("coffee", "Coffee")
  val shops = RecommendationType("shops", "Shopping")
  val arts = RecommendationType("arts", "Arts & Entertainment")
  val outdoors = RecommendationType("outdoors", "Outdoors")
  val all = RecommendationType("all", "All Categories")
  val values = List(none, food, drinks, coffee, shops, arts, outdoors, all)
  val defaultValue = none
  def byName(str: String) = values.find(_.name == str).getOrElse(defaultValue)
}

class User extends MongoRecord[User] {
  def meta = User

  object id extends StringField(this, 100) {
    override def name = "_id"
  }

  object joinTime extends LongField(this) {
    override def name = "jt"
  }
  def getJoinTime = Util.dateFromSeconds(joinTime.value)

  object viewType extends StringField(this, 50) {
    override def name = "vt"
    override def defaultValue = ViewType.defaultValue.name
  }
  def setViewType(vt: ViewType): User = viewType(vt.name)
  def getViewType = ViewType.values.find(_.name == viewType.value).getOrElse(ViewType.defaultValue)

  object playLevel extends StringField(this, 50) {
    override def name = "pl"
    override def defaultValue = PlayLevel.defaultValue.name
  }
  def setPlayLevel(pl: PlayLevel): User = playLevel(pl.name)
  def getPlayLevel = PlayLevel.values.find(_.name == playLevel.value).getOrElse(PlayLevel.defaultValue)

  object recommendations extends StringField(this, 50) {
    override def name = "rc"
    override def defaultValue = RecommendationType.defaultValue.name
  }
  def setRecommendations(rc: RecommendationType): User = recommendations(rc.name)
  def getRecommendations = RecommendationType.values.find(_.name == recommendations.value).getOrElse(RecommendationType.defaultValue)

  object opacity extends DoubleField(this) {
    override def name = "op"
    override def defaultValue = 1.0
  }
}

object User extends User with MongoMetaRecord[User] {
  override def collectionName = "users"

  def findOrCreate(user: UserDetail) = {
    User.find(user.id).openOr(
      User.createRecord
        .id(user.id)
        .joinTime(Util.nowInSeconds)
      .save
    )
  }
}
