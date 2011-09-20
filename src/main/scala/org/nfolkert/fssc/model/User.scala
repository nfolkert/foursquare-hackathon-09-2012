package org.nfolkert.fssc.model

import net.liftweb.mongodb.record.{MongoMetaRecord, MongoRecord}
import net.liftweb.mongodb.record.field.DateField
import org.scalafoursquare.response.{UserDetail, UserCompact}
import net.liftweb.record.field.{LongField, StringField}
import org.joda.time.DateTime
import org.nfolkert.lib.Util

class User extends MongoRecord[User] {
  def meta = User

  object id extends StringField(this, 100) {
    override def name = "_id"
  }

  object joinTime extends LongField(this) {
    override def name = "jt"
  }
  def getJoinTime = Util.dateFromSeconds(joinTime.value)
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
