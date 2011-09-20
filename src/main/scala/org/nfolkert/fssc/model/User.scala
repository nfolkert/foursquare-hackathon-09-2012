package org.nfolkert.fssc.model

import net.liftweb.mongodb.record.{MongoMetaRecord, MongoRecord}
import net.liftweb.mongodb.record.field.DateField
import net.liftweb.record.field.StringField
import org.scalafoursquare.response.{UserDetail, UserCompact}

class User extends MongoRecord[User] {
  def meta = User

  object id extends StringField(this, 100) {
    override def name = "_id"
  }
}

object User extends User with MongoMetaRecord[User] {
  override def collectionName = "users"

  def findOrCreate(user: UserDetail) = {
    User.find(user.id).openOr(
      User.createRecord.id(user.id).save
    )
  }
}
