package org.nfolkert.fssc.model

import org.scalafoursquare.response.{VenueCategoryWithChildren, VenueCategoriesResponse}
import net.liftweb.common.{Full, Box, Empty}
import org.nfolkert.fssc.UserData

case class VenueCategory(id: String, name: String, icon: String, children: List[VenueCategory]) {}

case class VenueCategories (roots: List[VenueCategory], map: Map[String, VenueCategory], ancestors: Map[VenueCategory, List[VenueCategory]]) {
  def parent(id: String): Option[VenueCategory] = category(id).flatMap(cat=>parent(cat))
  def category(id: String): Option[VenueCategory] = map.get(id)
  def root(id: String): Option[VenueCategory] = category(id).flatMap(cat=>root(cat))

  def parent(cat: VenueCategory): Option[VenueCategory] = ancestors.get(cat).flatMap(_.headOption).filter(_ != cat)
  def root(cat: VenueCategory): Option[VenueCategory] = ancestors.get(cat).flatMap(_.lastOption)

  def exploreSection(cat: VenueCategory) = root(cat).map(_.id match {
    // Meh.  Can't use anything from category to infer the explore section

    case "4d4b7104d754a06370d81259" => Some(RecommendationType.arts) // Arts & Entertainment
    case "4d4b7105d754a06372d81259" => None // Colleges & Universities
    case "4d4b7105d754a06374d81259" => Some(RecommendationType.food) // Food
    case "4d4b7105d754a06377d81259" => Some(RecommendationType.outdoors) // Great Outdoors
    case "4d4b7105d754a06376d81259" => Some(RecommendationType.drinks) // Nightlife
    case "4d4b7105d754a06375d81259" => None // Professional & Other Places
    case "4e67e38e036454776db1fb3a" => None // Residence
    case "4d4b7105d754a06378d81259" => Some(RecommendationType.shops) // Shops & Services
    case "4d4b7105d754a06379d81259" => Some(RecommendationType.travel) // Travel Spot
    case _ => None
  }).getOrElse({
    cat.id match {
      case "4bf58dd8d48988d1e0931735" | "4bf58dd8d48988d16d941735" | "4bf58dd8d48988d1dc931735" => Some(RecommendationType.coffee)
      case _ => None
    }
  })
}

object VenueCategories {
  def create(response: Option[VenueCategoriesResponse]): VenueCategories = {
    response.map(r=>{
      def recRebuild(cat: VenueCategoryWithChildren): VenueCategory = {
        val children = cat.categories.map(children => {
          children.map(ch => {recRebuild(ch)})
        }).getOrElse(Nil)
        VenueCategory(cat.id.getOrElse(cat.name), cat.name, cat.icon, children)
      }

      val roots = r.categories.map(cat => {recRebuild(cat)})

      def pull(cat: VenueCategory): List[VenueCategory] = cat :: cat.children.flatMap(cat=>pull(cat))
      val map = roots.flatMap(cat=>pull(cat)).map(cat=>(cat.id, cat)).toMap

      def ancs(category: VenueCategory, ancestors: List[VenueCategory]): List[(VenueCategory, List[VenueCategory])] = {
        (category -> ancestors) :: category.children.flatMap(ch=>ancs(ch, ch :: ancestors))
      }
      val ancestors = roots.flatMap(cat => ancs(cat, List(cat))).toMap

      VenueCategories(roots, map, ancestors)
    }).getOrElse(VenueCategories(Nil, Map.empty, Map.empty))
  }
}
