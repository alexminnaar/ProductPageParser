package com.viglink.productparser

import org.jsoup.nodes.Document

trait Parser {


  def getTitle(doc:Document): Option[String]

  def getImageUrl(doc:Document): Option[String]

  def getPrice(doc:Document): Option[String]

  def getAvailability(doc:Document): Option[String]

  def getSku(doc:Document): Option[String]


}
