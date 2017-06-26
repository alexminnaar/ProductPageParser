package com.viglink.productparser

import java.io.File

import org.jsoup.Jsoup
import org.jsoup.nodes.{Attribute, Document, Element}

import scala.collection.JavaConversions._


case class ParseResult(title: Option[String],
                       price: Option[String],
                       imageUrl: Option[String],
                       availability: Option[String],
                       sku: Option[String])

sealed trait TagValue

case class Content() extends TagValue
case class Text() extends TagValue
case class Value() extends TagValue
case class Source() extends TagValue

object ParseUtils {

  def tagParser(doc: Document,
                tag: String,
                attribute: String,
                attrValue: String,
                contentOrText: TagValue): Option[String] = {

    val selectedTags = doc.select(s"$tag[$attribute=$attrValue]")

    if (selectedTags.size() > 0) {

      contentOrText match {
        case Content() => Some(selectedTags.head.attr("content"))
        case Value() => Some(selectedTags.head.attr("value"))
        case Source() => Some(selectedTags.head.attr("src"))
        case Text() => Some(selectedTags.head.text())
      }
    }
    else {
      None
    }
  }


  def searchTag(tag: Element): Option[String] = {
    val content = tag.attr("content")
    val text = tag.text()
    val src = tag.attr("src")
    val value = tag.attr("value")

    if (!content.isEmpty) {
      Some(content)
    }
    else if (!text.isEmpty) {
      Some(text)
    }
    else if (!src.isEmpty) {
      Some(src)
    }
    else if (!value.isEmpty) {
      Some(value)
    }
    else {
      None
    }

  }


  def isLogoImage(imageUrl: String): Boolean = {

    val urlEnd = imageUrl.split("/").last
    urlEnd.toLowerCase().contains("logo")

  }

  def isImage(candidateValue: String): Boolean = {
    candidateValue.contains(".jpg") ||
      candidateValue.contains(".jpeg") ||
      candidateValue.contains(".png") ||
      candidateValue.contains(".gif") ||
      candidateValue.takeRight(4) == "jpeg"
  }


  def generalPriceRules(attribute: Attribute,
                        tag: Element,
                        candidateValue: String
                       ): Boolean = {

    val numPattern = "[0-9]+".r

    /*
    Attribute value should contain "price" but not "cart" or "ourPrice2".
    The tag should not be "a" or "img".
    The candidate value should contain a number and be less than 20 characters.
     */
    attribute.getValue.toLowerCase().contains("price") &&
      attribute.getValue != "ourPrice2" && //toys'r'us specific rule
      !attribute.getValue.contains("cart") &&
      tag.tagName() != "a" &&
      tag.tagName() != "img" &&
      numPattern.findFirstIn(candidateValue).isDefined &&
      candidateValue.size < 20

  }

  //Special high confidence price rule that should supercede every other price rule
  def specialPriceRules(attribute: Attribute,
                        candidateValue: String): Boolean = {

    val numPattern = "[0-9]+".r

    //checking if key is "itemprop" and value contains "price" and candidate contains a number
    if (attribute.getKey() == "itemprop" &&
      numPattern.findFirstIn(candidateValue).isDefined &&
      attribute.getValue.toLowerCase().contains("price")) {
      true
    }
    //amazon specific rule - high confidence
    else if (attribute.getValue == "a-size-medium a-color-price" &&
      numPattern.findFirstIn(candidateValue).isDefined) {
      true
    }
    else {
      false
    }

  }

  def generalTitleRules(attribute: Attribute, tag: Element): Boolean = {

    /*
    The attribute should contain "title".
    The attribute should not contain "subtitle", "review", or "modal".
    The tag should not be "div", "p", "h3", or "h4".
     */
    attribute.getValue.contains("title") &&
      !attribute.getValue.contains("subtitle") &&
      !attribute.getValue.contains("review") &&
      !attribute.getValue.contains("modal") &&
      tag.tagName() != "div" &&
      tag.tagName() != "p" &&
      tag.tagName() != "h3" &&
      tag.tagName() != "h4"
  }

  //Special high confidence title rule that should supercede every other title rule
  def specialTitleRules(attribute: Attribute): Boolean = {
    attribute.getValue == "og:title"
  }

  def generalImageRules(tag: Element,
                        attribute: Attribute,
                        candidateValue: String): Boolean = {

    /*
     Attribute value should contain "image".
     Attribute value should not contain ".png" or ".gif".
     Tag should not be "div".
     Candidate value should be a valid image.
     Candidate value should not be a logo image.
     */
    attribute.getValue.contains("image") &&
      !attribute.getValue.contains(".png") &&
      !attribute.getValue.contains(".gif") &&
      tag.tagName() != "div" &&
      isImage(candidateValue) &&
      !isLogoImage(candidateValue)

  }

  //Special high confidence image rule that should supercede every other image rule
  def specialImageRules(attribute: Attribute, candidateValue: String): Boolean = {
    attribute.getKey() == "property" && attribute.getValue == "og:image" && !isLogoImage(candidateValue)
  }

  def generalSkuRules(attribute: Attribute,
                      tag: Element,
                      candidateValue: String): Boolean = {

    val numPattern = "[0-9]+".r

    /*
    Attribute value should contain "sku".
    Attribute value should not contain "price".
    Tag should not be "a".
    Candidate value should be between 4 and 15 characters.
     */
    attribute.getValue.contains("sku") &&
      !attribute.getValue.contains("price") &&
      tag.tagName() != "a" &&
      candidateValue.size < 15 &&
      candidateValue.size > 4 &&
      numPattern.findFirstIn(candidateValue).isDefined

  }


  def generalAvailabilityRules(attribute: Attribute,
                               tag: Element,
                               candidateValue: String): Boolean = {

    /*
    Attribute should contain "stock", "availability", "available", or "onhand".
    Attribute should not contain "stocknum", "availableformat", "price",or "overstock".
    Tag should not be "a", "button", or "li".
    The candidate value should not contain "{".
    The candidate value should not contain "overstock".
    The candidate value should not be longer than 45 characters.
    */
    (attribute.getValue.contains("stock") ||
      attribute.getValue.contains("availability") ||
      attribute.getValue.contains("available") ||
      attribute.getValue.contains("onhand")) &&
      attribute.getValue != "stocknum" &&
      !attribute.getValue.contains("availableformat") &&
      !attribute.getValue.contains("price") &&
      !attribute.getValue.toLowerCase().contains("overstock") &&
      tag.tagName() != "a" &&
      tag.tagName() != "button" &&
      tag.tagName() != "li" &&
      !candidateValue.toLowerCase().contains("{") &&
      !candidateValue.toLowerCase().contains("overstock") &&
      candidateValue.size < 45

  }


  def ruleBasedParser(doc: Document): ParseResult = {

    var parsedTitle: Option[String] = None
    var parsedPrice: Option[String] = None
    var parsedImageUrl: Option[String] = None
    var parsedAvailability: Option[String] = None
    var parsedSku: Option[String] = None

    //get all tags
    val tags = doc.select("*")

    tags.foreach { tag =>

      tag.attributes().foreach { attribute =>

        val candidateValue = searchTag(tag)

        if (candidateValue.isDefined) {

          //Price Extraction
          //only try general price rule if price has not yet been found
          if (!parsedPrice.isDefined) {

            if (generalPriceRules(attribute, tag, candidateValue.get)) {
              parsedPrice = candidateValue
            }
          }

          //Even if price has already been found, try special high confidence rules which supercede general rules
          if (specialPriceRules(attribute, candidateValue.get)) {
            parsedPrice = candidateValue
          }


          //Title Extraction
          //only try general title rule if price has not yet been found
          if (!parsedTitle.isDefined) {

            if (generalTitleRules(attribute, tag)) {
              parsedTitle = candidateValue
            }
          }

          //Even if title has already been found, try special high confidence rules which supercede general rules
          if (specialTitleRules(attribute)) {
            parsedTitle = candidateValue
          }

          //Image Extraction
          //only try general image rule if image has not yet been found
          if (!parsedImageUrl.isDefined) {

            if (generalImageRules(tag, attribute, candidateValue.get)) {
              parsedImageUrl = candidateValue
            }
          }

          //Even if image has already been found, try special high confidence rules which supercede general rules
          if (specialImageRules(attribute, candidateValue.get)) {
            parsedImageUrl = candidateValue
          }

          //Sku Extraction
          //only try general image rule if image has not yet been found
          if (!parsedSku.isDefined) {

            if (generalSkuRules(attribute, tag, candidateValue.get)) {
              parsedSku = candidateValue
            }
          }

          //Availability Extraction
          //only try general availability rule if availability has not yet been found
          if (!parsedAvailability.isDefined) {

            if (generalAvailabilityRules(attribute, tag, candidateValue.get)) {
              parsedAvailability = candidateValue
            }
          }

        }
      }
    }

    //if product title was not found, default to page title
    if (!parsedTitle.isDefined) {
      parsedTitle = Some(doc.title())
    }

    ParseResult(parsedTitle, parsedPrice, parsedImageUrl, parsedAvailability, parsedSku)
  }

}


object test extends App {

  val srDoc = Jsoup.parse(new File("/users/alexminnaar/diffbot_examples/summitracing.html"), "UTF-8")
  val rDoc = Jsoup.parse(new File("/users/alexminnaar/diffbot_examples/rodale.html"), "UTF-8")
  val lpDoc = Jsoup.parse(new File("/users/alexminnaar/diffbot_examples/lampsplus.html"), "UTF-8")
  val wmDoc = Jsoup.parse(new File("/users/alexminnaar/diffbot_examples/wallmart.html"), "UTF-8")
  val etDoc = Jsoup.parse(new File("/users/alexminnaar/diffbot_examples/etsy.html"), "UTF-8")
  val wfDoc = Jsoup.parse(new File("/users/alexminnaar/diffbot_examples/wayfair.html"), "UTF-8")
  val truDoc = Jsoup.parse(new File("/users/alexminnaar/diffbot_examples/toysrus.html"), "UTF-8")
  val aeDoc = Jsoup.parse(new File("/users/alexminnaar/diffbot_examples/aliexpress.html"), "UTF-8")
  val nDoc = Jsoup.parse(new File("/users/alexminnaar/diffbot_examples/nordstrom.html"), "UTF-8")
  val lDoc = Jsoup.parse(new File("/users/alexminnaar/diffbot_examples/lowes.html"), "UTF-8")
  val pfDoc = Jsoup.parse(new File("/users/alexminnaar/diffbot_examples/pricefalls.html"), "UTF-8")
  val mwDoc = Jsoup.parse(new File("/users/alexminnaar/diffbot_examples/midwayusa.html"), "UTF-8")
  val wsDoc = Jsoup.parse(new File("/users/alexminnaar/diffbot_examples/williams-sonoma.html"), "UTF-8")
  val neDoc = Jsoup.parse(new File("/users/alexminnaar/diffbot_examples/newegg.html"), "UTF-8")
  val hdDoc = Jsoup.parse(new File("/users/alexminnaar/diffbot_examples/homedepot.html"), "UTF-8")
  val brDoc = Jsoup.parse(new File("/users/alexminnaar/diffbot_examples/brownells.html"), "UTF-8")
  val bbbDoc = Jsoup.parse(new File("/users/alexminnaar/diffbot_examples/buybuybaby.html"), "UTF-8")
  val bpvDoc = Jsoup.parse(new File("/users/alexminnaar/diffbot_examples/bhphotovideo.html"), "UTF-8")
  val nmDoc = Jsoup.parse(new File("/users/alexminnaar/diffbot_examples/neimanmarcus.html"), "UTF-8")
  val bgDoc = Jsoup.parse(new File("/users/alexminnaar/diffbot_examples/banggood.html"), "UTF-8")
  val osDoc = Jsoup.parse(new File("/users/alexminnaar/diffbot_examples/overstock.html"), "UTF-8")
  val rhDoc = Jsoup.parse(new File("/users/alexminnaar/diffbot_examples/restorationhardware.html"), "UTF-8")
  val bbDoc = Jsoup.parse(new File("/users/alexminnaar/diffbot_examples/bestbuy.html"), "UTF-8")
  val mmDoc = Jsoup.parse(new File("/users/alexminnaar/diffbot_examples/mission-modelisme.html"), "UTF-8")
  val ttDoc = Jsoup.parse(new File("/users/alexminnaar/diffbot_examples/tiretrack.html"), "UTF-8")
  val cfDoc = Jsoup.parse(new File("/users/alexminnaar/diffbot_examples/crutchfield.html"), "UTF-8")
  val hmDoc = Jsoup.parse(new File("/users/alexminnaar/diffbot_examples/hm.html"), "UTF-8")
  val amDoc = Jsoup.parse(new File("/users/alexminnaar/diffbot_examples/amazon.html"), "UTF-8")
  val ssixDoc = Jsoup.parse(new File("/users/alexminnaar/diffbot_examples/society6.html"), "UTF-8")
  val caridDoc = Jsoup.parse(new File("/users/alexminnaar/diffbot_examples/carid.html"), "UTF-8")
  val macysDoc = Jsoup.parse(new File("/users/alexminnaar/diffbot_examples/macys.html"), "UTF-8")
  val shopbopDoc = Jsoup.parse(new File("/users/alexminnaar/diffbot_examples/shopbop.html"), "UTF-8")
  val sixpmDoc = Jsoup.parse(new File("/users/alexminnaar/diffbot_examples/6pm.html"), "UTF-8")
  val bedbathDoc = Jsoup.parse(new File("/users/alexminnaar/diffbot_examples/bedbathandbeyond.html"), "UTF-8")
  val cabDoc = Jsoup.parse(new File("/users/alexminnaar/diffbot_examples/cabelas.html"), "UTF-8")
  val crateDoc = Jsoup.parse(new File("/users/alexminnaar/diffbot_examples/crateandbarrel.html"), "UTF-8")
  val kohlsDoc = Jsoup.parse(new File("/users/alexminnaar/diffbot_examples/kohls.html"), "UTF-8")
  val anthroDoc = Jsoup.parse(new File("/users/alexminnaar/diffbot_examples/anthropologie.html"), "UTF-8")
  val oklDoc = Jsoup.parse(new File("/users/alexminnaar/diffbot_examples/onekingslane.html"), "UTF-8")
  val dilDoc = Jsoup.parse(new File("/users/alexminnaar/diffbot_examples/dillards.html"), "UTF-8")
  val cartersDoc = Jsoup.parse(new File("/users/alexminnaar/diffbot_examples/carters.html"), "UTF-8")
  val towerhobbiesDoc = Jsoup.parse(new File("/users/alexminnaar/diffbot_examples/towerhobbies.html"), "UTF-8")
  val gearbestDoc = Jsoup.parse(new File("/users/alexminnaar/diffbot_examples/gearbest.html"), "UTF-8")
  val dominoDoc = Jsoup.parse(new File("/users/alexminnaar/diffbot_examples/domino.html"), "UTF-8")
  val harborfreightDoc = Jsoup.parse(new File("/users/alexminnaar/diffbot_examples/harborfreight.html"), "UTF-8")
  val barnesandnobleDoc = Jsoup.parse(new File("/users/alexminnaar/diffbot_examples/barnesandnoble.html"), "UTF-8")
  val nikeDoc = Jsoup.parse(new File("/users/alexminnaar/diffbot_examples/nike.html"), "UTF-8")
  val asosDoc = Jsoup.parse(new File("/users/alexminnaar/diffbot_examples/asos.html"), "UTF-8")
  val sephoraDoc = Jsoup.parse(new File("/users/alexminnaar/diffbot_examples/sephora.html"), "UTF-8")
  val ecstuningDoc = Jsoup.parse(new File("/users/alexminnaar/diffbot_examples/ecstuning.html"), "UTF-8")
  val foreverDoc = Jsoup.parse(new File("/users/alexminnaar/diffbot_examples/forever21.html"), "UTF-8")
  val partsExpressDoc = Jsoup.parse(new File("/users/alexminnaar/diffbot_examples/partsexpress.html"), "UTF-8")


  println("=====summitracing=====")
  println(ParseUtils.ruleBasedParser(doc = srDoc))
  println("=====rodale=====")
  println(ParseUtils.ruleBasedParser(doc = rDoc))
  println("=====lampslpus=====")
  println(ParseUtils.ruleBasedParser(doc = lpDoc))
  println("=====wallmart=====")
  println(ParseUtils.ruleBasedParser(doc = wmDoc))
  println("=====etsy=====")
  println(ParseUtils.ruleBasedParser(doc = etDoc))
  println("=====wayfair=====")
  println(ParseUtils.ruleBasedParser(doc = wfDoc))
  println("=====toysrus=====")
  println(ParseUtils.ruleBasedParser(doc = truDoc))
  println("=====aliexpress=====")
  println(ParseUtils.ruleBasedParser(doc = aeDoc))
  println("=====nordstrom=====")
  println(ParseUtils.ruleBasedParser(doc = nDoc))
  println("=====lowes=====")
  println(ParseUtils.ruleBasedParser(doc = lDoc))
  println("=====pricefalls=====")
  println(ParseUtils.ruleBasedParser(doc = pfDoc))
  println("=====midwayusa=====")
  println(ParseUtils.ruleBasedParser(doc = mwDoc))
  println("=====williams-sonoma=====")
  println(ParseUtils.ruleBasedParser(doc = wsDoc))
  println("=====newegg=====")
  println(ParseUtils.ruleBasedParser(doc = neDoc))
  println("=====homedepot=====")
  println(ParseUtils.ruleBasedParser(doc = hdDoc))
  println("=====brownells=====")
  println(ParseUtils.ruleBasedParser(doc = brDoc))
  println("=====buybuybaby=====")
  println(ParseUtils.ruleBasedParser(doc = bbbDoc))
  println("=====bhphotovideo=====")
  println(ParseUtils.ruleBasedParser(doc = bpvDoc))
  println("=====neimanmarcus=====")
  println(ParseUtils.ruleBasedParser(doc = nmDoc))
  println("=====banggood=====")
  println(ParseUtils.ruleBasedParser(doc = bgDoc))
  println("=====overstock=====")
  println(ParseUtils.ruleBasedParser(doc = osDoc))
  println("=====restorationhardware=====")
  println(ParseUtils.ruleBasedParser(doc = rhDoc))
  println("=====bestbuy=====")
  println(ParseUtils.ruleBasedParser(doc = bbDoc))
  println("=====mission-modelisme=====")
  println(ParseUtils.ruleBasedParser(doc = mmDoc))
  println("=====tiretrack=====")
  println(ParseUtils.ruleBasedParser(doc = ttDoc))
  println("=====crutchfield=====")
  println(ParseUtils.ruleBasedParser(doc = cfDoc))
  println("=====hm=====")
  println(ParseUtils.ruleBasedParser(doc = hmDoc))
  println("=====amazon=====")
  println(ParseUtils.ruleBasedParser(doc = amDoc))
  println("=====society6=====")
  println(ParseUtils.ruleBasedParser(doc = ssixDoc))
  println("=====carid=====")
  println(ParseUtils.ruleBasedParser(doc = caridDoc))
  println("=====macys=====")
  println(ParseUtils.ruleBasedParser(doc = macysDoc))
  println("=====shopbop=====")
  println(ParseUtils.ruleBasedParser(doc = shopbopDoc))
  println("=====6pm=====")
  println(ParseUtils.ruleBasedParser(doc = sixpmDoc))
  println("=====bedbathandbeyond=====")
  println(ParseUtils.ruleBasedParser(doc = bedbathDoc))
  println("=====cabelas=====")
  println(ParseUtils.ruleBasedParser(doc = cabDoc))
  println("=====crateandbarrel=====")
  println(ParseUtils.ruleBasedParser(doc = crateDoc))
  println("=====kohls=====")
  println(ParseUtils.ruleBasedParser(doc = kohlsDoc))
  println("=====anthropologie=====")
  println(ParseUtils.ruleBasedParser(doc = anthroDoc))
  println("=====onekingslane=====")
  println(ParseUtils.ruleBasedParser(doc = oklDoc))
  println("=====dillards=====")
  println(ParseUtils.ruleBasedParser(doc = dilDoc))
  println("=====carters=====")
  println(ParseUtils.ruleBasedParser(doc = cartersDoc))
  println("=====towerhobbies=====")
  println(ParseUtils.ruleBasedParser(doc = towerhobbiesDoc))
  println("=====gearbest=====")
  println(ParseUtils.ruleBasedParser(doc = gearbestDoc))
  println("=====domino=====")
  println(ParseUtils.ruleBasedParser(doc = dominoDoc))
  println("=====harborfreight=====")
  println(ParseUtils.ruleBasedParser(doc = harborfreightDoc))
  println("=====barnesandnoble=====")
  println(ParseUtils.ruleBasedParser(doc = barnesandnobleDoc))
  println("=====nike=====")
  println(ParseUtils.ruleBasedParser(doc = nikeDoc))
  println("=====asos=====")
  println(ParseUtils.ruleBasedParser(doc = asosDoc))
  println("=====sephora=====")
  println(ParseUtils.ruleBasedParser(doc = sephoraDoc))
  println("=====ecstuning=====")
  println(ParseUtils.ruleBasedParser(doc = ecstuningDoc))
  println("=====forever21=====")
  println(ParseUtils.ruleBasedParser(doc = foreverDoc))
  println("=====partsexpress=====")
  println(ParseUtils.ruleBasedParser(doc = partsExpressDoc))

}

