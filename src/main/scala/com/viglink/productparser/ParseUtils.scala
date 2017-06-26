package com.viglink.productparser

import java.io.File

import com.sun.org.apache.xml.internal.dtm.ref.sax2dtm.SAX2DTM2.AttributeIterator
import org.jsoup.Jsoup
import org.jsoup.nodes.{Attribute, Document, Element}
import org.jsoup.parser.Tag

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

    if (urlEnd.toLowerCase().contains("logo")) {
      true
    }
    else {
      false
    }

  }

//
//  def priceRules(attribute: Attribute,
//                 tag: Element,
//                 candidateValue: String,
//                 parsedPrice: Option[String],
//                 itemPropPrice: Boolean): Boolean = {
//
//    val numPattern = "[0-9]+".r
//
//    if (attribute.getValue.toLowerCase().contains("price") &&
//      attribute.getValue != "ourPrice2" && //toys'r'us specific rule
//      !attribute.getValue.contains("cart") &&
//      tag.tagName() != "a" &&
//      tag.tagName() != "img"
//    ) {
//
//      //only use the first match and make sure it contains numbers
//      if (!parsedPrice.isDefined &&
//        numPattern.findFirstIn(candidateValue).isDefined &&
//        candidateValue.size < 20) {
//        parsedPrice = candidateValue
//      }
//
//      //itemprop="price" is a very high confidence rule so it should supercede everything
//      if (attribute.getKey() == "itemprop" &&
//        numPattern.findFirstIn(candidateValue.get).isDefined &&
//        !itemPropPrice) {
//        parsedPrice = candidateValue
//        itemPropPrice = true
//      }
//
//      //amazon specific rule
//      if (attribute.getValue == "a-size-medium a-color-price" &&
//        !amazonPrice) {
//        parsedPrice = candidateValue
//        amazonPrice = true
//      }
//
//
//    }
//  }


  def ruleBasedParser(doc: Document): ParseResult = {

    var parsedTitle: Option[String] = None
    var parsedPrice: Option[String] = None
    var parsedImageUrl: Option[String] = None
    var parsedAvailability: Option[String] = None
    var parsedSku: Option[String] = None

    //get all tags
    val tags = doc.select("*")

    var itemPropPrice = false
    var amazonPrice = false
    val numPattern = "[0-9]+".r

    tags.foreach { tag =>

      tag.attributes().foreach { attribute =>

        //The value of interest would be in the content attribute or the tag text
        val candidateValue = searchTag(tag)

        if (candidateValue.isDefined) {
          //If the attribute value contains the word 'price' then the
          //actual price is probably associated with this tag
          if (attribute.getValue.toLowerCase().contains("price") &&
            attribute.getValue != "ourPrice2" && //toys'r'us specific rule
            !attribute.getValue.contains("cart") &&
            tag.tagName() != "a" &&
            tag.tagName() != "img"
          ) {

            //only use the first match and make sure it contains numbers
            if (!parsedPrice.isDefined &&
              numPattern.findFirstIn(candidateValue.get).isDefined &&
              candidateValue.get.size < 20) {
              parsedPrice = candidateValue
            }

            //itemprop="price" is a very high confidence rule so it should supercede everything
            if (attribute.getKey() == "itemprop" &&
              numPattern.findFirstIn(candidateValue.get).isDefined &&
              !itemPropPrice) {
              parsedPrice = candidateValue
              itemPropPrice = true
            }

            //amazon specific rule
            if (attribute.getValue == "a-size-medium a-color-price" &&
              !amazonPrice) {
              parsedPrice = candidateValue
              amazonPrice = true
            }

          }

          if (attribute.getValue.contains("title") &&
            !attribute.getValue.contains("subtitle") &&
            !attribute.getValue.contains("review") && //exclude review titles, only interested in product titles
            !attribute.getValue.contains("modal") &&
            tag.tagName() != "div" &&
            tag.tagName() != "p" &&
            tag.tagName() != "h3" &
            tag.tagName() != "h4") {

            //print(tag)


            if (!parsedTitle.isDefined) {
              parsedTitle = candidateValue
            }

            //property=og:title is a high confidence rule which should supercede everything
            if (attribute.getValue == "og:title") {
              parsedTitle = candidateValue
            }

          }

          if (attribute.getValue.contains("image") &&
            !attribute.getValue.contains(".png") &&
            !attribute.getValue.contains(".gif") &&
            tag.tagName() != "div"
          ) {

            //println(tag)
            //println(isLogoImage(candidateValue.get))

            val isImage = candidateValue.get.contains(".jpg") ||
              candidateValue.get.contains(".jpeg") ||
              candidateValue.get.contains(".png") ||
              candidateValue.get.contains(".gif") ||
              candidateValue.get.takeRight(4) == "jpeg"

            if (!parsedImageUrl.isDefined &&
              isImage &&
              !isLogoImage(candidateValue.get)) {
              //println("SET!!!!!!!!!!!!")
              parsedImageUrl = candidateValue
            }


            //property=og:image is a very high confidence rule, so let this supercede everything
            if (attribute.getKey() == "property" && attribute.getValue == "og:image" && !isLogoImage(candidateValue.get)) {
              //println("SET BY itemprop=og:image !!!!!!")
              parsedImageUrl = candidateValue
            }


          }

          if (attribute.getValue.contains("sku") &&
            tag.tagName() != "a" &&
            !attribute.getValue.contains("price")) {


            if (!parsedSku.isDefined &&
              candidateValue.get.size < 15 &&
              candidateValue.get.size > 4 &&
              numPattern.findFirstIn(candidateValue.get).isDefined) {


              parsedSku = candidateValue
            }
          }

          if ((attribute.getValue.contains("stock") ||
            attribute.getValue.contains("availability") ||
            attribute.getValue.contains("available") ||
            attribute.getValue.contains("onhand")) &&
            attribute.getValue != "stocknum" &&
            !attribute.getValue.contains("availableformat") &&
            !attribute.getValue.contains("price") &&
            tag.tagName() != "a" &&
            !attribute.getValue.toLowerCase().contains("overstock") &&
            tag.tagName() != "button" &&
            tag.tagName() != "li"
          ) {


            if (!parsedAvailability.isDefined &&
              !candidateValue.get.toLowerCase().contains("overstock") &&
              !candidateValue.get.toLowerCase().contains("{") && //midwayusa rule
              candidateValue.get.size < 45) {
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

