package com.viglink.productparser.siteparsers

import com.viglink.productparser._
import org.jsoup.nodes.Document


object LowesParser extends Parser{
  override def getTitle(doc: Document): Option[String] = {
    ParseUtils.tagParser(doc, "meta", "property", "og:title", Content())
  }

  override def getAvailability(doc: Document): Option[String] = {
    ParseUtils.tagParser(doc, "input", "name", "quantity-onhand", Value())
  }

  override def getSku(doc: Document): Option[String] = {
    None
  }

  override def getPrice(doc: Document): Option[String] = {
    ParseUtils.tagParser(doc, "span", "itemprop", "price", Content())
  }

  override def getImageUrl(doc: Document): Option[String] = {
    ParseUtils.tagParser(doc, "img", "class", "product-image pf-epc", Source())
  }
}
