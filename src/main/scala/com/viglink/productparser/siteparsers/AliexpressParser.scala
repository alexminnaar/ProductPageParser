package com.viglink.productparser.siteparsers

import com.viglink.productparser.{Content, ParseUtils, Parser, Text}
import org.jsoup.nodes.Document


object AliexpressParser extends Parser {
  override def getTitle(doc: Document): Option[String] = {
    ParseUtils.tagParser(doc, "meta", "property", "og:title", Content())
  }

  override def getAvailability(doc: Document): Option[String] = {
    None
  }

  override def getSku(doc: Document): Option[String] = {
    None
  }

  override def getPrice(doc: Document): Option[String] = {
    ParseUtils.tagParser(doc, "span", "itemprop", "price", Text())

  }

  override def getImageUrl(doc: Document): Option[String] = {
    ParseUtils.tagParser(doc, "meta", "property", "og:image", Content())
  }
}
