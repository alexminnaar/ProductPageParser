package com.viglink.productparser.siteparsers

import com.viglink.productparser.{Content, ParseUtils, Parser, Text}
import org.jsoup.nodes.Document


object RodaleParser extends Parser {
  override def getTitle(doc: Document): Option[String] = {
    ParseUtils.tagParser(doc, "meta", "property", "og:title", Content())
  }

  override def getAvailability(doc: Document): Option[String] = {
    ParseUtils.tagParser(doc, "meta", "itemprop", "sku", Content())
  }

  override def getSku(doc: Document): Option[String] = {
    ParseUtils.tagParser(doc, "meta", "itemprop", "sku", Content())
  }

  override def getPrice(doc: Document): Option[String] = {
    ParseUtils.tagParser(doc, "span", "itemprop", "price", Text())
  }

  override def getImageUrl(doc: Document): Option[String] = {
    ParseUtils.tagParser(doc,"meta","property","og:image",Content())
  }
}

