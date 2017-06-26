package com.viglink.productparser.siteparsers

import com.viglink.productparser._
import org.jsoup.nodes.Document

object WayfairParser extends Parser {
  override def getTitle(doc: Document): Option[String] = {
    ParseUtils.tagParser(doc, "meta", "property", "og:title", Content())
  }

  override def getAvailability(doc: Document): Option[String] = {
    None
  }

  override def getSku(doc: Document): Option[String] = {
    ParseUtils.tagParser(doc, "input", "name", "sku", Value())
  }

  override def getPrice(doc: Document): Option[String] = {
    ParseUtils.tagParser(doc, "span", "id", "sale_price", Text())
  }

  override def getImageUrl(doc: Document): Option[String] = {
    ParseUtils.tagParser(doc, "meta", "property", "og:image", Content())
  }
}
