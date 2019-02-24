package com.dataintuitive.brutus

import Model._
import java.net.URLEncoder
import scala.collection.immutable.Map

object Matching {

  def apply(inputFile: String): Map[String, String] = {

    val matchingRaw:Array[Array[String]] = scala.io.Source
      .fromFile(inputFile)(scala.io.Codec.ISO8859)
      .getLines
      .map(_.split("\t").map(_.trim))
      .toArray

      val dataWithoutHeader:Map[String,String] = matchingRaw
        .drop(1)
        .map(arr => (arr(0), arr(1)))
        .toMap

      dataWithoutHeader

  }

}
