package com.dataintuitive.brutus

import Model._
import java.net.URLEncoder

object DrugBank {

  def apply(inputFile: String): Array[DrugBankRecord] = {
    val drugbankRaw = scala.io.Source
      .fromFile(
        inputFile)(
        scala.io.Codec.ISO8859)
      .getLines
      .map(_.split("\t").map(_.trim))
      .toArray

    val dataWithoutHeader = drugbankRaw
      .drop(1)
      .zipWithIndex
      .map {
        case (a, i) =>
          parseArray(11, a: _*)
      }
      .toArray

    dataWithoutHeader
  }

  def parseOptionString(os: Option[String]): Option[String] = os match {
    case None     => None
    case Some("") => None
    case _        => os
  }

  def parseArray(nrItems: Int, strings: String*): DrugBankRecord = {
    val asF = strings.lift
    val pars = (0 to nrItems - 1).map(i => asF(i)).toList
    DrugBankRecord(
      parseOptionString(pars(0)).get.split("\\|").head,
      "http://localhost:9999/molecule/" + URLEncoder.encode(parseOptionString(pars(1)).get, "UTF-8"),
      parseOptionString(pars(2)),
      parseOptionString(pars(3)),
      parseOptionString(pars(4)),
      parseOptionString(pars(5)),
      parseOptionString(pars(6)),
      parseOptionString(pars(7)),
      parseOptionString(pars(8)),
      parseOptionString(pars(9)),
      parseOptionString(pars(10))
    )
  }

}
