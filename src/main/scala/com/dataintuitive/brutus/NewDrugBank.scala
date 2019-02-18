package com.dataintuitive.brutus

import Model._
import java.net.URLEncoder

object NewDrugBank {

  def apply(inputFile: String): Array[NewDrugBankRecord] = {
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
                  parseArray(25, a: _*)
              }
                .toArray

                dataWithoutHeader
  }

  def parseOptionString(os: Option[String]): Option[String] = os match {
    case None     => None
    case Some("") => None
    case _        => os
  }

  def parseListString(los: Option[String]): List[String] = 
    los.map(_.split('|').map(_.trim).toList).getOrElse(List[String]()) 


  def parseArray(nrItems: Int, strings: String*): NewDrugBankRecord = {
    val asF = strings.lift
    val pars = (0 to nrItems - 1).map(i => asF(i)).toList
    NewDrugBankRecord(
      accn = parseOptionString(pars(0)),
      canonicalSmiles = parseOptionString(pars(1)),
      tautomericSmiles = parseOptionString(pars(2)),
      targetGeneName = parseListString(pars(3)),
      targetName = parseListString(pars(4)),
      externalID = parseListString(pars(5)),
      genericName = parseListString(pars(6)),
      casNum = parseOptionString(pars(7)),
      highestPhase = parseOptionString(pars(8)),
      reference = parseListString(pars(9)),
      therapeuticGroup = parseOptionString(pars(10)),
      mechanismOfAction = parseOptionString(pars(11)),
      syn = parseListString(pars(12)),
      productName = parseListString(pars(13)),
      launched = parseListString(pars(14)),
      stopped = parseListString(pars(15)),
      emaProductCode = parseListString(pars(16)),
      emaMaNumber = parseListString(pars(17)),
      organization = parseOptionString(pars(18)),
      patentNumbers = parseListString(pars(19)),
      searchName = parseListString(pars(20)),
      searchField = parseOptionString(pars(21)),
      brandName = parseOptionString(pars(22)),
      inchi = parseOptionString(pars(23)),
      inchiKey = parseOptionString(pars(24))
    )
  }

}
