package com.dataintuitive.brutus

import Model._

object Genes {

  def apply(inputFile: String):Map[String, GenesRecord] = {
    val parsedGenes = scala.io.Source
      .fromFile(
        inputFile
      )(scala.io.Codec.ISO8859)
      .getLines
      .map(_.split(";").map(_.trim))
      .toList

    val genes = parsedGenes
      .drop(1)
      .zipWithIndex
      .map {
        case (a, i) =>
          parseGenesArray(12, a: _*)
      }
      .toArray

    // flatten symbol nested values
    val genesFlatSymbols = genes.flatMap(g =>
      g.symbol.split("///").map(_.trim).map(sym => g.copy(symbol = sym)))
    // genesFlatSymbols.length

    val defaultRecord = GenesRecord("No Probeset",
                                    "No EntrezID",
                                    "No Ensembl",
                                    "No Symbol",
                                    "No Name",
                                    None,
                                    None,
                                    None,
                                    None,
                                    None,
                                    None,
                                    None)
    genesFlatSymbols
      .map(g => (g.symbol.toUpperCase, g))
      .toMap
      //.withDefault(_ => defaultRecord)

  }

  def parseOptionString(os: Option[String]): Option[String] = os match {
    case None     => None
    case Some("") => None
    case Some("x") => None
    case _        => os
  }

  def parseGenesArray(nrItems: Int, strings: String*): GenesRecord = {
    // println(strings.zipWithIndex.map{case (el, i) => s"$i: $el"}.mkString(" -- "))
    val asF = strings.lift
    val pars = (0 to nrItems - 1).map(i => asF(i)).toList

    GenesRecord(
      parseOptionString(pars(0)).get,
      parseOptionString(pars(1)).get,
      parseOptionString(pars(2)).get,
      parseOptionString(pars(3)).get,
      parseOptionString(pars(4)).get,
      parseOptionString(pars(5)),
      parseOptionString(pars(6)),
      parseOptionString(pars(7)).map(ov => "http://www.uniprot.org/uniprot/" + ov),
      parseOptionString(pars(8)),
      parseOptionString(pars(9)),
      parseOptionString(pars(10)),
      parseOptionString(pars(11))
    )
  }

}
