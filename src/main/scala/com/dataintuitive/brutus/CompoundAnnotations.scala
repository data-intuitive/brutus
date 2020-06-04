package com.dataintuitive.brutus

import java.net.URLEncoder

/**
 * (Relatively) generic component to load a dump file with compound annotations into
 * the proper case class for query lookup.
 */
object CompoundAnnotations {

  // Helper function
  // TODO: Should be moved to utils package
  def extractFeaturesMap(d:Array[Array[String]],
    features:Seq[String],
    includeHeader:Boolean = false
    ): Array[Map[String, Option[String]]] = {
      val header = d.head.map(_.trim)  // Be on the safe side !
      val featureIndices = features.map(value => header.indexOf(value))
      val selection = d
        .map(row =>
            featureIndices
              .map(valueIndex => row.lift(valueIndex))
              .zip(features)
              .toMap
              .map(_.swap)
              .withDefault(_ => None)
              )
        if (!includeHeader)
          selection.zipWithIndex.filter(_._2 > 0).map(_._1)
        else
          selection
  }

  case class ExternalId(source: String, id: Option[String], link: Option[String])

  case class CompoundAnnotation(
    id: String,
    externalID: Seq[ExternalId],
    genericName: Seq[String],
    therapeuticGroup: Seq[String],
    mechanismOfAction: Option[String],
    syn: Seq[String],
    searchField: Option[String],
    targetGenes: Seq[String],
    name: Option[String],
    clinicalPhase: Option[String],
    indication: Seq[String],
    version: String = "v1"
  ) extends Serializable

  def apply(
    inputFile: String,
    delimiter:String = "\t",
    dict:Map[String, String],
    external:Map[String, String],
    sources:Map[String, String]):Array[CompoundAnnotation] = {

    val dataRaw = scala.io.Source
      .fromFile(inputFile)(scala.io.Codec.ISO8859)
      .getLines
      .map(_.split(delimiter).map(_.trim))
      .toArray

    val fullDict = dict ++ external

    val asMap:Array[Map[String, Option[String]]] = extractFeaturesMap(dataRaw, fullDict.map(_._2).toSeq, false)

    val safeDict = dict.withDefault( _ => "N/A")
    val safeExternal = external.withDefault( _ => "N/A")

    asMap.map{ x =>

      val xd:Map[String,Option[String]] = x.withDefault(_ => None)

      val externalIds = external.map{ case(source, feature) => xd(feature) match {
        case Some(str) if (str != "") =>
          Some(ExternalId(source, xd(feature), xd(feature).map(f => sources.get(source).getOrElse("") + f)) )
        case _ =>
          None
      }}.flatMap(x => x).toSeq

      CompoundAnnotation(
        xd(safeDict("id")).getOrElse("No ID!"),
        externalIds,
        xd(safeDict("genericName")).map(_.split("\\|").toSeq).getOrElse(Seq()),
        xd(safeDict("therapeuticGroup")).map(_.split("\\|").toSeq).getOrElse(Seq()),
        xd(safeDict("mechanismOfAction")),
        xd(safeDict("syn")).map(_.split("\\|").toSeq).getOrElse(Seq()),
        xd(safeDict("searchField")),
        xd(safeDict("targetGenes")).map(_.split("\\|").toSeq).getOrElse(Seq()),
        xd(safeDict("name")),
        xd(safeDict("clinicalPhase")),
        xd(safeDict("indication")).map(_.split("\\|").toSeq).getOrElse(Seq()),
        "v1"
      )
    }
  }
}
