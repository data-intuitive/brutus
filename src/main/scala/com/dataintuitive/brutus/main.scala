package com.dataintuitive.brutus

import com.twitter.finagle.Http
import com.twitter.util.Await
import com.twitter.server.TwitterServer
import com.twitter.util.FuturePool
import com.twitter.logging.Formatter

import io.finch._
import io.finch.circe._
import io.finch.syntax._
import io.circe.generic.auto._
import com.twitter.finagle.http.filter.Cors

import Model._
import Genes._
import Matching._

import com.typesafe.config.ConfigFactory
import scala.util.Try
import java.io.File

object Server extends TwitterServer {

  case class SymbolLookup(symbol: String)

  val myConfigFile = new File("etc/brutus.conf")
  val fileConfig = ConfigFactory.parseFile(myConfigFile).getConfig("brutus")
  val config = ConfigFactory.load(fileConfig)

  val port = Try(config.getString("api.port")).toOption.getOrElse("8082")

  val base="/Users/toni/Dropbox/_Janssen/ComPass/Architecture/Brutus/data/"
  val geneDictionaryFile = Try(config.getString("geneDictionaryFile")).toOption
    .getOrElse(base+"L1000 genes vs proteins.csv")
  val drugbankDataFile = Try(config.getString("drugbankDataFile")).toOption
    .getOrElse(base+"drugbank.csv")
  val newdrugbankDataFile = Try(config.getString("newdrugbankDataFile")).toOption
    .getOrElse(base+"drugbank_update.tsv")
  val matchingFile = Try(config.getString("matchingFile")).toOption
    .getOrElse(base+"matching-new.tsv")

  val ping: Endpoint[String] = get("ping") { Ok("Pong") }

  val geneDictionary = Genes(geneDictionaryFile)
  val drugbankData = DrugBank(drugbankDataFile)
  val newdrugbankData = NewDrugBank(newdrugbankDataFile)
  val matching = Matching(matchingFile)

  def simpleTransform(s: String):String =  
    s.replace("-", " ") // Avoid some strange cases...

  /**
    * /gene/symbol/<SYMBOL> results in an exact match or an empty result.
    * Case does not matter.
    *
    * Example:
    *   http "localhost:8082/gene/symbol/MELK"
    *
    * 404 is returned when no entry is available.
    */
  val exactSymbolSearch: Endpoint[GenesRecord] =
    get("gene" :: "symbol" :: path[String]) { (s: String) =>
      Ok(geneDictionary(s.toUpperCase))
    } handle {
      case e: NoSuchElementException => NotFound(e) // or BadRequest
    }

  /**
    * /genes/symbol/<PART_OF_SYMBOL> results in a list of gene records which
    * contains the query string. Case does not matter.
    *
    * Example:
    *   http "localhost:8082/genes/symbol/EDE"
    */
  val symbolSearch: Endpoint[List[GenesRecord]] =
    get("genes" :: "symbol" :: path[String]) { (s: String) =>
      if (s != "") Ok(
        geneDictionary
          .filterKeys(_ contains s.toUpperCase)
          .values
          .toList
      )
      else Ok(geneDictionary.values.toList)
    }

  /**
    * /drugbank/jnjs/<jnj> results in an exact match with the jnj
    *
    * Example:
    *   http "localhost:8082/drugbank/jnjs/17096768"
    *
    * A 404 is returned if no entry is found.
    *
    * Please note: 2 lookups are performed in one go. In future versions, we might
    * add support for the two steps individually.
    */
  val drugbankByJNJs: Endpoint[NewDrugBankRecord] = get(
    "drugbank" :: "jnjs" :: path[String]) { (s: String) =>
    FuturePool.unboundedPool {
      Ok(newdrugbankData.filter(_.accn.exists(_ == matching(s))).head)
    }
  } handle {
    case e: NoSuchElementException => NotFound(e) // or BadRequest
  }

  val api =
    ( exactSymbolSearch :+: 
      symbolSearch :+: 
      drugbankByJNJs :+: 
      ping
    ).toService

  val policy: Cors.Policy = Cors.Policy(
    allowsOrigin = _ => Some("*"),
    allowsMethods = _ => Some(Seq("GET", "POST")),
    allowsHeaders = _ => Some(Seq("Accept"))
  )

  val service = new Cors.HttpFilter(policy).andThen(api)

  def main(): Unit = {
    val server = Http.server
      .withStatsReceiver(statsReceiver)
      .serve(":" + port, service)
    onExit {
      server.close()
    }
    Await.ready(adminHttpServer)
  }

}
