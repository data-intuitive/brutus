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

  val ping: Endpoint[String] = get("ping") { Ok("Pong") }

  val geneDictionary = Genes(geneDictionaryFile)
  val drugbankData = DrugBank(drugbankDataFile)
  val newdrugbankData = NewDrugBank(newdrugbankDataFile)

  def simpleTransform(s: String):String =  
    s.replace("-", " ") // Avoid some strange cases...

  def fuzzyMatch(strWithCase:String, db:Array[NewDrugBankRecord]):Array[NewDrugBankRecord] = {

    val str = simpleTransform(
      strWithCase.toLowerCase
    )

    db.filter{entry =>
      (entry.syn.map(x => simpleTransform(x.toLowerCase)).filter(_.contains(str)).length > 0) ||
      (entry.productName.map(x => simpleTransform(x.toLowerCase)).filter(_.contains(str)).length > 0) ||
      (entry.genericName.map(x => simpleTransform(x.toLowerCase)).filter(_.contains(str)).length > 0) ||
      (entry.externalID.map(x => simpleTransform(x.toLowerCase)).filter(_.contains(str)).length > 0) ||
      (entry.accn == Some(str))
    }
  }

  /**
    * /gene/symbol/<SYMBOL> results in an exact match or an empty result
    *
    * Example:
    *   http "localhost:8082/gene/symbol/MELK"
    *
    * 404 is returned when no entry is available.
    */
  val exactSymbolSearch: Endpoint[GenesRecord] =
    get("gene" :: "symbol" :: path[String]) { (s: String) =>
      Ok(geneDictionary(s))
    } handle {
      case e: NoSuchElementException => NotFound(e) // or BadRequest
    }

  /**
    * /genes/symbol/<PART_OF_SYMBOL> results in a list of gene records
    *
    * Example:
    *   http "localhost:8082/genes/symbol/EDE"
    */
  val symbolSearch: Endpoint[List[GenesRecord]] =
    get("genes" :: "symbol" :: path[String]) { (s: String) =>
      if (s != "") Ok(geneDictionary.filterKeys(_ contains s).values.toList)
      else Ok(geneDictionary.values.toList)
    }

  /**
    * /newdrugbank/inchikey/key results in an exact match with the name
    *
    * Example:
    *   http "localhost:8082/newdrugbank/inchikey/..."
    *
    * A 404 is returned if no entry is found.
    */
  val exactDrugbank2ByInchiKey: Endpoint[NewDrugBankRecord] = get(
    "newdrugbank" :: "inchikey" :: path[String]) { (s: String) =>
    FuturePool.unboundedPool { Ok(newdrugbankData.filter(_.inchiKey.getOrElse("NA").replace(' ','-') == s).head) }
  } handle {
    case e: NoSuchElementException => NotFound(e) // or BadRequest
  }

 /**
   * /newdrugbank/fuzzy/<query> results in an exact match with the name
    *
    * Example:
    *   http "localhost:8082/newdrugbank/fuzzy/..."
    *
    * A 404 is returned if no entry is found.
    */
  val exactDrugbank2Fuzzy: Endpoint[NewDrugBankRecord] = get(
    "newdrugbank" :: "fuzzy" :: path[String]) { (s: String) =>
    FuturePool.unboundedPool { Ok(fuzzyMatch(s, newdrugbankData).head) }
  } handle {
    case e: NoSuchElementException => NotFound(e) // or BadRequest
  }

  val exactDrugbank2ByName: Endpoint[NewDrugBankRecord] = get(
    "newdrugbank" :: "name" :: path[String]) { (s: String) =>
    FuturePool.unboundedPool { Ok(newdrugbankData.filter(_.brandName.toSet.contains(s)).head) }
  } handle {
    case e: NoSuchElementException => NotFound(e) // or BadRequest
  }

  /**
    * /drugbank/name/<name> results in an exact match with the name
    *
    * Example:
    *   http "localhost:8082/drugbank/name/CETRORELIX"
    *
    * A 404 is returned if no entry is found.
    */
  val exactDrugbankByName: Endpoint[DrugBankRecord] = get(
    "drugbank" :: "name" :: path[String]) { (s: String) =>
    FuturePool.unboundedPool { Ok(drugbankData.filter(_.name == s).head) }
  } handle {
    case e: NoSuchElementException => NotFound(e) // or BadRequest
  }

  /**
    * /drugbanks/name/<name> results in an exact match with the name
    *
    * Example:
    *   http "localhost:8082/drugbank/name/CETRORELIX"
    *
    * A 404 is returned if no entry is found.
    */
  val drugbankByName: Endpoint[List[DrugBankRecord]] =
    get("drugbanks" :: "name" :: path[String]) { (s: String) =>
      FuturePool.unboundedPool {
        Ok(drugbankData.filter(_.name contains s).toList)
      }
    }

  /**
    * /drugbank/jnjs/<jnj> results in an exact match with the jnj
    *
    * Example:
    *   http "localhost:8082/drugbank/jnjs/17096768"
    *
    * A 404 is returned if no entry is found.
    */
  val drugbankByJNJs: Endpoint[DrugBankRecord] = get(
    "drugbank" :: "jnjs" :: path[String]) { (s: String) =>
    FuturePool.unboundedPool {
      Ok(drugbankData.filter(_.jnjs.contains(s)).head)
    }
  } handle {
    case e: NoSuchElementException => NotFound(e) // or BadRequest
  }

  val api =
    (exactDrugbank2Fuzzy :+: exactDrugbank2ByInchiKey :+: exactDrugbank2ByName :+: exactSymbolSearch :+: symbolSearch :+: exactDrugbankByName :+: drugbankByName :+: drugbankByJNJs :+: ping).toService

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
