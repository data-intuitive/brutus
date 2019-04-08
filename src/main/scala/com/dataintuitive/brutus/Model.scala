
package com.dataintuitive.brutus

object Model {

  case class GenesRecord(
    probesetID: String,
    entrezid: String,
    ensembl: String,
    symbol: String,
    name: String,
    synonyms: Option[String],
    protein: Option[String],
    uniprot: Option[String],
    functionClass: Option[String],
    function: Option[String],
    remarks: Option[String],
    involved: Option[String]
  ) extends Serializable


  case class DrugBankRecord(name: String, smiles:String,
    tautomericSmiles:Option[String],
    Accn: Option[String], 
    drugBankMechanismOfAction: Option[String], drugBankTargetGeneName: Option[String], 
    iupharMechanismOfAction: Option[String], iupharTargetGeneName:Option[String],
    chemblMechanismOfAction: Option[String], chemblTargetGeneName:Option[String],
    jnjs: Option[String], 
    version: String = "v1") extends Serializable

  case class NewDrugBankRecord(
    accn: Option[String],
    canonicalSmiles: Option[String],
    tautomericSmiles: Option[String],
    targetGeneName: List[String],
    targetName: List[String],
    externalID: List[String],
    genericName: List[String],
    casNum: Option[String],
    highestPhase: Option[String],
    reference: List[String],
    therapeuticGroup: Option[String],
    mechanismOfAction: Option[String],
    syn: List[String],
    productName: List[String],
    launched: List[String],
    stopped: List[String],
    emaProductCode: List[String],
    emaMaNumber: List[String],
    organization: Option[String],
    patentNumbers: List[String],
    searchName: List[String],
    searchField: Option[String],
    brandName: Option[String],
    inchi: Option[String],
    inchiKey: Option[String],
    version: String = "v2"
  ) extends Serializable

def convertNewToOld(n: NewDrugBankRecord):DrugBankRecord = {
    DrugBankRecord(
      n.genericName.headOption.getOrElse("NA"),
      "", //smiles
      None, //tautomericSmiles
      n.accn,
      n.mechanismOfAction,
      Some(n.targetGeneName.mkString(" | ")),
      None,
      None,
      None,
      None,
      None,
      "v1"
      )
  }

}


