
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
                          jnjs: Option[String]) extends Serializable

}