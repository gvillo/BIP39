package bitcoin

import bitcoin.Crypt

/**
  * Created by h.kawayoke on 3/16/16.
  */
object Base58 {

  val BASE_58_SEED = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz".toList.map(_.toString)
  val LENGTH = BASE_58_SEED.length

  def convert(decimal: BigInt,str: String = ""):String = {
    if(decimal > 0){
      convert((decimal - (decimal % LENGTH)) / LENGTH,BASE_58_SEED((decimal % LENGTH).toInt) + str)
    }else{
      BASE_58_SEED((decimal % LENGTH).toInt) + str
    }
  }

  def decode(base58: String,as: String = "DECIMAL"): String = {
    require(!base58.matches(s"[^$BASE_58_SEED]"))
    base58.reverse.toArray.zipWithIndex.map{ case (item,index) =>
      BigInt(BASE_58_SEED.indexOf(item.toString)) * BigInt(LENGTH).pow(index)
    }.sum.toString(as.toLowerCase match {
      case "hex" | "hash" => 16
      case "binary" => 2
      case "decimal" | _ => 10
    })
  }

  def checkEncode(plain: String,prefixType: String = "address"):String = {
    require(List("address","script-hash","address-t","wif","bip38","bip32").contains(prefixType.toLowerCase))
    val prefix = prefixType match {
      case "address" => "00"
      case "script-hash" => "05"
      case "address-t" => "6F"
      case "wif" => "80"
      case "bip38" => "0142"
      case "bip32" =>  "0488B21E"
    }
    val hash160 = Crypt.hash160(plain)
    convert(BigInt(s"$prefix$hash160" + Crypt.hash256(s"$prefix$hash160").take(8),16))
  }
}
