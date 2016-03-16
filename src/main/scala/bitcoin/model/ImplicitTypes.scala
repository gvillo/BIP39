package bitcoin.model

import java.security.MessageDigest

/**
  * Created by h.kawayoke on 3/16/16.
  */
object ImplicitTypes {

  implicit class HashString(str: String){

    val ERROR_BINARY = "Invalid binary string!"
    val ERROR_HEX = "Invalid hex string!"
    val HEX_REG_EXP = "[0-9a-fA-F]"

    def checksum: String = {
      MessageDigest.getInstance("SHA-256").digest(str.getBytes)
        .map("%02x".format(_)).mkString
    }
    def padZero(digits: Int): String = str.reverse.padTo(digits,'0').reverse

    def toEach11Bits:List[String] = ".{11}".r.findAllIn(str).toList

    def isHex: Boolean = str.replaceAll(HEX_REG_EXP,"").isEmpty

    def isBinary: Boolean = str.replaceAll("[0-1]","").isEmpty

    def isValidPassPhraseBinary: Boolean = {
      try{
        str.takeRight(str.length % 32) == str.dropRight(str.length % 32).toHexFromBinary.getCSFromHex
      }catch{
        case e:Exception =>
          e.printStackTrace()
          false
      }
    }

    def toBinaryFromHex:String = {
      require(str.isHex,ERROR_HEX)
      BigInt(str,16).toString(2).padZero(str.length * 4)
    }

    def toHexFromBinary:String = {
      require(str.isBinary,ERROR_BINARY)
      BigInt(str,2).toString(16).padZero(str.length / 4)
    }

    def toDecimalFromBinary:String = {
      require(str.isBinary,ERROR_BINARY)
      BigInt(str,2).toString(10)
    }

    def getCSFromHex: String = {
      require(str.isHex,ERROR_HEX)
      str.checksum.toBinaryFromHex.take(str.toBinaryFromHex.length / 32)
    }

    def hex2bytes: Array[Byte] = {
      require(str.isHex,ERROR_HEX)
      str.replaceAll(HEX_REG_EXP, "").sliding(2, 2).toArray.map(Integer.parseInt(_, 16).toByte)
    }
  }

  implicit class hexBytes(bytes: Array[Byte]){
    def toHex: String = {
      bytes.map("%02x".format(_)).mkString
    }
  }
}