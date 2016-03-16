package bitcoin

import java.security.MessageDigest
import gnu.crypto.hash.HashFactory
import scala.util.Random
import bitcoin.model.ImplicitTypes.HashString

/**
  * Created by h.kawayoke on 3/16/16.
  */
object Crypt {
  def generateHash(algorithm: String,seed: Array[Byte]): String = {
    algorithm match {
      case "RIPEMD-160" =>
        val md = HashFactory.getInstance("RIPEMD-160")
        md.update(seed, 0, seed.length)
        gnu.crypto.util.Util.toString(md.digest).toLowerCase
      case _ =>
        MessageDigest.getInstance(algorithm).digest(seed).map("%02x".format(_)).mkString
    }
  }
  def generateHash(algorithm: String = "SHA-256",seed: String = Random.alphanumeric.take(256).mkString): String = {
    generateHash(algorithm,seed.getBytes)
  }

  def generateHashByHex(algorithm: String = "SHA-256",seed: String): String = {
    require(seed.isHex)
    generateHash(algorithm,seed.hex2bytes)
  }

  def hash256(plain: String):String = generateHashByHex("SHA-256",generateHashByHex("SHA-256",plain))

  def hash160(plain: String):String = generateHashByHex("RIPEMD-160",generateHashByHex("SHA-256",plain))

}
