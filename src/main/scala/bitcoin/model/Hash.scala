package bitcoin.model

import bitcoin.Crypt.generateHash

/**
  * Created by h.kawayoke on 3/16/16.
  */
case class Hash(typeOrHash: String) {
  import bitcoin.model.ImplicitTypes.HashString

  val (raw: String,binary: String,types: String) = typeOrHash match {
    case "SHA-256" | "SHA256" | "sha256" | "sha-256" =>
      val hash = generateHash()
      (hash,hash.toBinaryFromHex,"SHA-256")
    case "SHA-512" | "SHA512" | "sha512" | "sha-512" =>
      val hash = generateHash("SHA-512")
      (hash,hash.toBinaryFromHex,"SHA-512")
    case "MD5" | "md5" =>
      val hash = generateHash("MD5")
      (hash,hash.toBinaryFromHex,"MD5")
    case hash if hash.length == 128 && hash.isHex  =>
      (hash,hash.toBinaryFromHex,"SHA-512")
    case hash if hash.length == 64 && hash.isHex =>
      (hash,hash.toBinaryFromHex,"SHA-256")
    case hash if hash.length == 32 && hash.isHex =>
      (hash,hash.toBinaryFromHex,"MD5")
    case binary if binary.length == 512 && binary.isBinary =>
      (binary.toHexFromBinary,binary,"SHA-512")
    case binary if binary.length == 256 && binary.isBinary =>
      (binary.toHexFromBinary,binary,"SHA-256")
    case binary if binary.length == 128 && binary.isBinary =>
      (binary.toHexFromBinary,binary,"MD5")
    case _ =>
      throw new Exception("invalid argument")

  }
}
