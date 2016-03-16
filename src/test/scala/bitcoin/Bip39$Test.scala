package bitcoin

import bitcoin.model.Hash
import bitcoin.model.InvalidPassphraseException
import org.scalatest.FlatSpec

/**
  * Created by h.kawayoke on 3/16/16.
  */
class Bip39$Test extends FlatSpec {
  "256bit hash" should "generates 24 words" in {
    assert(
      (1 to 100).toList.filter{ x =>
        Bip39.convert(Hash("SHA-256")).size != 24
      }.isEmpty
    )
  }
  "128bits hash" should "generates 12 words" in {
    assert(
      (1 to 100).toList.filter{ x =>
        Bip39.convert(Hash("MD5")).size != 12
      }.isEmpty
    )
  }
  "converted jp words" should "matches jp to en to jp" in {
    val japanese = Bip39.convert(Hash("SHA-256"),"Japanese")
    assert(
      Bip39.translate(Bip39.translate(japanese,"English").right.get,"Japanese").right.get.mkString == japanese.mkString
    )
  }

  "decoded hash" should "matches original hash" in {
    val sha256 = Hash("SHA-256")
    assert(
      (1 to 100).toList.filter { x =>
        sha256.raw != Bip39.decode(Bip39.convert(sha256)).right.get.raw
      }.isEmpty
    )
  }

  it should "throw InvalidPassphraseException if passphrase does not match to its checksum" in {
    assert(Bip39.decode(Bip39.convert(Hash("SHA-256")).tail).left.get.isInstanceOf[InvalidPassphraseException])
  }
}

