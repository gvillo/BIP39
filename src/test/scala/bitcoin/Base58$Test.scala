package bitcoin

import org.scalatest.FlatSpec

/**
  * Created by h.kawayoke on 3/16/16.
  */
class Base58$Test extends FlatSpec {
  "any BigInt" should "be converted to base58" in {
    assert(Base58.convert(BigInt("82347958239")).length > 0)
  }

  "decoded converted number" should "be equals to the original number" in {
    val testNumber = "123123123123"
    assert(Base58.decode(Base58.convert(BigInt(testNumber))) == testNumber )
  }

}
