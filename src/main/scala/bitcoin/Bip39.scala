package bitcoin

import bitcoin.model.Hash
import bitcoin.model.InvalidPassphraseException
import bitcoin.wordList.Bip39WordList
import bitcoin.model.ImplicitTypes.HashString

/**
  * Created by h.kawayoke on 3/16/16.
  */
object Bip39 {
  val languages = List(
    "japanese","french","spanish","italian","english","chinese-sim","chinese-tra","jpn","fra","spa","ita","eng"
  )
  def convert(hash: Hash,lang: String = "English"): List[String] = {
    require(languages.contains(lang.toLowerCase),"Invalid Language")
    s"${hash.binary}${hash.raw.getCSFromHex}".toEach11Bits.map{ bits =>
      Bip39WordList.list(lang.toLowerCase match {
        case "japanese" | "jpn" => "Japanese"
        case "french" | "fra" => "French"
        case "spanish" | "spa" => "Spanish"
        case "italian" | "ita" => "Italian"
        case "english" | "eng" => "English"
        case "chinese-sim" => "Chinese-sim"
        case "chinese-tra" => "Chinese-tra"
      })(bits.toDecimalFromBinary.toInt)
    }
  }

  def decode(phrase: List[String]): Either[Throwable,Hash] = {
    require(phrase.length > 0,"phrase list must have phrases")
    Bip39WordList.getLanguage(phrase.head) match {
      case Some(lang)  =>
        phrase.map{ x =>
          BigInt(Bip39WordList.list(lang).indexOf(x)).toString(2).padZero(11)
        }.mkString match {
          case x if x.isValidPassPhraseBinary => Right(Hash(x.dropRight(x.length % 32)))
          case _ => Left(new InvalidPassphraseException("Invalid passphrase: wrong phrase or invalid checksum"))
        }
      case None =>  Left(new InvalidPassphraseException("Invalid passphrase: language is invalid"))
    }
  }

  def decode(phrase: String,separator: Char): Either[Throwable,Hash] = decode(phrase.split(separator).toList)

  def translate(from: List[String],to: String): Either[Throwable,List[String]] = decode(from).right.map(convert(_,to))

  def translate(from: String,to: String,separator: Char): Either[Throwable,String] = {
    translate(from.split(separator).toList,to).right.map{_.mkString}
  }
}
