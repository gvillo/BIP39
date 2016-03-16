package bitcoin.wordList

import bitcoin.util.FileUtil
import scala.util.{Failure, Success}

/**
  * Created by h.kawayoke on 3/16/16.
  */
object Bip39WordList {
  def getFile(fileName: String) = {
    FileUtil.getAsStringList(fileName) match {
      case Success(list) =>
        list
      case Failure(e) =>
        throw new Exception("initial Load Failure!!",e)
    }
  }
  lazy val list:Map[String,List[String]] = Map(
    "English" -> getFile("/BIP39/WordList/english.txt"),
    "Japanese" -> getFile("/BIP39/WordList/japanese.txt"),
    "Italian" -> getFile("/BIP39/WordList/italian.txt"),
    "Spanish" -> getFile("/BIP39/WordList/spanish.txt"),
    "French" -> getFile("/BIP39/WordList/french.txt")
  )

  def getLanguage(anyWord: String):Option[String] = {
    for((key,value) <- list if value.contains(anyWord)){
      return Option(key)
    }
    None
  }

}
