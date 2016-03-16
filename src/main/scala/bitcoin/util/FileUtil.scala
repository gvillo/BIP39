package bitcoin.util

import scala.io.Source
import scala.util.Try

/**
  * Created by h.kawayoke on 3/16/16.
  */
object FileUtil {
  def getAsStringList(filePath: String):Try[List[String]] = {
    Try {
      val s = Source.fromInputStream(getClass.getResourceAsStream(filePath))
      try s.getLines.toList
      finally s.close
    }
  }
}
