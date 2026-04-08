import scala.io.Source
import org.json4s._
import org.json4s.jackson.JsonMethods._
import Types.Subscription

object Formatters {

//Given a relative path, it returns a Subscripton type
 def getSubscriptions(path: String): List[Subscription] = {
    implicit val formats: Formats = DefaultFormats
    val source = Source.fromFile(path)
    val content = source.mkString
    source.close()

    val json = parse(content)
    val subredditNames = (json \ "name").extract[List[String]]
    val subredditUrls = (json \ "url").extract[List[String]]

    subredditNames zip subredditUrls
  }

}
