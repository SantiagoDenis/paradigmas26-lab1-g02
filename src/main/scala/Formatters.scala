import scala.io.Source
import org.json4s._
import org.json4s.jackson.JsonMethods._
import Types.Subscription
import Types.Post

object Formatters {

//Given a relative path, it returns a Subscripton type
 def getSubscriptions(path: String): List[Subscription] = {
    implicit val formats: Formats = DefaultFormats
    val source = Source.fromFile(path)
    val content = source.mkString
    source.close()

    val json = parse(content)
    json.children.map{elem => 
      val name = (elem \ "name").extract[String]
      val url = (elem \ "url").extract[String]
      (name,url)}
    // val subredditNames = (json \ "name").extract[List[String]]
    // val subredditUrls = (json \ "url").extract[List[String]]

    // subredditNames zip subredditUrls
  }
 
 //Given a List of Subscription, it returns a Post type
 def getPosts(subs: List[Subscription]): List[Post] = {
    implicit val formats: Formats = DefaultFormats

    // List of posts for every sub
    subs.flatMap { sub =>
      val (name, url) = sub

      val source = scala.io.Source.fromURL(url)
      val content = source.mkString
      source.close()

      val json = parse(content)
      val children = (json \ "data" \ "children").children

      children.map { c =>
        val data = c \ "data"
        val title = (data \ "title").extract[String]
        val selftext = ( data \ "selftext").extract[String]

        val createdUTC = ( data \ "created_utc").extract[Double].toLong
        val date = TextProcessing.formatDateFromUTC(createdUTC)

        (name, title, selftext, date)
      }
    }
  }

  def filterPosts (posts: List[Post]): List[Post] = {
    posts.filter { post => val (subrredit, title, text, date) = post
                  title.trim.nonEmpty && text.trim.nonEmpty 
    }
  }
}
