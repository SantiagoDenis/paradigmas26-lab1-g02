import scala.io.Source
import org.json4s._
import org.json4s.jackson.JsonMethods._
import Types.Subscription
import Types.Post
import StopWords.words

object Formatters {

//Given a relative path, it returns a Subscripton type
 def getSubscriptions(path: String): Option[List[Subscription]] = {
    implicit val formats: Formats = DefaultFormats
    try{
      val source = Source.fromFile(path)

      try{
        val content = source.mkString
        val json = parse(content)
        Some(json.children.map{elem => 
          val name = (elem \ "name").extract[String]
          val url = (elem \ "url").extract[String]
          (name,url)})

      }
      catch{
        case _: Exception => None
      }
      finally{
        source.close()
      }
    }
    catch{
      case _: Exception => None
    }
  }
 
 //Given a List of Subscription, it returns a Post type
 def getPosts(subs: List[Subscription]): Option[List[Post]] = {
    implicit val formats: Formats = DefaultFormats

    // List of posts for every sub
    Some(subs.flatMap { sub =>
      val (name, url) = sub
      try{
        val source = scala.io.Source.fromURL(url)

        try{
          val content = source.mkString
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
        catch{
          case _: Exception => Nil
        }
        finally{
          source.close()
        }
      }
      catch{
        case _: Exception => Nil
      }
    })
  }

  def filterPosts (posts: List[Post]): List[Post] = {
    posts.filter { post => val (subrredit, title, text, date) = post
                  title.trim.nonEmpty && text.trim.nonEmpty 
    }
  }

  def countFrecuency (posts: List[Post]): List[(String, Int)] = {
    implicit val formats: Formats = DefaultFormats

    val listofStrings = posts.flatMap{post => 
      val subrredit = post._1
      val title = post._2
      val text = post._3
      (" " + subrredit + " " + title + " " + text + " ").split(' ')
      }.filter(word => word.exists(_.isUpper) || (!words.contains(word) &&
    !words.contains(word.toLowerCase) && word != "")).groupBy(word=>word).mapValues(_.size)  
    
    listofStrings.toList.sortBy(-_._2)
  }
}
