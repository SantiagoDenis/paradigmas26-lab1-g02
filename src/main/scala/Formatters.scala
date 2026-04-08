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
            val score = (data \ "score").extract[Int]
            (name, title, selftext, date, score)
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
    posts.filter { post => val (subrredit, title, text, date, score) = post
                  title.trim.nonEmpty && text.trim.nonEmpty 
    }
  }

  def countFrecuency (posts: List[Post]): List[(String,List[(String, Int)])] = {
    implicit val formats: Formats = DefaultFormats

    val listofStrings = posts.map{post => 
      val subrredit = post._1
      val title = post._2
      val text = post._3
      val publication = (" " + title + " " + text + " ").split("\\s+")
      (subrredit, publication)
      }.groupBy(_._1).map{case (subName, totalPostsInSub) => 
      val concat_intern_lists = totalPostsInSub.flatMap(_._2)
      val filter_words = concat_intern_lists.filter{word => 
      val lowercase = word.toLowerCase 
      word.forall{letter => 
        val ascii_code = letter.toInt 
      (64<ascii_code && ascii_code<91) || (96<ascii_code && ascii_code<123)} && 
      ((word.exists(_.isUpper) && !words.contains(lowercase)) || 
      (!words.contains(word) && !words.contains(lowercase) && 
      word != ""))}.groupBy(word=>word).mapValues(_.size)
      (subName, filter_words)}  
    // basicamente agarro las secciones title y text de cada post, las concateno como un unico
    // string, lo hago una lista de palabras y a esta lista la asocio al subrredit al que pertenecian el
    // title/text. Luego quedan tuplas (subrredit, lista de palabras), colapso las tuplas para que haya una 
    // unica tupla por cada subrredit que tenga todas las palabras del mismo. Por ultimo en cada subrredit 
    // se descartan aquellas palabras que no cumplen las condiciones adecuadas y se cuenta
    // la frecuencia de aquellas que quedaron. A la salida va a quedar una tupla por cada subrredit
    // con una lista que cuenta la frecuencia de las palabras en cada uno.
    listofStrings.toList.map{case(subName, countFrec) => 
      (subName, countFrec.toList.sortBy(-_._2) )}
  }

  def getScores(posts: List[Post]): Map[String, Int] = {
    val grouped = posts.groupBy(post => post._1)
    grouped.map{
      case (subreddit, listOfPosts) => 
        val totalScore = listOfPosts.foldLeft(0)(acc, post) => acc + post._5 //aca con post ._5 estoy sumando el score que es el 5to lugar
      (subreddit, totalScore)
    }
  }

}
