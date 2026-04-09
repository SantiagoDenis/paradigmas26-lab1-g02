import scala.io.Source
import org.json4s._
import org.json4s.jackson.JsonMethods._
import Types.Subscription
import Types.Post
import StopWords.words
import scala.util.{Using, Try}

object Formatters {
 //Helper #1 para getSubscriptions: Dado un path, leeun archivo
 def readFile(path: String): Option[String] = {
    val attempt = Using(Source.fromFile(path)) {source =>  //El Using abre el archivo, ejecuts la funcion y lo cierra. Devuelve un Try[String] -> success si todo ok o failure si no
      source.mkString //Paso el contenido del archivo a string
    }
    attempt.toOption //el toOption convierte el Success(value) en Some(value) y el Failure en un None
 }
 //Helper #2: Dado el contenido de un archivo, lo parsea
 def parseSubs(content:String): Option[List[Subscription]] = {
    implicit val formats: Formats = DefaultFormats
    Try {
      val json = parse(content)
      json.children.map { elem =>
        val name = (elem \ "name").extract[String]
        val url = (elem \ "url").extract[String]
        (name, url)
      }
    }.toOption //De vuelta, tambien lo pasa a un Some si hay valor y a un None si no
 }
 //Dado el path a un archivo, lee lo que esta ahi, lo parsea y devuelve una lista con elem tipo Subscription
 def getSubscriptions(path: String): Option[List[Subscription]] = {
    val content = readFile(path)
    content match {
      case Some(content) => parseSubs(content)
      case None => None
    }
 }


 //Helper #1 para getPosts; dada una url, devuelve la data
 def readUrl(url:String): Option[String] = {
    val attempt = Using(Source.fromURL(url)){source => //El Source.fromUrl puede fallar, por eso usamos Using
      source.mkString
    }
    attempt.toOption //Si el using me da Success(valor) -> Some(valor); si me da Failure(valor) -> None
 }
//Helper #2; igual que parseSubs, este me parsea el contenido obtenido a un tipo Post, devuelve una Option con el valor o None si no salio
 def parsePosts(name: String, content: String): Option[List[Post]] = {
    implicit val formats: Formats = DefaultFormats
    Try {
      val json = parse(content)
      val children = (json \ "data" \ "children").children
      children.map{jsonPost => 
        val data = jsonPost \ "data"
        val title = (data \ "title").extract[String]
        val selftext = ( data \ "selftext").extract[String]
        val createdUTC = ( data \ "created_utc").extract[Double].toLong
        val date = TextProcessing.formatDateFromUTC(createdUTC)
        val score = (data \ "score").extract[Int]
        (name, title, selftext, date, score)
      }
    }.toOption //Some(valor) o None
 }
//Dado una lista de Subscription, te devuelve una lista de posts
 def getPosts(subs: List[Subscription]): List[Post] ={ //No devuelvo un Option[List[Post]] porque esto no falla completamente como getSubscriptions si no hay datos. Si algo falla, doy eso vacio y sigo obteniendo el resto de posts del resto de subreddits
    val posts = subs.flatMap{sub => //Recorro cada subreddit que parseo y junto todo en una lista de posts
      val (name, url) = sub
      val content = readUrl(url) //Puede ser Some(json) o None
      content match { //Puede salir bien y darme los posts, en cual caso los agregamos a la lista; o puede salir mal, en cual caso no agregamos nada a la lista y seguimos viendo
        case Some(jsonPosts) => parsePosts(name, jsonPosts) match {
          case Some(posts) => posts //Salio bien asiq lo agregamos
          case None => Nil //Fallo al parsear -> devolvemos Nil y no None (como en getSubscriptions) pq no queremos romper todo si falla, solo ignorarlo
        }
        case None => Nil //Fallo al traer de la url -> devolvemos algo vacio si no anduvo para ignorarlo, sin romper nada
      }
    }
    posts
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

//Helper para getStats
  def getScores(posts: List[Post]): Map[String, Int] = {
    val grouped = posts.groupBy(post => post._1)
    grouped.map{
      case (subreddit, listOfPosts) => 
        val totalScore = listOfPosts.foldLeft(0){(acc, post) => 
          acc + post._5
        } //aca con post ._5 estoy sumando el score que es el 5to lugar
      (subreddit, totalScore)
    }
  }

  def getStats(posts: List[Post]): String = {
    val scores = getScores(posts)
    val frecMap = countFrecuency(posts).toMap //lo paso a un Map porque me sirve el mismo tipo que tiene scores
    scores.map{
      case(subreddit, totalScore) => 
        val frecuencias = frecMap.getOrElse(subreddit, List())
        s"""
          | Suberddit: $subreddit
          | Score total: $totalScore
          | Palabras:
          |${frecuencias.map{case (palabra, frec) => s" >> Palabra:$palabra, Frecuencia: $frec"}.mkString("\n")}
          """.stripMargin
    }.mkString("\n")
  }
}
