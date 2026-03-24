import Types.Subscription

object Main {

  def main(args: Array[String]): Unit = {

    val subsList: Option[List[Subscription]] = Formatters.getSubscriptions("subscriptions.json")
    
    subsList match{

      case Some(value) =>
        val allPosts = Formatters.getPosts(value)

        allPosts match {

          case Some(list) => 
            val filterPosts = Formatters.filterPosts(list)
            val relevantWords = Formatters.countFrecuency(filterPosts)
            println(subsList.mkString("\n"))
            println(allPosts.mkString("\n"))
            println(filterPosts.mkString("\n"))
            println(relevantWords.take(25).mkString)
            
          case None => 
            print("Error")
        }
      case None => 
        print("Error")
    }
  }
}
