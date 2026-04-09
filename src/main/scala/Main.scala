import Types.Subscription

object Main {

  def main(args: Array[String]): Unit = {

    val subsList: Option[List[Subscription]] = Formatters.getSubscriptions("subscriptions.json")
    
    subsList match{

      case Some(value) => {
          val allPosts = Formatters.getPosts(value)
          val filterPosts = Formatters.filterPosts(allPosts)
          val relevantWords = Formatters.countFrecuency(filterPosts)
          println(allPosts.mkString("\n"))
          println(filterPosts.mkString("\n"))
          println(relevantWords.mkString)
          println(" \n ================================================================= \n")
          println(Formatters.getStats(allPosts))
        }
            
      case None => 
        print("Error")
    }
  }
}
