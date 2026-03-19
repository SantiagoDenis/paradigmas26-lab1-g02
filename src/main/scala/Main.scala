import Types.Subscription

object Main {

  def main(args: Array[String]): Unit = {

    val subsList: List[Subscription] = Formatters.getSubscriptions("subscriptions.json")
    val allPosts = Formatters.getPosts(subsList)
    println(subsList.mkString("\n"))
    println(allPosts.mkString("\n"))
  }

}
