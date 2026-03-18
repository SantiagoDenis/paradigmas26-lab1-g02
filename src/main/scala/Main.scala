import Types.Subscription

object Main {

  def main(args: Array[String]): Unit = {

    val subsList: List[Subscription] = Formatters.getSubscriptions("subscriptions.json")

    println(subsList.mkString("\n"))
  }

}
