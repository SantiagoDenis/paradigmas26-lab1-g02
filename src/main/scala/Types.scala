
object Types {
    type Subscription = (String, String) // Subreddit, URL
    type Post = (String, String, String, String, Int, String) // Subreddit, Title, Text, FormattedDate, Score, Url del post
}
