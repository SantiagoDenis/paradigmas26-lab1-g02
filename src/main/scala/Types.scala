
object Types {
    type Subscription = (String, String) // Subreddit, URL
    type Post = (String, String, String, String, Int) // Subreddit, Title, Text, FormattedDate, Score
}
