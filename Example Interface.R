student_responses <- data.frame(
  responses = c("I like housing",
                "I dislike housing",
                "I like my teachers",
                "I dislike my teachers",
                "I dislike housing")
)

categories_sentiment_code <- data.frame(
  category = as.factor(c("housing", "housing", "teachers", "teachers", "housing")),
  sentiment = as.factor(c("positive", "negative", "positive", "negative", "negative")),
  coded_response = as.factor(c("positive sentiment toward housing",
                               "negative sentiment toward housing",
                               "positive sentiment toward teachers",
                               "negative sentiment toward teachers",
                               "negative sentiment toward housing"))
)

coded_responses <- cbind(student_responses, categories_sentiment_code)

code_responses(student_responses)











