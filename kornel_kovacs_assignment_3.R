library(tidyverse)
library(lubridate)
library(readxl)
library(tidytext)
library(ggplot2)
library(glmnet)
library(scales)

Sys.setenv(TZ='Europe/Budapest')
df <- read_excel("tweets_formatted.xlsx")

df$created_at <- parse_date_time(df$created_at, c("ymd_HMS"))
df$is_customer <- as.factor(df$is_customer)
df %>% 
  filter(created_at < ymd("2020-03-09")) %>% 
  nrow()
# This is a very insignificant amount of tweet. 
# They must be from conversations that were revoked at this time.
# Not really relevant to our case.

df <- df %>% 
  filter(created_at >= ymd("2020-03-09"))

# Distribution by the day of year - wizzair
number_of_tweets_per_day <- df %>% 
  select(created_at, is_customer) %>%
  rename(Type = is_customer) %>% 
  ggplot(aes(x = date(created_at), fill = Type)) + 
    geom_histogram(binwidth = 1, alpha = 0.9, colour = "white") +
    ggtitle("Daily number of tweets over time") + 
    facet_wrap(~Type) + 
    ylab("Number of tweets") +
    xlab("Date") +
    scale_x_date(labels = date_format("%Y-%m-%d")) +
    geom_vline(xintercept = ymd("2020-03-16"), color = "black", size= 0.7)

ggsave("number_of_tweets_per_day.png", plot = number_of_tweets_per_day, dpi = 320, height = 7, width = 15)

word_counter <- function(sentence) {
  # counts the number of words in a sentence
  # well..precisely.. in a charactervector
  words <- strsplit(sentence, " ")
  return(sapply(words, length))
}
df$word_count <- unlist(lapply(df$full_text, word_counter))

# Distribution of words
tweet_length_distr <- df %>% 
  select(word_count, is_customer) %>% 
  ggplot(aes(x = is_customer, y = word_count, fill = is_customer)) +
  geom_violin()

ggsave("tweet_length_distr.png", plot = tweet_length_distr, dpi = 320, height = 7, width = 15)

# Daily median length of words
daily_median_word <- df %>% 
  select(word_count, is_customer, created_at) %>% 
  group_by(which_day = floor_date(created_at, "day"), is_customer) %>% 
  summarise(avg_word_count = median(word_count)) %>% 
  ungroup() %>% 
  rename(Type = is_customer) %>% 
  ggplot(aes(x = which_day, y = avg_word_count, fill = Type)) + 
    geom_col() +
    ggtitle("Median length of tweets (in words)") + 
    xlab("Date") + 
    ylab("Number of words") +
    facet_wrap(~Type)

ggsave("daily_median_word.png", plot = daily_median_word, dpi = 320, height = 7, width = 15)

words <- df %>%
  unnest_tokens(word, full_text) %>% 
  select(word, is_customer, id_str)

data("stop_words")
words_to_ignore <- tibble(word = c("https", "amp", "t.co"))

remove_non_english <- function(word) {
  return(gsub('[^a-zA-Z|[:blank:]]', "", word))
}

# words$word <- stringi::stri_trans_general(words$word, "latin-ascii")
words$word <- unlist(lapply(words$word, remove_non_english))

words <- words %>%
  anti_join(stop_words, by = "word") %>%
  anti_join(words_to_ignore, by = "word") %>% 
  filter(! str_detect(word, "\\d")) %>% 
  filter(word != "")

word_freq <- words %>% 
  count(word, sort = TRUE, is_customer) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  rename(Type = is_customer) %>% 
  ggplot(aes(x = word, y = n, fill = Type)) + 
    geom_col() + 
    xlab("Count") +
    facet_wrap(~Type, ncol = 2, scales = "free") +
    coord_flip() + 
    ggtitle("Most frequent words tweeted by Wizzair")

ggsave("word_freq.png", plot = word_freq, dpi = 320, height = 7, width = 15)

tweets <- words %>%
  group_by(is_customer, id_str, word) %>%
  summarise(contains = 1) %>%
  ungroup() %>%
  spread(key = word, value = contains, fill = 0) %>%
  mutate(tweet_by_wizzair = as.integer(is_customer == "wizzair")) %>%
  select(-is_customer, -id_str)

set.seed(42)
fit <- cv.glmnet(
  x = tweets %>% select(-tweet_by_wizzair) %>% as.matrix(),
  y = tweets$tweet_by_wizzair,
  family = "binomial",
  trace.it = TRUE,
  nfolds = 4,
  parallel = 1
)

temp <- coef(fit, s = exp(-3)) %>% as.matrix()
coefficients <- tibble(word = row.names(temp), beta = temp[, 1])
impactful_words <- coefficients %>%
  filter(beta != 0) %>%
  filter(word != "(Intercept)") %>%
  arrange(desc(beta)) %>%
  mutate(index = row_number())

regression_result <- ggplot(impactful_words, aes(x = index, y = beta, fill = ifelse(beta > 0, "Wizzair", "Customer"))) +
  geom_bar(stat = "identity", alpha = 0.75) +
  scale_x_continuous(breaks = data$i, labels = data$word, minor_breaks = NULL) +
  xlab("") +
  ylab("Coefficient Estimate") +
  coord_flip() +
  scale_fill_manual(
    guide = guide_legend(title = "Word typically used by:"),
    values = c("#446093", "#bc3939")
  ) +
  theme_bw() +
  theme(legend.position = "top")

ggsave("regression_result.png", plot = regression_result, dpi = 320, height = 7, width = 15)

# bind_tf_idf approach for word "importances"
tf_idf <- df %>%
  unnest_tokens(word, full_text) %>%
  group_by(is_customer, word) %>% 
  summarise(n = n()) %>% 
  bind_tf_idf(word, is_customer, n) %>%
  mutate(word = fct_reorder(word, tf_idf)) %>% 
  group_by(is_customer) %>% 
  top_n(15, tf_idf) %>% 
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = is_customer)) +
    geom_col(show.legend = FALSE) +
    labs(x = NULL, y = "tf-idf") +
    facet_wrap(~is_customer, ncol = 2, scales = "free") +
    coord_flip()

ggsave("tf_idf.png", plot = tf_idf, dpi = 320, height = 7, width = 15)

# N-grams
# Bigrams were not very meaningful

#with tf-idf
trigram_tf_idf <- df %>%
  unnest_tokens(trigram, full_text, token = "ngrams", n = 3) %>% 
  group_by(is_customer, trigram) %>% 
  summarise(n = n()) %>% 
  bind_tf_idf(trigram, is_customer, n) %>%
  mutate(trigram = fct_reorder(trigram, tf_idf)) %>% 
  group_by(is_customer) %>% 
  top_n(15, tf_idf) %>% 
  ungroup() %>%
  mutate(trigram = reorder(trigram, tf_idf)) %>%
  ggplot(aes(trigram, tf_idf, fill = is_customer)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~is_customer, ncol = 2, scales = "free") +
  coord_flip()
ggsave("trigram_tf_idf.png", plot = trigram_tf_idf, dpi = 320, height = 7, width = 15)

## Zip's law
powerlaw_data <- df %>% 
  unnest_tokens(word, full_text) %>%
  select(word, is_customer) %>%
  filter(is_customer == "wizzair") %>% 
  count(word, sort = TRUE) %>% 
  mutate(total = sum(n)) %>% 
  mutate(rank = row_number(), 
         term_frequency = n/total)
  
powerlaw_reg <- lm(formula = log10(term_frequency) ~ log10(rank), data = powerlaw_data)
summary(powerlaw_reg)

zipf_plot <- ggplot(powerlaw_data, aes(rank, term_frequency)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) +
  scale_x_log10() +
  ylab("Term frequency") + 
  xlab("Rank") +
  ggtitle("Zip's law") +
  scale_y_log10(labels = scales::comma) +
  geom_abline(intercept = 0.722461, slope = -1.701950, color = "gray50", linetype = 2)
ggsave("zipf_plot.png", plot = zipf_plot, dpi = 320, height = 7, width = 15)

# Sentiment analysis part

bing <- get_sentiments("bing")
nrc <- get_sentiments("nrc")

# Bing part

# Number of positive/negative words by category
df %>% 
  unnest_tokens(word, full_text) %>% 
  inner_join(bing) %>% 
  select(sentiment, word, is_customer) %>% 
  group_by(is_customer, sentiment) %>% 
  summarise(n = n())

# Number of positive / negative words used per day
bing_plot <- df %>% 
  unnest_tokens(word, full_text) %>% 
  inner_join(bing) %>% 
  select(sentiment, word, is_customer, created_at) %>%
  group_by(date(created_at), is_customer, sentiment) %>% 
  rename(tweet_date = `date(created_at)`) %>% 
  filter(tweet_date > ymd("2020-03-11")) %>% 
  summarise(n = n()) %>% 
  rename(Type = is_customer) %>% 
  ggplot(aes(fill=sentiment, y=n, x=tweet_date)) + 
    geom_bar(position="fill", stat="identity") +
    geom_vline(xintercept = ymd("2020-03-16"), color = "black", size= 0.7) +
    facet_wrap(~Type) + 
    ggtitle("Proportion of positive/negative words used by Wizz and their customers over time") + 
    scale_x_date(labels = date_format("%Y-%m-%d")) +
    ylab("Proportion") + 
    xlab("Date")
ggsave("bing_plot.png", plot = bing_plot, dpi = 320, height = 7, width = 15)

# NRC part
nrc_plot <- df %>% 
  unnest_tokens(word, full_text) %>% 
  inner_join(nrc) %>% 
  select(sentiment, word, is_customer) %>% 
  group_by(is_customer, sentiment) %>% 
  summarise(n = n()) %>% 
  rename(Type = is_customer) %>% 
  ggplot(aes(x = sentiment, y = n, fill = Type)) +
    geom_bar(position = "dodge", stat = "identity") +
    ylab("Count") +
    xlab("Sentiment")
ggsave("nrc_plot.png", plot = nrc_plot, dpi = 320, height = 7, width = 15)


nrc_overtime <- df %>% 
  unnest_tokens(word, full_text) %>% 
  inner_join(nrc) %>% 
  select(sentiment, word, is_customer, created_at) %>%
  group_by(date(created_at), is_customer, sentiment) %>% 
  rename(tweet_date = `date(created_at)`) %>% 
  filter(tweet_date > ymd("2020-03-11")) %>% 
  summarise(n = n()) %>% 
  rename(Type = is_customer) %>% 
  ggplot(aes(fill=Type, y=n, x=tweet_date)) + 
  geom_bar(position="fill", stat="identity") +
  geom_vline(xintercept = ymd("2020-03-16"), color = "black", size= 0.7) +
  facet_wrap(~sentiment, ncol = 2) + 
  ggtitle("Proportion of NRC sentiments used by Wizz and their customers over time") + 
  scale_x_date(labels = date_format("%Y-%m-%d")) +
  ylab("Proportion") + 
  xlab("Date")
ggsave("nrc_overtime.png", plot = nrc_overtime, dpi = 320, height = 7, width = 15)
