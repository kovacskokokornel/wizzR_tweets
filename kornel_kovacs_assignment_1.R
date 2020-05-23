library(openssl)
library(httpuv)
library(twitteR)
library(tidyverse)

setup_twitter_oauth(
  consumer_key = Sys.getenv("twitterAPIkey"),
  consumer_secret = Sys.getenv("twitterAPIsecretkey"),
  access_token = Sys.getenv("twitterAPIaccesstoken"),
  access_secret = Sys.getenv("twitterAPIaccesstokensecret")
)

wizzair <- userTimeline('wizzair', n = 3000)
df <- twListToDF(wizzair)

# Exported this df
# Go to Python


