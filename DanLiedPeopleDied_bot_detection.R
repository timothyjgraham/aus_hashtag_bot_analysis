# DanLiedPeopleDied hashtag analysis 
# 26/11/2020 1:05pm
# We collect 5000 latest tweets and run a state-of-the-art bot detection model:
# https://github.com/mkearney/tweetbotornot2

# libraries required 
require(tweetbotornot2)
require(glue)
require(dplyr)
require(rtweet)

# API credentials go here 
api_key <- "YOUR KEY HERE"
api_secret_key <- "YOUR KEY HERE"
access_token <- "YOUR KEY HERE"
access_token_secret <- "YOUR KEY HERE"

token <- create_token(
  app = "YOUR APP NAME HERE",
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_token_secret)

######################################################
#### COLLECTION - tweets containing #danliedpeopledied
tweet_search_danliedpeopledied <- search_tweets('#danliedpeopledied', n = 5000, include_rts = TRUE, retryonratelimit = TRUE)
saveRDS(tweet_search_danliedpeopledied, paste0(Sys.time()," tweet_search_danliedpeopledied.rds"))

# SAVE TO DISK
library(dplyr)
df_combined_danliedpeopledied <- tweet_search_danliedpeopledied %>% distinct(status_id, .keep_all = TRUE)
dim(df_combined_danliedpeopledied)
# subset only the columns we want to save to disk 
df_combined_danliedpeopledied_TO_DISK <- df_combined_danliedpeopledied[,c(1:6,14:16,48:62,63:66,82:83)]
write.csv(df_combined_danliedpeopledied_TO_DISK,paste0(Sys.time()," tweet_search_danliedpeopledied.csv"),row.names = F)
# write tweet IDs to disk
write.table(df_combined_danliedpeopledied$status_id,paste0(Sys.time(),"_danliedpeopledied_tweet_ids.csv"), row.names = F, col.names = F, sep=",")

# run sentiment analysis 
library(vader)
tweet_search_danliedpeopledied$score_vader <- "Error"
for (i in 1:length(tweet_search_danliedpeopledied$text)){
  Loop_Error <- F
  tryCatch({ 
    get_vader(tweet_search_danliedpeopledied$text[i]) %>%
      as.numeric(unlist(.)) %>%
      .[length(.)-4] ->tweet_search_danliedpeopledied$score_vader[i]
  }, error = function(e){
    Loop_Error <<- T})
  if (Loop_Error){
    tweet_search_danliedpeopledied$score_vader[i] <- "Error"
  }
}

tweet_search_danliedpeopledied_SENTIMENT_SCORES_VADER <- data.frame(
  compound_sentiment_score = tweet_search_danliedpeopledied$score_vader,
  status_id = tweet_search_danliedpeopledied$status_id,
  stringsAsFactors = F
)

# write to disk
library(openxlsx)
write.xlsx(tweet_search_danliedpeopledied_SENTIMENT_SCORES_VADER,paste0(Sys.time(),"_tweet_search_danliedpeopledied_SENTIMENT_SCORES_VADER.xlsx"))


# BOT ANALYSIS
userids_danliedpeopledied2 <- unique(df_combined_danliedpeopledied$user_id)
# collect timeline data (latest 500 tweets), to feed into the bot prediction model
# we have to use a custom function to avoid a curl error with rate limits
# from here: https://github.com/ropensci/rtweet/issues/266#issuecomment-471092678 
get_timeline_unlimited <- function(users, n){
  
  if (length(users) ==0){
    return(NULL)
  }
  
  rl <- rate_limit(query = "get_timeline")
  
  if (length(users) <= rl$remaining){
    print(glue("Getting data for {length(users)} users"))
    tweets <- get_timeline(users, n, check = FALSE)  
  }else{
    
    if (rl$remaining > 0){
      users_first <- users[1:rl$remaining]
      users_rest <- users[-(1:rl$remaining)]
      print(glue("Getting data for {length(users_first)} users"))
      tweets_first <- get_timeline(users_first, n, check = FALSE)
      rl <- rate_limit(query = "get_timeline")
    }else{
      tweets_first <- NULL
      users_rest <- users
    }
    wait <- rl$reset + 0.1
    print(glue("Waiting for {round(wait,2)} minutes"))
    Sys.sleep(wait * 60)
    
    tweets_rest <- get_timeline_unlimited(users_rest, n)  
    tweets <- bind_rows(tweets_first, tweets_rest)
  }
  return(tweets)
}

# collect user timeline data, which will be inputted to the model 
danliedpeopledied_user_timelines2 <- get_timeline_unlimited(userids_danliedpeopledied2,n=200)
# save it to disk 
saveRDS(danliedpeopledied_user_timelines2,paste0(Sys.time(),"_danliedpeopledied_user_timelines.rds")) # save data to disk 

# run tweetbotornot2 predictions 
bot_results_danliedpeopledied2 <- predict_bot(danliedpeopledied_user_timelines2)

# which accounts have a greater than 0.5 probability of being a bot? 
bot_results_danliedpeopledied2$screen_name[which(bot_results_danliedpeopledied2$prob_bot > 0.5)]
# write results to disk 
write.csv(bot_results_danliedpeopledied2,"bot_results_danliedpeopledied2.csv",row.names = F)