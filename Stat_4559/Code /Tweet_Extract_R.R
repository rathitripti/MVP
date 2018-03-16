install.packages("twitteR")
install.packages("ROAuth")
install.packages("jsonlite")
install.packages("rtweet")
install.packages("tidytext")
install.packages("purrr")
install.packages("data.table")
library("twitteR")
library("ROAuth")
library("jsonlite")
library("rtweet")
library("tidytext")
library("purrr")
library("data.table")
##Mining Tweets 
app_name <- "flutracking1"
key <- "nQ8HlR0WO0iWsX5O7OCcifUzB"
secret <- "1cACcfLbrkvZmkPxHT8eVBiRvaCQEwCGoVUYBtlEwelbi2M8B5"

twitter_token <- create_token(app = app_name, consumer_key = key, consumer_secret = secret)

##File Format: keyword_month/day
##Accessing Tweets for 3/2
##fwrite saves in appropriate .csv file
flu_32 <- search_tweets("flu", lang = "en" , retryonratelimit = TRUE, n = 250000, geocode = lookup_coords("usa"), include_rts = FALSE)
flu_32 <- lat_lng(flu_32)
fwrite(flu_32, file = "flu_32.csv")

influ_32 <- search_tweets("influenza", lang = "en" , n = 25000, retryonratelimit = TRUE, geocode = lookup_coords("usa"), include_rts = FALSE)
influ_32 <- lat_lng(influ_32)
fwrite(influ_32, file = "influ32.csv")

feverish_32 <- search_tweets("feverish", lang = "en", n = 25000, retryonratelimit = TRUE, geocode = lookup_coords("usa"), include_rts = FALSE)
feverish_32 <- lat_lng(feverish_32)
fwrite(feverish_32, file = "feverish32.csv")

fever_32 <- search_tweets("fever", lang = "en", n = 50000, retryonratelimit = TRUE, geocode = lookup_coords("usa"), include_rts = FALSE)
fever_32 <- lat_lng(fever_32)
fwrite(fever_32, file = "fever32.csv")

flu_hashtag_32 <- search_tweets("#flu", lang = "en", n = 50000, retryonratelimit = TRUE, geocode = lookup_coords("usa"), include_rts = FALSE)
flu_hastag_32 <- lat_lng(flu_hashtag_32)
fwrite(flu_hashtag_32, file = "flu_hashtag32.csv")

aches_32 <- search_tweets("body aches OR bodyaches OR body ache OR bodyache", lang = "en", n = 25000, retryonratelimit = TRUE, geocode = lookup_coords("usa"), include_rts = FALSE)
aches_32 <- lat_lng(aches_32)
fwrite(aches_32, file = "aches32.csv")

sore_32 <- search_tweets("sore throat OR sorethroat", lang = "en", n= 25000, retryonratelimit = TRUE, geocode = lookup_coords("usa"), include_rts = FALSE)
sore_32 <- lat_lng(sore_32)
fwrite(sore_32, file = "sore32.csv")

pneumonia_32 <- search_tweets("pneumonia", lang = "en", n= 25000, retryonratelimit = TRUE, geocode = lookup_coords("usa"), include_rts = FALSE)
pneumonia_32 <- lat_lng(pneumonia_32)
fwrite(pneumonia_32, file = "pneumonia32.csv")

tamiflu_32 <- search_tweets("tamiflu", lang = "en", n= 25000, retryonratelimit = TRUE, geocode = lookup_coords("usa"), include_rts = FALSE)
tamiflu_32 <- lat_lng(tamiflu_32)
fwrite(tamiflu_32, file = "tamiflu32.csv")

flu_shot_32 <- search_tweets("flu shot", lang = "en", n= 36000, retryonratelimit = TRUE, geocode = lookup_coords("usa"), include_rts = FALSE)
flu_shot_32 <- lat_lng(flu_shot_32)
fwrite(flu_shot_32, file = "flu_shot32.csv")

flu_vaccine_32 <- search_tweets("flu vaccine", lang = "en", n = 36000, retryonratelimit = TRUE, geocode = lookup_coords("usa"), include_rts = FALSE)
flu_vaccine_32 <- lat_lng(flu_vaccine_32)
fwrite(flu_vaccine_32, file = "flu_vaccine32.csv")

headaches_32 <- search_tweets("headaches OR head aches OR headache OR head ache", n = 100000, retryonratelimit = TRUE, geocode = lookup_coords("usa"),include_rts = FALSE)
headaches_32 <- lat_lng(headaches_32)
fwrite(headaches_32, file = "headaches32.csv")

cough_32 <- search_tweets("cough", lang = "en", n= 100000, retryonratelimit = TRUE, geocode = lookup_coords("usa"), include_rts = FALSE)
cough_32 <- lat_lng(cough_32)
fwrite(cough_32, file = "cough32.csv")

fatigue_32 <- search_tweets("fatigue OR fatigued", lang = "en", n= 25000, retryonratelimit = TRUE, geocode = lookup_coords("usa"), include_rts = FALSE)
fatigue_32 <- lat_lng(fatigue_32)
fwrite(fatigue_32, file = "fatigue32.csv")

