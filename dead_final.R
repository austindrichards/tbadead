

library(rtweet)
library(httpuv)
library(httr)
library(tidyverse)
library(stringr)
library(cronR)
library(shinyFiles)
library(miniUI)




get_token()



#fixing error in package- wasn't letting a tweet post based on error with character length

is_tweet_length <- function(.x, n = 280) {
  .x <- gsub("https?://[[:graph:]]+\\s?", "", .x)
  while (grepl("^@\\S+\\s+", .x)) {
    .x <- sub("^@\\S+\\s+", "", .x)
  }
  !(nchar(.x) <= n)  
}


assignInNamespace("is_tweet_length", is_tweet_length, ns = "rtweet")


get_token()




djt <- get_timeline("realDonaldTrump", n = 1)



djt_clean <- djt %>% 
  filter(is_retweet == FALSE) %>% 
  select(text) %>%
  mutate(text_clean = str_replace_all(text, 'http[^ ]+', '_url_')) %>% 
  filter( !grepl("_url_", text_clean))


already_posted <- get_timeline("TBADead", n = 3200) %>% 
  select(text) 

exclamations <- c("perfectly!|fighting")
nations <- c("Turkey|Syria|kurds|turkey|syria")
other <- c("ISIS|attack")
fake <- c("Fake News|fake news|Fake news|Fake News!|fake news!")
biden <- c("biden|Biden|Hunter Biden|hunter biden|joe biden|Joe Biden")
obama <- c("obama|Obama|Barack Obama|barack obama")
clinton <-c("Hilary Clinton|Hilary|hilary clinton|hilary")
dem_news <-c("CNN|cnn|NBC|nbc")
collude <- c("Collusion|collusion")
nyt <-c("NYT|New York Times|new york times")
fox <-c("Fox News|fox news")
witch <-c("Witch Hunt|witch hunt| Witch hunt|Witch|witch")
rudy <- c("Rudy Giuliani|Giuliani|giuliani")
uke <- c("Ukrainian|ukranian|ukraine|Ukraine")
stupid<- c("stupid|Stupid")
lawyer<- c("lawyer|Lawyer")
congress <-c("congressman|congresswoman|congress|Congressman|Congressmen")
guy <- c("guy|Guy|Man|man")
illegal <- c("illegal immigrant|illegal immigrants|illegals|immigrants")
happy <- c("Happy|happy")
dems <- c("Dems|Democrats|dems|democrats")
schiff <- c("Schiff|schiff|adam schiff|Adam Schiff|Adam schiff")
china <- c("China|china")
camera <- c("camera|Camera")
house <- c("House|house")



dt_tweet <- as.character(sample_n(djt_clean, 1, replace = FALSE) %>% 
                           select(text_clean))


tbas_tweet <- dt_tweet %>% 
  str_replace_all(., exclamations, 'face melting solo') %>%
  str_replace_all(., nations, "Europe '72") %>%
  str_replace_all(., other, 'Jerry Licks') %>% 
  str_replace_all(., fake, 'lot lizards') %>% 
  str_replace_all(., biden, 'Bobby Weir') %>% 
  str_replace_all(., congress, 'Uncle Johns Band') %>% 
  str_replace_all(., obama, 'Lesh') %>% 
  str_replace_all(., clinton, 'Brown Eyed Woman') %>% 
  str_replace_all(., dem_news, 'Box of Rain') %>% 
  str_replace_all(., collude, 'Steal your face') %>% 
  str_replace_all(., nyt, 'Shakedown Street') %>% 
  str_replace_all(., fox, 'Wall of Sound') %>% 
  str_replace_all(., witch, 'Deep Elem Blues') %>% 
  str_replace_all(., rudy, 'RatDog')%>% 
  str_replace_all(., uke, 'the lot') %>% 
  str_replace_all(., stupid, 'scarlet begonias') %>% 
  str_replace_all(., lawyer, 'sugar magnolia') %>% 
  str_replace_all(., guy, 'Deadheads') %>% 
  str_replace_all(., illegal, 'gate crasher') %>% 
  str_replace_all(., happy, 'Sunshine Daydream') %>% 
  str_replace_all(., dems, 'Wooks') %>% 
  str_replace_all(., schiff, 'Wook') %>% 
  str_replace_all(., china, 'Bertha') %>% 
  str_replace_all(., camera, 'Eyes of the World') %>% 
  str_replace_all(., house, 'Brokedown Palace') %>% 
  str_trunc(., 240, "right")



replaced <- str_detect(tbas_tweet, c('face melting solo',"Europe '72",'Jerry Licks', 'lot lizards', 'Bobby', 'Uncle Johns Band', 'Lesh', 'Brown Eyed Woman', 'Box of Rain', 'Steal your face', 'Shakedown Street',  'Wall of Sound', 'Deep Elem Blues', 'RatDog', 'the lot', 'scarlet begonias', 'sugar magnolia', 'Deadhead', 'gate crasher', 'Sunshine Daydream',  'Wooks', 'Wook',  'Bertha', 'Eyes of the World', 'Brokedown Palace'))  


count_replace <- sum(as.numeric(str_count(replaced, "TRUE")))


if (count_replace > 0 && dt_tweet %in% already_posted$text == FALSE)
  post_tweet(status = tbas_tweet)




