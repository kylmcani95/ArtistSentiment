library(tidytext)
library(tidyverse)

songs=read.csv("songdata.csv")
songs$text=as.character(songs$text)
songs$song=as.character(songs$song)
songs$artist=as.character(songs$artist)
song_text <- songs %>%
  select(lyrics = text, song, artist)
song_text <- filter(song_text, is.element(artist,c("Katy Perry","Taylor Swift")))
word_df <- song_text %>%
  unnest_tokens(word, lyrics)
word_df_nostop <- word_df %>% anti_join(stop_words)
## Joining, by = "word"
word_count <- word_df_nostop %>%
  count(artist, word,sort = TRUE) %>% group_by(artist)


library(wordcloud)
filter(word_df_nostop,artist=="Katy Perry") %>%
 count(word) %>%
 with(wordcloud(word, n, max.words = 100))

word_sentiment <- word_df_nostop %>%
  inner_join(get_sentiments())
head(word_sentiment)
## Joining, by = "word"
sentiment_df <- word_sentiment %>%
  group_by(song,artist) %>%
  summarise(sentiment = sum(value), unique_words = n())

sentiment_df %>% arrange(desc(sentiment)) %>% filter(abs(sentiment)>10) %>%
 ggplot(aes(reorder(song, -sentiment), sentiment, color = artist)) +
 geom_col() + xlab("") +
 coord_flip()+ theme(text = element_text(size=7))
#FIND THE COUNT OF WORD PER SONG
love_count <- subset(word_count, word == "love", select = c("artist", "word", "n"))
songs$count = 1
num_songs=aggregate(count~artist,
                    data=songs,
                    FUN=sum)
love_count <- merge(num_songs, love_count, by="artist")
love_count <- transform(love_count, avg = n/count)
print(c(love_count$artist, love_count$avg)[love_count$avg == max(love_count$avg)])
## [1] "Soundtracks" "11"
print(c(love_count$artist, love_count$avg)[love_count$avg == min(love_count$avg)])
## [1] "Death" "0.0166666666666667"
#
#
#Doing the average times love is said per song:
# Artist: Soundtracks have the most with 11 times per song
# Artist: Death has the least with .02 times per song.
#
#
#Find the sentiment for each band
sentiment_count <- aggregate(sentiment~artist,
                             data=sentiment_df,
                             FUN=sum)
print(c(sentiment_count$artist, sentiment_count$sentiment)[sentiment_count$sentiment == max(sentiment_count$sentiment)])
## [1] "Hillsong" "3898"
print(c(sentiment_count$artist, sentiment_count$sentiment)[sentiment_count$sentiment == min(sentiment_count$sentiment)])
## [1] "Insane Clown Posse" "-8819"
#
#
#Hillsong has the most sentiment in there songs
#Insane Clown Posse has the least sentiment
#
#
2
#Find best vocabulary
vocabulary_df <- word_sentiment %>%
  group_by(artist) %>%
  summarise(sentiment = sum( value), unique_words = n())
print(c(vocabulary_df$artist, vocabulary_df$unique_words)[vocabulary_df$unique_words == max(vocabulary_df$unique_words)])
## [1] "Insane Clown Posse" "4678"
print(c(vocabulary_df$artist, vocabulary_df$unique_words)[vocabulary_df$unique_words == min(vocabulary_df$unique_words)])
## [1] "Iwan Fals" "3"
#Combining the lyrics of all songs:
#Insane Clown Posse has the most with 4678 unique words
#Iwan Fals has the lowest with 3 unique words
set.seed(123)
samp <- sample(nrow(song_taylor_katy), 0.6 * nrow(song_taylor_katy))
song_text_katy <- song_taylor_katy[samp, ]
test <- song_taylor_katy[-samp, ]
word_df_katy <- song_text_katy %>%
  unnest_tokens(word, lyrics)
word_df_nostop_katy <- word_df_katy %>% anti_join(stop_words)
## Joining, by = "word"
word_count_katy <- word_df_nostop_katy %>%
  count(artist, word,sort = TRUE) %>% group_by(artist)
word_sentiment_katy <- word_df_nostop_katy %>%
  inner_join(get_sentiments())
## Joining, by = "word"
sentiment_df_katy <- word_sentiment_katy %>%
  group_by(song,artist) %>%
  summarise(sentiment = sum(value), unique_words = n())
sentiment_count_katy <- aggregate(sentiment~artist,
                                  data=sentiment_df_katy,
                                  FUN=sum)
sentiment_avg_katy <- aggregate(sentiment~artist,
                                data=sentiment_df_katy,
                                FUN=mean)
vocabulary_df_katy <- word_sentiment_katy %>%
  group_by(artist) %>%
  summarise(sentiment = sum(value), unique_words = n())
3
word_song_katy <- word_df_nostop_katy %>%
  count(artist, word,song,sort = TRUE) %>% group_by(artist)
word_song_katy <- subset(word_song_katy, n >=5)
word_song_katy <- transform(word_song_katy, artist = as.numeric(as.factor(artist)))
word_song_katy$artist = word_song_katy$artist - 1
final_katy <- transform(sentiment_df_katy, artist = as.numeric(as.factor(artist)))
final_katy$artist = final_katy$artist - 1
model <- glm(artist~sentiment + unique_words,
             data = final_katy,
             family = "binomial")
final_katy$prediction = predict(object = model,
                                newdata = final_katy,
                                n.trees = 15000,
                                type = "response")
final_katy$prediction=round(final_katy$prediction)
#I do not think you can predict whether a song is Katy Perry's or Taylor Swift's with just lyrics and have good accuracy.
#I could not come up with 5 features from the data set that allowed me to predict.
#Counting words from each song was trivial since some words are repeated a lot in one artists song but will not be repeated in another song
#by the same artists giving false negatives.
