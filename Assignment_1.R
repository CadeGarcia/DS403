#install.packages('spotifyr')
#devtools::install_github('charlie86/spotifyr')
#install.packages("conflicted")

#Libraries for sorting table
library(spotifyr)
library(purrr)
library(knitr)
library(conflicted)
library(dplyr)
library(tidyverse)
#Libraries for plotting
library(ggplot2)
library(plotly)

#---

Sys.setenv(SPOTIFY_CLIENT_ID = 'ff9322ca286d4d4b803c0f2dfd9771ca')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '15d4fe86d7234577927c7c32f0beeded')

get_spotify_access_token()

access_token <- get_spotify_access_token()

#---
  
#ALL data on Childish Gambino
#Childish Gambino artist ID = 73sIBHcqh3Z3NyqHKZ7FOL
childish_gambino <- get_artist_audio_features(artist = "73sIBHcqh3Z3NyqHKZ7FOL",
                                              authorization = access_token)

#---

#Filtering the OG data set for Bando Stone
childish_filtered1 <- subset(childish_gambino, 
                             album_name == "Bando Stone and The New World")

keep_cols <- c("album_release_year" , "danceability", "energy", "loudness", 
               "mode", "speechiness", "acousticness", "instrumentalness", 
               "liveness", "valence", "tempo", "time_signature", "track_name", 
               "track_number", "album_name")

filtered_bando <- childish_filtered1[ , keep_cols]

#---
  
#Filtering the OG data set for BTI
childish_filtered2 <- subset(childish_gambino, 
                             album_name == "Because the Internet")

filtered_BTI <- childish_filtered2[ , keep_cols]

final_dataset <- rbind(filtered_BTI, filtered_bando, fill = TRUE)

#---

#write.csv(final_dataset, 'Childish_Gambino_df.csv', row.names = TRUE)

#---

as.data.frame(final_dataset)

#Plot for BTI
plottin <- ggplot(filtered_BTI, aes(energy,valence)) + 
  geom_jitter(aes(fill=track_name)) +
  labs(title = "Because The Internet Vibe") +
  xlim(0,0.85) +
  ylim(0,0.85)

ggplotly(plottin)

# Plot for awaken
plottin_2 <- ggplot(filtered_bando, aes(energy,valence)) + 
  geom_jitter(aes(fill=track_name)) +
  labs(title = "Bando Stone and The New World Vibe") +
  xlim(0,0.85) +
  ylim(0,0.85)

ggplotly(plottin_2)
