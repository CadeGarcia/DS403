#--------# Digital Humanities Project #--------#
#-- Cade Garcia
#- Oct 31, 2024 

# # # # Installing & Loading Libraries # # # #
#> Using Spotify R package to retrieve data
#install.packages('spotifyr')
#devtools::install_github('charlie86/spotifyr')

#> Libraries for sorting table
library(spotifyr)
library(purrr)
library(knitr)
library(conflicted)
library(dplyr)
library(tidyverse)

#> Libraries for plotting
library(ggplot2)
library(plotly)

#> Attatch spotify developer account using Tokens
Sys.setenv(SPOTIFY_CLIENT_ID = '---')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '---')

get_spotify_access_token()

access_token <- get_spotify_access_token()
  
# # # # ALL data on Childish Gambino in the Spotify R package # # # #
#> Childish Gambino artist ID is: 73sIBHcqh3Z3NyqHKZ7FOL
childish_gambino <- get_artist_audio_features(artist = "73sIBHcqh3Z3NyqHKZ7FOL",
                                              authorization = access_token)

# # # # Filtering the original data set for Bando Stone # # # #
#> Subsetting for Bando Stone and The New World
childish_filtered1 <- subset(childish_gambino, 
                             album_name == "Bando Stone and The New World")

keep_cols <- c("album_release_year" , "danceability", "energy", "loudness", 
               "mode", "speechiness", "acousticness", "instrumentalness", 
               "liveness", "valence", "tempo", "time_signature", "track_name", 
               "track_number", "album_name")

#> Making a new dataset with the desired columns above ^
filtered_bando <- childish_filtered1[ , keep_cols]

  
# # # # Filtering the OG data set for BTI # # # #
#> same concept as above but tailored for Because The Internet
childish_filtered2 <- subset(childish_gambino, 
                             album_name == "Because the Internet")

filtered_BTI <- childish_filtered2[ , keep_cols]

final_dataset <- rbind(filtered_BTI, filtered_bando, fill = TRUE)

# # # # Plotting Energy and Valence # # # #
#> Set the dataset as a data frame so its readable
as.data.frame(final_dataset)

#> Plot for BTI
plottin <- ggplot(filtered_BTI, aes(energy,valence)) + 
  geom_jitter(aes(fill=track_name)) +
  labs(title = "Because The Internet Vibe") +
  xlim(0,0.85) +
  ylim(0,0.85)

ggplotly(plottin) # interactive plot

#> Plot for Bando Stone
plottin_2 <- ggplot(filtered_bando, aes(energy,valence)) + 
  geom_jitter(aes(fill=track_name)) +
  labs(title = "Bando Stone and The New World Vibe") +
  xlim(0,0.85) +
  ylim(0,0.85)

ggplotly(plottin_2)