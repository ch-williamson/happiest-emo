#### Setup ####

# Load useful packages
# For importing data and polarity calculation:
library(spotifyr)
library(genius)
library(qdap)
# For data manipulation:
library(dplyr)
library(magrittr)
library(tidytext)
library(stringr)
# To help with functions:
library(purrr)
# For plotting:
library(ggplot2)
library(ggrepel)


# Add your own Spotify client ID and secret here - 
# these can be found on your Spotify dev account page
# These allow the other spotifyr functions to work

Sys.setenv(SPOTIFY_CLIENT_ID = "...")
Sys.setenv(SPOTIFY_CLIENT_SECRET = "...")

access_token = get_spotify_access_token()


# Fetch tracks from Spotify's "Emo Forever" playlist
# using the playlist ID (found in Spotify's 'Share' menu)
emo_forever = get_playlist_tracks("37i9dQZF1DX9wa6XirBPv8")

# This fetches a lot of info as a list, so create a tibble with just the 
# artists, track titles and track IDs
# Get the 'name' element of the track.artists list for each track
ef = tibble(artist = unlist(lapply(emo_forever$track.artists, "[", , "name")), 
            track = emo_forever$track.name,
            id = emo_forever$track.id)


#### Musical polarity ####

# Define musical polarity to be sqrt(valence^2 + energy^2)
# which is the distance of the (valence, energy) coordinate from (0, 0)
# Add valence, energy and musical polarity columns to ef
musical_pol = ef %>% 
  mutate(features = get_track_audio_features(id),
         valence = features$valence,
         energy = features$energy,
         musical_polarity = sqrt(valence^2 + energy^2)) %>%
  select(id, artist, track, musical_polarity) 

# Remove id and arrange by descending musical polarity to find top and bottom 5
mp_info = musical_pol %>% 
  select(-id) %>% 
  arrange(desc(musical_polarity))

# Top and bottom 5 for musical positivity:
mp_info %>% 
  head(5)

mp_info %>% 
  tail(5)


#### Lyrical polarity ####

# Inputs to genius_lyrics are artist name and song title rather than Spotify ID, 
# so these need to be correctly formatted

# First mutate to remove brackets,
# then manually fix some that would otherwise break
# Using track IDs to futureproof in case this playlist changes

tidy_ef = ef %>% 
  mutate(track = str_replace(track, "\\(", ""),
         track = str_replace(track, "\\)", ""),
         track = case_when(id == "2TfSHkHiFO4gRztVIkggkE" ~ "Sugar, We're Goin' Down",
                           id == "5sTVykpRs4eiZKn96bZogj" ~ "Feel Good Drag",
                           id == "4OsLDuaH0bWR6xM6nj66F8" ~ "FCPREMIX",
                           TRUE ~ track),
         artist = case_when(id == "3t9OWuyMpQclTWD6ABjqXj" ~ "Aiden Band",
                            id == "3jme2RKgrkBcIs0wbwbc7B" ~ "The Blackout Welsh Band",
                            TRUE ~ artist))


# genius_lyrics occasionally returns NAs and/or halts - define a wrapper function

get_lyrics = function(artist_name, song_name) {
  
  tryCatch({ # To stop things breaking if it can't find something
    lyric_char = ""
    i = 0
    
    while (i < 10 & lyric_char == "") { # Try fetching the lyrics 10 times - otherwise move on
      cat("Trying...\n")
      lyric_df = genius_lyrics(artist = artist_name, song = song_name, info = "simple")
      lyric_char = toString(lyric_df$lyric) # Convert to string for qdap's polarity function
      i = i + 1
    }
    return(lyric_char)
    
  }, error = function(e) {
    cat("Couldn't get lyrics for ", artist_name, " - ", song_name, "\n")
    return(NA)
  })
  
}


# Append lyrics - this takes a while
ef_lyrics = tidy_ef %>%
  rowwise() %>% 
  mutate(lyrics = get_lyrics(artist, track))


# Define a function to return the polarity of the lyrics (using qdap)
ave_polarity = function(char) {
  pol_df = polarity(char) # Returns a list with various info
  pol_val = pol_df$all$polarity # Just take the polarity score from this list
  return(pol_val)
}


# Calculate lyrical polarity
lyrical_pol = ef_lyrics %>%
  mutate(lyrical_polarity = ave_polarity(lyrics)) %>% 
  select(id, artist, track, lyrical_polarity)


# Remove id and arrange by descending lyrical polarity to find top and bottom 5
lp_info = lyrical_pol %>% 
  select(-id) %>% 
  arrange(desc(lyrical_polarity))

# Top and bottom 5 for lyrical positivity:
lp_info %>% 
  head(5)

lp_info %>% 
  tail(5)


#### Combined ####

# Scale musical polarity to be between 0 and 1
# Apply logistic function to lyrical polarity, again to map it to (0, 1)
overall_pol = musical_pol %>% 
  inner_join(lyrical_pol, by = c("id")) %>% 
  mutate(scaled_musical_polarity = musical_polarity/sqrt(2),
         scaled_lyrical_polarity = exp(lyrical_polarity)/(exp(lyrical_polarity) + 1),
         overall_polarity = sqrt(scaled_musical_polarity^2 + scaled_lyrical_polarity^2)) %>% 
  select(id, artist = artist.y, track = track.y,
         scaled_musical_polarity, scaled_lyrical_polarity, overall_polarity)


# Remove id and arrange by descending overall polarity to find top and bottom 5
overall_info = overall_pol %>% 
  select(-id) %>% 
  arrange(desc(overall_polarity))

# Top and bottom 5 for overall positivity:   
overall_info %>% 
  head(5)

overall_info %>% 
  tail(5)


#### Rerunning with an augmented dictionary ####

custom_pol = sentiment_frame(c(positive.words, "alright"), c(negative.words, "down"))

ave_polarity_aug = function(char) {
  pol_df = polarity(char, polarity.frame = custom_pol)
  pol_val = pol_df$all$polarity
  return(pol_val)
}

lyrical_pol_aug = ef_lyrics %>%
  mutate(lyrical_polarity = ave_polarity_aug(lyrics)) %>% 
  select(id, artist, track, lyrical_polarity)

overall_pol_aug = musical_pol %>% 
  left_join(lyrical_pol_aug, by = c("id")) %>% 
  mutate(scaled_musical_polarity = musical_polarity/sqrt(2),
         scaled_lyrical_polarity = exp(lyrical_polarity)/(exp(lyrical_polarity) + 1),
         overall_polarity = sqrt(scaled_musical_polarity^2 + scaled_lyrical_polarity^2)) %>% 
  select(id, artist = artist.y, track = track.y,
         scaled_musical_polarity, scaled_lyrical_polarity, overall_polarity)


# Remove id and arrange by descending overall polarity to find top and bottom 5
overall_info_aug = overall_pol_aug %>% 
  select(-id) %>% 
  arrange(desc(overall_polarity))

# Top and bottom 5 for overall positivity:   
overall_info_aug %>% 
  head(5)

overall_info_aug %>% 
  tail(5)


#### Appendix ####

# Plot to illustrate valence and energy for the Medium article

# Making a tibble out of four song IDs, acquired from the Share menu on Spotify
rep_df = tibble(id = c("0SxFyA4FqmEQqZVuAlg8lf", # The First Time ...
                       "7MJQ9Nfxzh8LPZ9e9u68Fq", # Lose Yourself
                       "2oRQmJitqUIwovf3Ttg4sw", # Hey Ya!
                       "5YbgcwHjQhdT1BYQ4rxWlD")) %>% # Don't Worry ...
  rowwise() %>% 
  mutate(title = get_track(id)$name,
         features = get_track_audio_features(id),
         valence = features$valence,
         energy = features$energy)

ggplot(rep_df, aes(x = valence, y = energy)) +
  annotate("rect", xmin = 0, xmax = 0.5, ymin = 0, ymax = 0.5, fill = "#a8e4ef", alpha = 0.3) + 
  annotate("rect", xmin = 0.5, xmax = 1, ymin = 0, ymax = 0.5, fill = "#79de79", alpha = 0.3) + 
  annotate("rect", xmin = 0, xmax = 0.5, ymin = 0.5, ymax = 1, fill = "#fb6962", alpha = 0.3) + 
  annotate("rect", xmin = 0.5, xmax = 1, ymin = 0.5, ymax = 1, fill = "#fcfc99", alpha = 0.3) +
  geom_point() +
  geom_hline(aes(yintercept = 0.5)) +
  geom_vline(aes(xintercept = 0.5)) +
  geom_text(aes(x = c(0.25, 0.25, 0.75, 0.75),
                y = c(0.8, 0.3, 0.8, 0.3),
                label = c("ANGRY", "SAD", "HAPPY", "CALM")),
            size = 5,
            colour = "#999999") +
  geom_label_repel(aes(label = title)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")) +
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  labs(x = "Valence",
       y = "Energy",
       title = "Examples of songs with extreme valence and energy")


# An alternative polarity metric

# Another option to calculate overall polarity is taking the logit of the
# (scaled) musical polarity, then adding the musical and lyrical polarities
# I found that this gave results that corresponded less well to my own opinion
# of the songs' positivities

overall_pol_alt = musical_pol %>% 
  left_join(lyrical_pol, by = c("id")) %>% 
  mutate(scaled_musical_polarity = musical_polarity/sqrt(2),
         logit_mp = log(scaled_musical_polarity / (1 - scaled_musical_polarity)),
         overall_polarity = logit_mp + lyrical_polarity) %>% 
  select(id, artist = artist.y, track = track.y,
         logit_mp, lyrical_polarity, overall_polarity)

# Remove id and arrange by descending overall polarity to find top and bottom 5
overall_info_alt = overall_pol_alt %>% 
  select(-id) %>% 
  arrange(desc(overall_polarity))

# Top and bottom 5 for overall positivity:
overall_info_alt %>% 
  head(5)

overall_info_alt %>% 
  tail(5)
