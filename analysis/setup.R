

library(tidyverse)


source("get_data.R")

# https://github.com/markkohdev/spotify-api-starter
# https://www.youtube.com/watch?v=goUzHd7cTuA&t=1626s

# https://spotify-audio-analysis.glitch.me/analysis.html

# tatums > segments > beats > bars > sections

# sections: drastic changes in the song. Chorus. Verse. Solo. Might not be the best of measures.

# Segments: Set of sound entities (typically under a section), each relatively uniform in timbre and harmony. 
# Characterizes by  their perceptual onsets  and duration in seconds, loudness, pitch and timbral content. 
# Chunk of time that has the same general sound. 
  #   Pitches is an array of 12 values, each corresponding to the scale on a piano (white and black keys).
  #   If value close to one in index 0 (first position),  C was dominant in that segment. 


# Timbre: 
# if you play C on a piano, and C on a saxophone. Could you determine which one is which?
# It is the same pithc, but they sound different. It has to do with the quality of the sound
# from the different instruments. It is hard to represent this in data. Humans can easily observe it. 
# It is represented in a vector of 12 unbounded values, centered around 0. 
  # The first dimension represents the average  loudness of the segment
  # the second analyses the brightness
  # Third is closely correlated to the flatness of the sound
  # Fourth is correlated to sounds with strong attack
# Best use is in comparison with each other. Euclidean distance. 
# Timbers is the result of a PCA analysis of specograms. 
# The spectogram is reduced to 12 dimensions. 

bars %>% 
  filter(id == "2h5CXy8Pme1lTY3CUAEI1i") %>%
  ggplot(aes(start, confidence)) + 
  geom_point()

bars %>% 
  #filter(id == "2h5CXy8Pme1lTY3CUAEI1i") %>%
  left_join(
    album %>% 
      select(id, name), 
    "id"
  ) %>% 
  ggplot(aes(start, duration)) + 
  geom_point() +
  facet_wrap(~name)


sections %>% 
  #filter(id == "2h5CXy8Pme1lTY3CUAEI1i") %>%
  left_join(
    album %>% 
      select(id, name), 
    "id"
  ) %>% 
  ggplot(aes(start, time_signature)) +
  geom_point() +
  facet_wrap(~name) +
  scale_x_continuous(
    breaks = c(seq(0, 780, 60))
  )
