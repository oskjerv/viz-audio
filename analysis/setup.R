

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

pitches <- 
  segments %>%
  mutate(
    pitches = gsub("\\[|\\]", "", pitches),
    loudness_max = 1- abs(loudness_max)/max(abs(loudness_max))
  ) %>% 
  #filter(loudness_max != 0) %>% 
  separate(pitches, into = paste0("pitches", seq(1, 12)), sep = ",") %>% 
  mutate_at(
    vars(starts_with("pitches")), as.numeric
  ) %>% 
  select(id, start, duration, loudness_max, confidence, starts_with("pitches")) %>% 
  pivot_longer(
    -c(id,start, duration, loudness_max, confidence),
    names_to = "pitches",
    values_to = "values"
  ) %>% 
  mutate(
    pitches = as.numeric(str_remove(pitches, "pitches")),
    end = start + duration
  ) %>% 
  left_join(
    album %>%
      mutate(
        song_duration = duration_ms/1000,
        name = fct_reorder(name, track_number)
        ) %>% 
      select(id, name, song_duration),
    "id"
  ) %>% 
  relocate(end, .after = "start")
  

mode <- 
  sections %>% 
  transmute(
    mode_start = start, 
    mode_end = mode_start + duration, 
    mode_duration = duration, 
    mode, 
    mode_id = id
    )


by <- join_by(id == mode_id, closest(end <= mode_end))

pitches_and_modes <- 
  left_join(
    pitches, 
    mode, 
    by
)


pitches_and_modes %>% 
  #filter(id == "2h5CXy8Pme1lTY3CUAEI1i") %>% 
  #ggplot(aes(as.factor(start), as.factor(pitches), fill = values, alpha = loudness_max)) + 
  #geom_tile() +
  ggplot(aes(ymin = pitches, ymax = pitches + 1 )) + 
  
  #geom_point(aes(mode_start, pitches, color = mode)) +
  #geom_rect(aes(xmin = mode_start, xmax = mode_start + 10, color = as.factor(mode)), alpha = .5, fill = "transparent", linewidth = .1) +
  geom_rect(aes(xmin = start, xmax = end, fill = values, alpha = loudness_max)) +
  
  #geom_segment(mode, aes(xmin = start, xmax = start + duration, ymin = 1, ymax = 12), alpha = .2) +
  # scale_color_manual(
  #   values = c("white", "#1F2C3C")
  # ) +
  scale_fill_gradient2(
    
    min = "white",
    #mid = "#1F2C3C",
    high = "#CA303A"
  ) +
  theme_void() +
  theme(
    legend.position = "none",
    #strip.text = element_blank(),
    panel.spacing = unit(0, "cm")
  ) +
  facet_wrap(
    ~name,
    #scales = "free_",
    ncol = 2
    ) 



# pitches %>% 
#   filter(id == "2h5CXy8Pme1lTY3CUAEI1i") %>% 
#   ggplot(aes(start, as.factor(pitches), color = pitches), width = .9) +
#   #geom_tile() +
#   #geom_point() +
#   geom_segment(aes(x= start, y = values, xend = start + duration, yend = values, alpha = loudness_max)) +
#   theme_void() +
#   theme(
#     legend.position = "none",
#     panel.background = element_rect(fill = 'black')
#   ) +
#   scale_color_gradient(
#     low = "#FFBF00",
#     high = "#EADDCA"
#   )
    
  #facet_wrap(~name, scales = "free") 
  
