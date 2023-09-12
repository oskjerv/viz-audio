
# https://developer.spotify.com/documentation/web-api/reference/get-audio-analysis

datafiles <- list.files("../data/", full.names = TRUE)


album <- readxl::read_excel("../data/album.xlsx") %>% 
  select(id, name, duration_ms, track_number)

tracks <- readxl::read_excel("../data/tracks.xlsx") %>% 
  rename(id = `...1`) %>% 
  select(id, end_of_fade_in, start_of_fade_out, loudness, tempo, tempo_confidence, time_signature, time_signature_confidence, key, key_confidence, mode, mode_confidence)

bars <- 
  lapply(datafiles[str_detect(datafiles, "bars")], function(x){
    #if(str_detect(x, "bars")){
      readxl::read_excel(x) %>% 
      mutate(
        id = str_extract(x, "[^_]+"),
        id = str_remove(id, "../data/")
        )
    #}
  }) %>% 
  bind_rows() %>% 
  select(-`...1`)

tatums <- 
  lapply(datafiles[str_detect(datafiles, "tatums")], function(x){
    #if(str_detect(x, "bars")){
    readxl::read_excel(x) %>% 
      mutate(
        id = str_extract(x, "[^_]+"),
        id = str_remove(id, "../data/")
      )
    #}
  }) %>% 
  bind_rows() %>% 
  select(-`...1`)

segments <- 
  lapply(datafiles[str_detect(datafiles, "segments")], function(x){
    #if(str_detect(x, "bars")){
    readxl::read_excel(x) %>% 
      mutate(
        id = str_extract(x, "[^_]+"),
        id = str_remove(id, "../data/")
      )
    #}
  }) %>% 
  bind_rows() %>% 
  select(-`...1`)

sections <- 
  lapply(datafiles[str_detect(datafiles, "sections")], function(x){
    #if(str_detect(x, "bars")){
    readxl::read_excel(x) %>% 
      mutate(
        id = str_extract(x, "[^_]+"),
        id = str_remove(id, "../data/")
      )
    #}
  }) %>% 
  bind_rows() %>% 
  select(-`...1`)

beats <- 
  lapply(datafiles[str_detect(datafiles, "beats")], function(x){
    #if(str_detect(x, "bars")){
    readxl::read_excel(x) %>% 
      mutate(
        id = str_extract(x, "[^_]+"),
        id = str_remove(id, "../data/")
      )
    #}
  }) %>% 
  bind_rows() %>% 
  select(-`...1`)

