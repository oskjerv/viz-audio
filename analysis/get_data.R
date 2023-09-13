
# https://developer.spotify.com/documentation/web-api/reference/get-audio-analysis

datafiles <- list.files("../data/", full.names = TRUE)


album <- readxl::read_excel("../data/album.xlsx") %>% 
  select(id, name, duration_ms, track_number)

tracks <- readxl::read_excel("../data/tracks.xlsx") %>% 
  rename(id = `...1`) %>% 
  select(id, end_of_fade_in, start_of_fade_out, loudness, tempo, tempo_confidence, time_signature, time_signature_confidence, key, key_confidence, mode, mode_confidence)


import_data <- 
  function(files, str){
  suppressMessages({
    lapply(files[str_detect(files, str)], function(x){
      #if(str_detect(x, "bars")){
      readxl::read_excel(x) %>% 
        mutate(
          id = str_extract(x, "[^_]+"),
          id = str_remove(id, "../data/")
        )
      #}
    }) %>% 
      bind_rows() %>% 
      select(-starts_with("...1"))
  })
}

bars <- import_data(datafiles, "bars")
tatums <- import_data(datafiles, "tatums")
segments <- import_data(datafiles, "segments")
sections <- import_data(datafiles, "sections")
beats <- import_data(datafiles, "beats")
