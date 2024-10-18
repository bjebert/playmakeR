# Paste a list of song names and artist to convert them to a Spotify playlist

# tracks2playlist <- function(playlist_name, tracks) {
#     source("R/spotify.R")
#     access_token <- get_new_access_token()
#     
#     track_split <- gsub('"', '', strsplit(tracks, "\n")[[1]])
#     track_ids <- sapply(track_split, track2id)
#     
#     export_playlist(playlist_name, track_ids)    
# }
# 
# tracks <- "Always Like This - Bombay Bicycle Club
# Dissolve - Absofacto
# Paresthesia - Wild Ones
# Night Knuckles - Men I Trust
# Past Lives - BØRNS
# It's Alright - Fractures
# Agnes - Glass Animals
# Flaws - Vancouver Sleep Clinic
# Ultraviolet - Faded Paper Figures
# Waves - Lewis Del Mar
# You're Too Precious - James Blake
# Somebody Else - The 1975
# Beginnings - Houses
# Paris - Magic Man
# Everytime - Boy Pablo
# Sunroof - courtship.
# Talk Too Much - COIN
# Tungs - The Frights
# Pumpin Blood - NONONO"
# 
# tracks2playlist("FAVAI", tracks)
