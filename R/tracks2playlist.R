# Paste a list of song names and artist to convert them to a Spotify playlist

tracks2playlist <- function(playlist_name, tracks) {
    source("R/spotify.R")
    access_token <- get_new_access_token()
    
    track_split <- gsub('"', '', strsplit(tracks, "\n")[[1]])
    track_ids <- sapply(track_split, track2id)
    
    export_playlist(playlist_name, track_ids)    
}


tracks <- '"Bulls On Parade" - Rage Against The Machine
"Seven Nation Army" - The White Stripes
"Come With Me Now" - KONGOS
"Ban All the Music" - Nothing But Thieves'

tracks2playlist("Band", tracks)
