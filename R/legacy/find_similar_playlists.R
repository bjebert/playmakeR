library(data.table)
rm(list = ls())

#' Create search results before running this function for better results ------------


find_similar_playlists <- function(playlist_link) {
    source("R/spotify.R")
    access_token <- get_new_access_token()
    
    playlist <- import_playlist(playlist_link, use_cache = F)
    playlist_ids <- unique(unlist(sapply(playlist[["tracks"]], function(x) x[["id"]])))
    
    cached_playlists <- list.files("cache/playlists")
    cached_ids <- unname(sapply(cached_playlists, function(x) strsplit(x, ".cache")[[1]][1]))
    
    results <- rbindlist(lapply(cached_ids, function(id) {
        cached_playlist <- import_playlist(sprintf("https://open.spotify.com/playlist/%s", id), use_cache = T, cache_expiry_days = Inf)
        matched_tracks <- cached_playlist[["tracks"]][sapply(cached_playlist[["tracks"]], function(x) !is.null(x[["id"]]) && x[["id"]] %in% playlist_ids)]
        
        track_names <- lapply(matched_tracks, function(x) sprintf("%s - %s", x[["name"]], paste(x[["artists"]], collapse = ", ")))
        
        return(data.table(id = id, matches = length(matched_tracks), num_tracks = length(cached_playlist[["tracks"]]),
                          tracks = list(track_names)))
    }))
    
    # Exclude our own playlists
    
    exclusion_playlists <- get_user_playlists("1254892983")
    results <- results[order(-matches)][!(id %in% sapply(strsplit(exclusion_playlists, "/"), function(x) x[6]))]
    
    results[, ratio := matches / num_tracks]
    
    results[, link := sprintf("https://open.spotify.com/playlist/%s", id)]
    
    # Find Xth percentile of matches
    qm <- quantile(results[["matches"]], probs = 0.98)
    
    results[order(-matches)][1:10]
    r <- results[matches > qm][order(-ratio)][1:20]
    
    r[11][["tracks"]]
    r[11]
}


playlist_link <- "https://open.spotify.com/playlist/37i9dQZF1F0sijgNaJdgit"  # Your Top Songs 2022
playlist_link <- "https://open.spotify.com/playlist/7b6ueoSafCYxT0Ibn5RFmU"  # Favs
playlist_link <- "https://open.spotify.com/playlist/20I4zuJOtEHVV83jbw6VOh?si=4ed0369991bf4f50"  # The Archive
playlist_link <- "https://open.spotify.com/playlist/662K5c1IVOW3RdGAHl15sw?si=dcceaf22726044ee"  # Summer II
playlist_link <- "https://open.spotify.com/playlist/1vKWNlUNYhZtlOI5ZIcy8W?si=c97cc3cb936144d1"  # Running 2023
playlist_link <- "https://open.spotify.com/playlist/4N8OjXizWL8cQBH69gmT6O?si=25a42bd67b3e4bd5"  # Gym 2023

find_similar_playlists(playlist_link)  





