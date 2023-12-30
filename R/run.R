library(data.table)

rm(list = ls())
source("R/search/search.R")

# Searches ----------------------------------------------------------------


playlist_name <- "PartyFM"
playlist_link <- "https://open.spotify.com/playlist/5wUAaXxBU5NYKdoJt0bkJ7?si=cc6bb0f8fe4a443f"  # PartyFM

playlist_name <- "Arvos"
playlist_link <- "https://open.spotify.com/playlist/4N2YbLrUL28BNSihG5wwA0?si=cb3d65618b434e18"

playlist_name <- "Running"
playlist_link <- "https://open.spotify.com/playlist/2cjtOf78JYoEUd1rYMnrkI?si=ea0a3ac004bc4656"

playlist_name <- "Gym 2023"
playlist_link <- "https://open.spotify.com/playlist/4N8OjXizWL8cQBH69gmT6O?si=25a42bd67b3e4bd5"

playlist_name <- "Favs"
playlist_link <- "https://open.spotify.com/playlist/7b6ueoSafCYxT0Ibn5RFmU?si=2ad3f6904f564050"

playlist_name <- "Limey"
playlist_link <- "https://open.spotify.com/playlist/37i9dQZF1DZ06evO3S578s?si=69c347d005fa4e94"

playlist_name <- "Running 2023"
playlist_link <- "https://open.spotify.com/playlist/1vKWNlUNYhZtlOI5ZIcy8W?si=c97cc3cb936144d1"

playlist_name <- "Your Top Songs 2022"
playlist_link <- "https://open.spotify.com/playlist/37i9dQZF1F0sijgNaJdgit?si=a21a61dfc85b4c7d"

playlist_name <- "More Listening"
playlist_link <- "https://open.spotify.com/playlist/1KN0GCQ09Khdx3ITZ9z5Kq?si=0dd7eed66ff7417e"

playlist_name <- "Summer II"
playlist_link <- "https://open.spotify.com/playlist/662K5c1IVOW3RdGAHl15sw?si=dcceaf22726044ee"

playlist_name <- "Running 2024"
playlist_link <- "https://open.spotify.com/playlist/7z9ygXKiVFMoc71UYPMSUm?si=2202d2b80f60430c"

num_searches <- 250
search_results <- create_playlist_searches(playlist_name, playlist_link, num_searches)


# Build playlist ----------------------------------------------------------

user_to_exclude <- NULL 
# user_to_exclude <- "1254892983"  # Blake
model_name <- "diversity"
num_output_tracks <- 100
search_results <- readRDS(sprintf("cache/search/%s.rds", playlist_name))[["search_results"]]  # New way
# search_results <- readRDS(sprintf("cache/results/%s.cache", playlist_name))  # Old way
diversity_bias <- 50
recency_bias <- 50
popularity_bias <- 50

model <- new.env()
sys.source(sprintf("R/models/%s.R", model_name), envir = model)

model$build_playlist(search_results, playlist_name, playlist_link, user_to_exclude, num_output_tracks,
                     diversity_bias)






