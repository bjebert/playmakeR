
source("R/search.R")

# Searches ----------------------------------------------------------------


playlist_name <- "PartyFM"
playlist_link <- "https://open.spotify.com/playlist/5wUAaXxBU5NYKdoJt0bkJ7?si=cc6bb0f8fe4a443f"  # PartyFM

playlist_name <- "Arvos"
playlist_link <- "https://open.spotify.com/playlist/4N2YbLrUL28BNSihG5wwA0?si=cb3d65618b434e18"

playlist_name <- "Running"
playlist_link <- "https://open.spotify.com/playlist/2cjtOf78JYoEUd1rYMnrkI?si=ea0a3ac004bc4656"

playlist_name <- "Gym"
playlist_link <- "https://open.spotify.com/playlist/0eeOR0B6maAMtMXTU4q7Cz?si=9e8118d403284875"

playlist_name <- "Favs"
playlist_link <- "https://open.spotify.com/playlist/7b6ueoSafCYxT0Ibn5RFmU?si=2ad3f6904f564050"

num_searches <- 5

search_results <- create_playlist_searches(playlist_name, playlist_link, num_searches)



# Build playlist ----------------------------------------------------------

# user_to_exclude <- NULL  
user_to_exclude <- "1254892983"  # Blake
model_name <- "diversity"
num_output_tracks <- 250
search_results <- readRDS(sprintf("cache/results/%s.cache", playlist_name))
diversity_bias <- 50
# recency_bias <- 50
# popularity_bias <- 50

model <- new.env()
sys.source(sprintf("R/models/%s.R", model_name), envir = model)

model$build_playlist(search_results, playlist_name, playlist_link, user_to_exclude, num_output_tracks,
                     diversity_bias)






