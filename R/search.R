
create_playlist_searches <- function(playlist_name, playlist_link, num_searches) {
    
    # Get fresh access token --------------------------------------------------
    
    source("R/spotify.R")
    access_token <- get_new_access_token()
    
    # Import playlist ---------------------------------------------------------
    
    source_playlist <- import_playlist(playlist_link)
    
    # Make Google searches ----------------------------------------------------
    
    search_results <- make_google_searches(source_playlist, num_searches)
    
    # Save to cache -----------------------------------------------------------
    
    f <- sprintf("cache/results/%s.cache", playlist_name)
    
    if(file.exists(f)) {
        search_results <- c(readRDS(f), search_results)
    } 
    
    saveRDS(search_results, f)
    
    return(search_results)
}


make_google_searches <- function(source_playlist, num_searches) {
    
    total_searches <- 0
    
    search_results <- list()
    history <- c()  
    
    while(total_searches < num_searches) {
        
        n <- sample(1:3, 1)
        
        search_tracks <- sample(source_playlist[["tracks"]], n)
        search_titles <- sapply(search_tracks, function(track) sprintf("%s - %s", track[["name"]], paste(track[["artists"]], collapse = ", ")))
        search_string <- paste(sort(search_titles), collapse = " | ")
        
        if(search_string %in% history) {
            next
        }
        
        search_result <- search_valueserp(search_tracks, search_titles)
            
        if(length(search_result[["search_links"]]) > 0) {
            search_result[["search_playlists"]] <- lapply(search_result[["search_links"]], function(link) playlist2dt(import_playlist(link)))
        } else {
            search_result[["search_playlists"]] <- NULL
        }
        
        search_results[[length(search_results) + 1]] <- search_result
        
        total_searches <- total_searches + 1
        
        print(sprintf("Search %d/%d | Num Results: %d | Search: %s", 
                      total_searches, num_searches, search_result[["total_results"]], search_string))
        
        history <- c(history, search_string)
    }
    
    return(search_results)
}


search_valueserp <- function(search_tracks, search_titles) {
    api_key <- readLines("tokens/valueserp.token")
    
    latin_titles <- stringi::stri_trans_general(search_titles, "latin-ascii")
    query <- gsub(" ", "+", sprintf('site:open.spotify.com inurl:playlist %s', paste(sprintf('"%s"', latin_titles), collapse = " ")))
    
    query <- gsub("&", "%26", query)
    query <- gsub("'", "%27", query)
    
    address <- sprintf("https://api.valueserp.com/search?api_key=%s&q=%s", api_key, query)
    res <- content(httr::GET(address))
    
    if(!("organic_results" %in% names(res))) {
        return(list(search_links = NULL,
                    total_results = 0))
    } 
    
    return(list(search_tracks = search_tracks,
                search_links = sapply(res[["organic_results"]], function(x) x[["link"]]),
                total_results = res[["search_information"]][["total_results"]]))
}


collate_search_results <- function(search_results, playlist_link, user_to_exclude) {
    
    # Get fresh access token --------------------------------------------------
    
    source("R/spotify.R")
    access_token <- get_new_access_token()
    
    # Import playlist ---------------------------------------------------------
    
    source_playlist <- import_playlist(playlist_link)
    source_dt <- playlist2dt(source_playlist)
    
    # Create target playlist dt -----------------------------------------------
    
    target_dt <- rbindlist(lapply(search_results, function(result) {
        if(is.null(result[["search_playlists"]])) {
            return(NULL)
        }
        
        result_dt <- rbindlist(lapply(1:length(result[["search_playlists"]]), function(i) {
            x <- result[["search_playlists"]][[i]]
            
            if(is.null(x)) {
                return(NULL)
            }
            
            x[, playlist_id := link2id(result[["search_links"]][i])]
            x[, playlist_length := nrow(x)]
            
            search_names <- sapply(result[["search_tracks"]], function(x) sprintf("%s - %s", x[["name"]], paste(x[["artists"]], collapse = ", ")))
            matched_names <- unique(c(search_names, x[["full_name"]][x[["full_name"]] %in% source_dt[["full_name"]]]))
            
            x[, matched_tracks := lapply(1:nrow(x), function(z) matched_names)]
            x[, num_matches := length(matched_names)]
            return(x)
        }))
        
        result_dt[, search_results := result[["total_results"]]]
    }))
    
    target_dt <- target_dt[!(name == "" | artists == "" | release_date == "" | release_date == "0000")]
    target_dt <- target_dt[!(id %in% source_dt[["id"]])]    
    target_dt <- target_dt[!(full_name %in% source_dt[["full_name"]])]
    
    # Also, exclude tracks from given user ------------------------------------
    
    if(!is.null(user_to_exclude)) {
        exclusion_playlists <- get_user_playlists(user_to_exclude)
        exclusion_names <- sapply(exclusion_playlists, function(x) playlist2dt(import_playlist(x))[["full_name"]])
        exclusion_names <- unique(unlist(exclusion_names))
        
        target_dt <- target_dt[!(full_name %in% exclusion_names)]
    }
    
    # Convert dates to same format --------------------------------------------
    
    target_dt[nchar(release_date) == 4, release_date := sprintf("%s-01-01", release_date)]
    target_dt[nchar(release_date) == 7, release_date := sprintf("%s-01", release_date)]
    target_dt[, release_date := as.Date(release_date)]
    target_dt <- target_dt[!(year(release_date) < 1700)]
}

