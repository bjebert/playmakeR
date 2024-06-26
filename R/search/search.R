
create_playlist_searches <- function(playlist_name, playlist_link, num_searches, user_to_exclude, trawl = FALSE) {
    
    # Get fresh access token --------------------------------------------------
    
    source("R/spotify.R")
    access_token <- get_new_access_token()
    
    # Import playlist ---------------------------------------------------------
    
    source_playlist <- import_playlist(playlist_link, use_cache = F)
    
    # Create search cache -----------------------------------------------------
    
    cache_dt <- create_cache(user_to_exclude, playlist_link)
    
    # Make Google searches ----------------------------------------------------
    
    search_results <- make_google_searches(playlist_name, source_playlist, num_searches, cache_dt, trawl)
}


create_cache <- function(user_to_exclude, playlist_link) {
    
    if(!is.null(user_to_exclude)) {
        exclusion_playlists <- unname(c(sapply(get_user_playlists(user_to_exclude), link2id), link2id(playlist_link), "1vKWNlUNYhZtlOI5ZIcy8W"))
    } else {
        exclusion_playlists <- NULL
    }
    
    #' Search from cached playlists instead of Google
    
    cached_ids <- sapply(strsplit(list.files("cache/playlists"), "\\."), function(x) x[1])
    cached_ids <- cached_ids[!(cached_ids %in% exclusion_playlists)]
    
    full_cache <- lapply(cached_ids, function(playlist_id) {
        playlist <- import_playlist(playlist_id, use_cache = TRUE, cache_expiry_days = Inf)
        dt <- playlist2dt(playlist)
        
        if(is.data.table(dt) && nrow(dt) > 0) {
            dt[, playlist_id := playlist_id]
        }
        
        return(dt)
    })
    
    cache_dt <- rbindlist(full_cache)
    
    return(cache_dt)
}


search_cache <- function(cache_dt, search_tracks, is_track_search) {
    if(is_track_search) {
        playlist_ids <- Reduce(intersect, lapply(search_tracks, function(track) {
            cache_dt[id == track[["id"]]][["playlist_id"]]
        }))
    } else {
        cache_subset <- copy(cache_dt)  # Use subsetting to speed artist search up; progressively reduce search space
        artists <- sapply(search_tracks, function(x) x[["artists"]][[1]])
        
        for(artist in artists) {
            matching_playlist_ids <- unique(cache_subset[grepl(artist, artists)][["playlist_id"]])
            cache_subset <- cache_subset[playlist_id %in% matching_playlist_ids]
            
            if(nrow(cache_subset) == 0) {
                break
            }
        }
        
        playlist_ids <- matching_playlist_ids
    }
    
    return(list(search_tracks = search_tracks,
                search_links = playlist_ids, 
                total_results = length(playlist_ids)))
}


make_google_searches <- function(playlist_name, source_playlist, num_searches, cache_dt, trawl = FALSE) {
    
    # Support for caching of search results
    
    cf <- sprintf("cache/search/%s.rds", playlist_name)
    
    if(file.exists(cf)) {
        cache <- readRDS(cf)
        search_results <- cache[["search_results"]]
        history <- cache[["history"]]
        print(sprintf("Note: %d cached search results already exist for this playlist", length(search_results)))
    } else {
        search_results <- list()
        history <- c()  
    }
    
    successful_searches <- 0
    
    while(successful_searches < num_searches) {
        
        r <- runif(1)
        
        # 50%: search for tracks
        # 50%: search for artists
        is_track_search <- r <= 0.5
        
        if(is_track_search) {
            r <- runif(1)
            
            # 20%: 1 song
            # 60%: 2 songs
            # 15%: 3 songs
            # 5%: 4 songs
            
            if(r <= 0.2) {
                n <- 1
            } else if(r <= 0.8) {
                n <- 2
            } else if(r <= 0.95) {
                n <- 3
            } else {
                n <- 4
            }
            
            search_tracks <- sample(source_playlist[["tracks"]], n)
            search_titles <- sapply(search_tracks, function(track) sprintf("%s - %s", track[["name"]], paste(track[["artists"]], collapse = ", ")))
            
        } else {
            n <- sample(2:4, 1)
            search_tracks <- sample(source_playlist[["tracks"]], n)
            search_titles <- unique(sort(sapply(search_tracks, function(x) x[["artists"]][1])))
        }
        
        history_string <- paste(sort(search_titles), collapse = " | ")
        
        if(history_string %in% history) {
            next
        }
        
        search_result <- search_valueserp(search_tracks, search_titles)
        cache_result <- search_cache(cache_dt, search_tracks, is_track_search)
        
        if(length(search_result[["search_links"]]) > 0) {
            search_result[["search_playlists"]] <- lapply(search_result[["search_links"]], function(link) playlist2dt(import_playlist(link)))
        } else {
            search_result[["search_playlists"]] <- NULL
        }
        
        if(length(cache_result[["search_links"]]) > 0) {
            cache_result[["search_playlists"]] <- lapply(cache_result[["search_links"]], function(link) playlist2dt(import_playlist(link)))
        } else {
            cache_result[["search_playlists"]] <- NULL
        }
        
        search_results[[length(search_results) + 1]] <- search_result
        search_results[[length(search_results) + 1]] <- cache_result
        
        successful_searches <- successful_searches + 1
        
        print(sprintf("Search %d/%d | Num Results: %d | Num Results (cache): %d | Search: %s", 
                      successful_searches, num_searches, search_result[["total_results"]], cache_result[["total_results"]], history_string))
        
        if(trawl) {  # For top 10 results, import other playlists from users that created them
            sapply(head(unname(sapply(c(search_result[["search_links"]], cache_result[["search_links"]]), link2id)), 10), function(playlist_id) {
                import_other_playlists_from_creator(playlist_id, verbose = TRUE)    
            })
        }
        
        history <- c(history, history_string)
        
        cache <- list(search_results = search_results,
                      history = history)
        
        saveRDS(cache, cf)
    }
}


search_valueserp <- function(search_tracks, search_titles) {
    api_key <- readLines("tokens/valueserp.token")
    
    latin_titles <- stringi::stri_trans_general(search_titles, "latin-ascii")
    query <- gsub(" ", "+", sprintf('site:open.spotify.com inurl:playlist %s', paste(sprintf('"%s"', latin_titles), collapse = " ")))
    
    query <- gsub("&", "%26", query)
    query <- gsub("'", "%27", query)
    
    address <- sprintf("https://api.valueserp.com/search?api_key=%s&max_page=3&q=%s&filter=0", api_key, query)
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
    
    unique_playlists <- c()
    
    target_dt <- rbindlist(lapply(search_results, function(result) {
        if(is.null(result[["search_playlists"]]) || is.null(unlist(result[["search_playlists"]]))) {
            return(NULL)
        }
        
        result_dt <- rbindlist(lapply(1:length(result[["search_playlists"]]), function(i) {
            x <- result[["search_playlists"]][[i]]
            
            if(is.null(x)) {
                return(NULL)
            }
            
            playlist_id <- link2id(result[["search_links"]][i])
            
            if(playlist_id %in% unique_playlists) {
                return(NULL)
            } else {
                unique_playlists <<- c(unique_playlists, playlist_id)
            }
            
            x[, playlist_id := playlist_id]
            x[, playlist_length := nrow(x)]
            
            search_names <- sapply(result[["search_tracks"]], function(x) sprintf("%s - %s", x[["name"]], paste(x[["artists"]], collapse = ", ")))
            matched_names <- unique(c(search_names, x[["full_name"]][x[["full_name"]] %in% source_dt[["full_name"]]]))
            
            x[, matched_tracks := lapply(1:nrow(x), function(z) matched_names)]
            x[, num_matches := length(matched_names)]
            return(x)
        }))
        
        if(nrow(result_dt) == 0) {
            return(NULL)
        }
        
        if(is.null(result[["total_results"]])) {
            result[["total_results"]] <- length(result[["search_playlists"]])
        }
        
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
    
    return(target_dt)
}

