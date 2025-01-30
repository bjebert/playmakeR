# Use an unpublished web API to find all followers of a user, then continue searching outwards


get_followers <- function(user_id) {
    access_token <- readLines("tokens/access.token")
    
    res <- GET(sprintf("https://spclient.wg.spotify.com/user-profile-view/v3/profile/%s/followers", user_id),
               add_headers('Authorization' = access_token))
    
    if(status_code(res) == 401) {
        stop("Invalid access token, try manual https://open.spotify.com/user/1254892983/followers and checking headers")
    }
    
    raw_json <- paste(sapply(content(res), function(x) rawToChar(as.raw(strtoi(x, 16L)))), collapse = "")
    
    followers <- as.data.table(jsonlite::fromJSON(raw_json)[["profiles"]])
    followers <- followers[, .(uri, name, followers_count)]
    followers[, id := sapply(strsplit(followers[["uri"]], ":"), function(x) x[length(x)])]
    
    return(followers)
}


trawl_friends <- function(user_id = "1254892983") {
    
    # Get fresh access token --------------------------------------------------
    
    source("R/spotify.R")
    access_token <- get_new_access_token()

    # Begin trawl -------------------------------------------------------------

    followers_dt <- get_followers(user_id)
    followers <- followers_dt[["id"]]
    
    while(length(followers) > 0) {
        import_user_playlists(followers[1])
        
        new_followers <- get_followers(user_id = followers[1])
        
        followers_dt <- rbind(followers_dt, new_followers)
        followers <- unique(c(followers, new_followers[["id"]]))
                        
        followers <- followers[-1]
    }
}