


build_playlist <- function(search_results, playlist_name, playlist_link, user_to_exclude, num_output_tracks,
                           diversity_bias = 50, recency_bias = 50, popularity_bias = 50) {
    
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
            
            search_ids <- sapply(result[["search_tracks"]], function(x) x[["id"]])
            matched_ids <- unique(c(search_ids, x[["id"]][x[["id"]] %in% source_dt[["id"]]]))
            
            x[, matched_tracks := lapply(1:nrow(x), function(z) matched_ids)]
            x[, num_matches := length(matched_ids)]
            return(x)
        }))
        
        result_dt[, search_results := result[["total_results"]]]
    }))
    
    target_dt <- target_dt[!(id %in% source_dt[["id"]])]    
    
    # In case of duplicate IDs, delete by matches in name and artists too
    target_dt[, artist_text := sapply(target_dt[["artists"]], function(x) paste(x, collapse = ", "))]
    target_dt[, full_name := sprintf("%s - %s", name, artist_text)]
    
    source_dt[, artist_text := sapply(source_dt[["artists"]], function(x) paste(x, collapse = ", "))]
    source_dt[, full_name := sprintf("%s - %s", name, artist_text)]
    
    target_dt <- target_dt[!(full_name %in% source_dt[["full_name"]])]
    
    # Weightings --------------------------------------------------------------

    target_dt[, raw_score := (num_matches ^ 2) / (search_results ^ 0.3) / (playlist_length ^ 0.5)]
    target_dt[, diversity_score := 1]
    target_dt[, score := raw_score * diversity_score]
    
    top_dt <- target_dt[, .(SumScore = sum(score)), by = full_name][order(-SumScore)][1:num_output_tracks]
    
    # Get contribution
    
    match_dt <- target_dt[full_name %in% top_dt[["full_name"]]][, .(matched_tracks, num_matches, split_score = score / num_matches)]
    summed_dt <- data.table(id = match_dt[, unlist(matched_tracks)], amt = match_dt[, rep(split_score, num_matches)])[, .(contrib = sum(amt)), by = id]
    
    contribution_dt <- merge(source_dt[, .(id, full_name)], summed_dt, by = "id", all.x = T, sort = F)[order(-contrib)]
    contribution_dt[is.na(contrib), contrib := 0]
    
    contribution_dt[, meanContrib := mean(contrib)]
    loss <- contribution_dt[, sum((contrib - meanContrib) ^ 2)]

    # Iterate to reduce loss --------------------------------------------------
    
    # Strategy: Random jitter until loss decreases
    
    iter <- 1
    jitter <- 0.2
    while(iter < 1000) {
        old_scores <- target_dt[["diversity_score"]]
        old_loss <- copy(loss)
        
        target_dt[, diversity_score := diversity_score + runif(nrow(target_dt), -jitter, jitter)]
        target_dt[, score := raw_score * diversity_score]
        
        top_dt <- target_dt[, .(SumScore = sum(score)), by = full_name][order(-SumScore)][1:num_output_tracks]
        
        # Get contribution
        
        match_dt <- target_dt[full_name %in% top_dt[["full_name"]]][, .(matched_tracks, num_matches, split_score = score / num_matches)]
        summed_dt <- data.table(id = match_dt[, unlist(matched_tracks)], amt = match_dt[, rep(split_score, num_matches)])[, .(contrib = sum(amt)), by = id]
        
        contribution_dt <- merge(source_dt[, .(id, full_name)], summed_dt, by = "id", all.x = T, sort = F)[order(-contrib)]
        contribution_dt[is.na(contrib), contrib := 0]
        
        contribution_dt[, meanContrib := mean(contrib)]
        loss <- contribution_dt[, sum((contrib - meanContrib) ^ 2)]
        
        if(loss > old_loss) {
            target_dt[, diversity_score := old_scores]
            loss <- old_loss
        }
        
        print(sprintf("Iteration: %d, Loss: %s", iter, loss))    
        
        iter <- iter + 1
    }
    
}