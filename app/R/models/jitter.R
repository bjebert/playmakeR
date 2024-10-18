
build_playlist <- function(search_results, playlist_name, playlist_link, user_to_exclude, num_output_tracks,
                           diversity_bias = 50, recency_bias = 50, popularity_bias = 50) {
    
    # Get fresh access token --------------------------------------------------
    
    source("R/spotify.R")
    access_token <- get_new_access_token()
    
    # Import playlist ---------------------------------------------------------
    
    source_playlist <- import_playlist(playlist_link)
    source_dt <- playlist2dt(source_playlist)
    
    results_dt <- collate_search_results(search_results, playlist_link, user_to_exclude)
    
    # Resolve popularity ------------------------------------------------------
    
    # z <- Sys.time()
    # popularity <- sapply(unique(results_dt[["full_name"]]), function(trk) {
    #     p <- results_dt[["popularity"]][results_dt[["full_name"]] == trk]
    #     if(length(unique(p)) > 1) {
    #         return(as.numeric(names(rev(sort(table(p)))[1])))
    #     } else {
    #         return()
    #     }
    # })
    # print(Sys.time() - z)
    
    
    # Weightings --------------------------------------------------------------
    
    results_dt[, raw_score := (num_matches ^ 1.5) / (search_results ^ 0.2) / (playlist_length ^ 0.7)]
    results_dt[, diversity_score := 1]
    
    results_dt[, days_old := as.numeric(difftime(Sys.Date(), release_date, units = "days"))]
    results_dt[, recency_score := (days_old * (50 - recency_bias) - min(days_old * (50 - recency_bias))) * 1e-4]
    
    results_dt[, score := raw_score * diversity_score]
    
    base_sum_score <- results_dt[, sum(score)]
    
    top_dt <- results_dt[, .(SumScore = sum(score)), by = full_name][order(-SumScore)][1:num_output_tracks]


    # Iterate, randomising weights as we go -----------------------------------
    
    iter <- 0
    max_iter <- diversity_bias * 2
    while(iter < max_iter) {
        results_dt[, diversity_score := diversity_score * runif(nrow(results_dt), min = 0.9, max = 1/0.9)]
        results_dt[, score := raw_score * diversity_score]
        
        top_track <- results_dt[, .(SumScore = sum(score)), by = full_name][order(-SumScore)][1]
        
        # if(iter %% 10 == 0) {
        print(sprintf("Iter %d / %d: New top track: %s (%s)", iter, max_iter, top_track[["full_name"]], top_track[["SumScore"]]))
        # }
        
        iter <- iter + 1
        top_dt <- results_dt[, .(SumScore = sum(score)), by = full_name][order(-SumScore)][1:num_output_tracks]
        top_dt <- merge(top_dt, results_dt[, .SD[1], by = .(full_name)][, .(full_name, id)], by = "full_name", all.x = T, sort = F)
        print(top_dt)
    }
    
    results_dt[, score := raw_score * diversity_score]
    
    top_dt <- results_dt[, .(SumScore = sum(score)), by = full_name][order(-SumScore)][1:num_output_tracks]
    top_dt <- merge(top_dt, results_dt[, .SD[1], by = .(full_name)][, .(full_name, id)], by = "full_name", all.x = T, sort = F)
    
    export_playlist(sprintf("PlaymakeR-%s (jitter)", playlist_name), top_dt[["id"]])
}