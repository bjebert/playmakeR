
build_playlist <- function(search_results, playlist_name, playlist_link, user_to_exclude, num_output_tracks,
                           diversity_bias = 50, recency_bias = 50, popularity_bias = 50) {
    
    # Get fresh access token --------------------------------------------------
    
    source("R/spotify.R")
    access_token <- get_new_access_token()
    
    # Import playlist ---------------------------------------------------------
    
    source_playlist <- import_playlist(playlist_link)
    source_dt <- playlist2dt(source_playlist)
    
    results_dt <- collate_search_results(search_results, playlist_link, user_to_exclude)
    
    # results_dt <- results_dt[year(release_date) >= 2022]
    
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
    # set.seed(0)
    
    results_dt[, raw_score := (num_matches ^ 1.5) / (search_results ^ 0.2) / (playlist_length ^ 0.7)]
    results_dt[, diversity_score := 1]
    
    results_dt[, days_old := as.numeric(difftime(Sys.Date(), release_date, units = "days"))]
    results_dt[, recency_score := (days_old * (50 - recency_bias) - min(days_old * (50 - recency_bias))) * 1e-4]
    
    results_dt[, score := raw_score * diversity_score]
    
    base_sum_score <- results_dt[, sum(score)]
    
    top_dt <- results_dt[, .(SumScore = sum(score)), by = full_name][order(-SumScore)][1:num_output_tracks]
    
    # Get contribution
    
    match_dt <- results_dt[full_name %in% top_dt[["full_name"]]][, .(matched_tracks, num_matches, split_score = score / num_matches)]
    summed_dt <- data.table(full_name = match_dt[, unlist(matched_tracks)], amt = match_dt[, rep(split_score, num_matches)])[, .(contrib = sum(amt)), by = full_name]
    
    contribution_dt <- merge(source_dt[, .(id, full_name)], summed_dt, by = "full_name", all.x = T, sort = F)[order(-contrib)]
    contribution_dt[is.na(contrib), contrib := 0]
    
    contribution_dt[, meanContrib := mean(contrib)]
    loss <- contribution_dt[, sum((contrib - meanContrib) ^ 2)]
    
    # Iterate to reduce loss --------------------------------------------------
    
    # Strategy: use diversity to adjust and make sure each song is represented as close to even as possible
    
    iter <- 0
    results <- data.table()
    history <- list()
    
    adj_amt <- 0.1
    jitter_amt <- 0.98
    # adj_amt <- runif(n = 1, min = 0.04, max = 0.08)
    
    while(iter < 100) {
        clist <- contribution_dt[["contrib"]]
        names(clist) <- contribution_dt[["full_name"]]
        mean_contrib <- mean(contribution_dt[["contrib"]])
        
        results_dt[, adj_ratio := sapply(results_dt[["matched_tracks"]], function(x) {
            (mean_contrib * length(x)) / sum(clist[x])
        })]
        
        results_dt[is.na(adj_ratio), adj_ratio := 0.25]
        results_dt[is.infinite(adj_ratio), adj_ratio := 10]
        
        results_dt[, diversity_score := pmin(diversity_bias * 2, pmax(0.001, diversity_score * (adj_ratio ^ adj_amt)))]
        results_dt[, jitter := runif(.N, jitter_amt, 1/jitter_amt)]
        results_dt[, score := raw_score * diversity_score * jitter]
        
        results_dt[, diversity_score := diversity_score / (sum(score) / base_sum_score)]  # Ensure sum of score doesnt change (cant hack loss by just decreasing all scores)
        
        top_dt <- results_dt[, .(SumScore = sum(score)), by = full_name][order(-SumScore)][1:num_output_tracks]
        
        # Get contribution
        
        match_dt <- results_dt[full_name %in% top_dt[["full_name"]]][, .(matched_tracks, num_matches, split_score = score / num_matches)]
        summed_dt <- data.table(full_name = match_dt[, unlist(matched_tracks)], amt = match_dt[, rep(split_score, num_matches)])[, .(contrib = sum(amt)), by = full_name]
        
        contribution_dt <- merge(source_dt[, .(id, full_name)], summed_dt, by = "full_name", all.x = T, sort = F)[order(-contrib)]
        contribution_dt[is.na(contrib), contrib := 0]
        
        contribution_dt[, meanContrib := mean(contrib)]
        new_loss <- contribution_dt[, sum((contrib - meanContrib) ^ 2)]
        
        if(new_loss > loss) {
            adj_amt <- adj_amt * 0.96
        }
        
        loss <- new_loss
        
        iter <- iter + 1
        print(sprintf("Iteration: %d, Loss: %s", iter, round(loss, 2)))
        
        results <- rbind(results, data.table(iter = iter, loss = loss))
        history[[length(history) + 1]] <- results_dt[["diversity_score"]]
    }
    
    print(results[loss == min(loss)])
    
    results_dt[, diversity_score := history[[results[loss == min(loss), iter]]]]
    results_dt[, score := raw_score * diversity_score]
    
    top_dt <- results_dt[, .(SumScore = sum(score)), by = full_name][order(-SumScore)][1:num_output_tracks]
    top_dt <- merge(top_dt, results_dt[, .SD[1], by = .(full_name)][, .(full_name, id)], by = "full_name", all.x = T, sort = F)
    
    match_dt <- results_dt[full_name %in% top_dt[["full_name"]]][, .(matched_tracks, num_matches, split_score = score / num_matches)]
    summed_dt <- data.table(full_name = match_dt[, unlist(matched_tracks)], amt = match_dt[, rep(split_score, num_matches)])[, .(contrib = sum(amt)), by = full_name]
    
    contribution_dt <- merge(source_dt[, .(id, full_name)], summed_dt, by = "full_name", all.x = T, sort = F)[order(-contrib)]
    contribution_dt[is.na(contrib), contrib := 0]
    
    top_contrib <- lapply(1:nrow(top_dt), function(i) {
        match_dt <- results_dt[full_name == top_dt[i][["full_name"]]][, .(matched_tracks, num_matches, split_score = score / num_matches)]
        data.table(full_name = match_dt[, unlist(matched_tracks)], amt = match_dt[, rep(split_score, num_matches)])[, .(contrib = sum(amt)), by = full_name][order(-contrib)]
    })
    
    
    export_playlist(sprintf("PlaymakeR-%s (diversity)", playlist_name), top_dt[["id"]])
}


breakdown <- function(results_dt, top_dt) {
    for(trk in top_dt[["full_name"]][7:8]) {
        match_dt <- results_dt[full_name == trk][, .(playlist_id, matched_tracks, num_matches, split_score = score / num_matches)]
        summed_dt <- data.table(full_name = match_dt[, unlist(matched_tracks)], amt = match_dt[, rep(split_score, num_matches)])[, .(contrib = sum(amt)), by = full_name][order(-contrib)]
        
        print(trk)
        print(summed_dt)
    }
}