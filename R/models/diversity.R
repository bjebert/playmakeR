
build_playlist <- function(search_results, playlist_name, playlist_link, user_to_exclude, num_output_tracks,
                           diversity_bias = 50, recency_bias = 50, popularity_bias = 50) {
    
    # Resolve popularity ------------------------------------------------------
    
    # z <- Sys.time()
    # popularity <- sapply(unique(target_dt[["full_name"]])[1:1000], function(trk) {
    #     if(length(unique(p)) > 1) {
    #         p <- target_dt[["popularity"]][target_dt[["full_name"]] == trk]
    #         return(as.numeric(names(rev(sort(table(p)))[1])))
    #     } else {
    #         return()
    #     }
    # })
    # print(Sys.time() - z)
    
    
    # Weightings --------------------------------------------------------------
    
    target_dt[, raw_score := (num_matches ^ 1.5) / (search_results ^ 0.2) / (playlist_length ^ 0.7)]
    target_dt[, diversity_score := 1]
    
    target_dt[, days_old := as.numeric(difftime(Sys.Date(), release_date, units = "days"))]
    target_dt[, recency_score := (days_old * (50 - recency_bias) - min(days_old * (50 - recency_bias))) * 1e-4]
    
    target_dt[, score := raw_score * diversity_score * recency_score]
    
    base_sum_score <- target_dt[, sum(score)]
    
    top_dt <- target_dt[, .(SumScore = sum(score)), by = full_name][order(-SumScore)][1:num_output_tracks]
    
    # Get contribution
    
    match_dt <- target_dt[full_name %in% top_dt[["full_name"]]][, .(matched_tracks, num_matches, split_score = score / num_matches)]
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
    
    adj_amt <- 0.05
    
    while(iter < diversity_bias) {
        clist <- contribution_dt[["contrib"]]
        names(clist) <- contribution_dt[["full_name"]]
        mean_contrib <- mean(contribution_dt[["contrib"]])
        
        target_dt[, adj_ratio := sapply(target_dt[["matched_tracks"]], function(x) {
            (mean_contrib * length(x)) / sum(clist[x])
        })]
        
        target_dt[is.na(adj_ratio), adj_ratio := 5]
        
        target_dt[, diversity_score := pmin(diversity_bias * 2, pmax(0.001, diversity_score * (adj_ratio ^ adj_amt)))]
        target_dt[, score := raw_score * diversity_score * recency_score]
        
        target_dt[, diversity_score := diversity_score / (sum(score) / base_sum_score)]  # Ensure sum of score doesnt change (cant hack loss by just decreasing all scores)
        
        top_dt <- target_dt[, .(SumScore = sum(score)), by = full_name][order(-SumScore)][1:num_output_tracks]
        
        # Get contribution
        
        match_dt <- target_dt[full_name %in% top_dt[["full_name"]]][, .(matched_tracks, num_matches, split_score = score / num_matches)]
        summed_dt <- data.table(full_name = match_dt[, unlist(matched_tracks)], amt = match_dt[, rep(split_score, num_matches)])[, .(contrib = sum(amt)), by = full_name]
        
        contribution_dt <- merge(source_dt[, .(id, full_name)], summed_dt, by = "full_name", all.x = T, sort = F)[order(-contrib)]
        contribution_dt[is.na(contrib), contrib := 0]
        
        contribution_dt[, meanContrib := mean(contrib)]
        new_loss <- contribution_dt[, sum((contrib - meanContrib) ^ 2)]
        
        if(new_loss > loss) {
            adj_amt <- adj_amt * 0.95
        }
        
        loss <- new_loss
        
        iter <- iter + 1
        print(sprintf("Iteration: %d, Loss: %s", iter, loss))
        
        results <- rbind(results, data.table(iter = iter, loss = loss))
        history[[length(history) + 1]] <- target_dt[["diversity_score"]]
    }
    
    target_dt[, diversity_score := history[[results[loss == min(loss), iter]]]]
    target_dt[, score := raw_score * diversity_score * recency_score]
    
    top_dt <- target_dt[, .(SumScore = sum(score)), by = full_name][order(-SumScore)][1:num_output_tracks]
    top_dt <- merge(top_dt, target_dt[, .SD[1], by = .(full_name)][, .(full_name, id)], by = "full_name", all.x = T, sort = F)
    
    export_playlist(sprintf("PlaymakeR-%s (diversity)", playlist_name), top_dt[["id"]])
}


breakdown <- function(target_dt, top_dt) {
    for(trk in top_dt[["full_name"]][7:8]) {
        match_dt <- target_dt[full_name == trk][, .(playlist_id, matched_tracks, num_matches, split_score = score / num_matches)]
        summed_dt <- data.table(full_name = match_dt[, unlist(matched_tracks)], amt = match_dt[, rep(split_score, num_matches)])[, .(contrib = sum(amt)), by = full_name][order(-contrib)]
        
        print(trk)
        print(summed_dt)
    }
}