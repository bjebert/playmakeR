
# Libraries and helpers ---------------------------------------------------

library(data.table)
library(promises)
library(future)
library(httr)
library(DT)
plan(multisession)

api <- new.env()

sys.source("R/spotify.R", envir = api)
sys.source("R/search/search.R", envir = api)


# Helpers -----------------------------------------------------------------


import_playlist_to_server <- function(rv, playlist_link) {
    debug_mode <- T
    
    if(debug_mode) {
        playlist <- readRDS("tmp_playlist.rds")
    } else {
        playlist <- api$import_playlist(playlist_link, use_cache = FALSE, attempts = 1, get_metadata = TRUE)
        saveRDS(playlist, "tmp_playlist.rds")
    }
    
    if(playlist[["result"]] == "success") {
        rv$imported_playlist <- copy(playlist)
    } else {
        showNotification(p(playlist[["result"]]), type = "error")
    }
}


create_track_ui_row <- function(track, top_px) {
    div(style = "font-size: 12px", fluidRow(
        absolutePanel(top = top_px, left = "0px", width = "160px", div(style = "font-weight: bold", track[["name"]])),
        absolutePanel(top = top_px, left = "160px", width = "260px", paste(track[["artists"]], collapse = ", "))
    ))
}


create_search_result <- function() {
    ExtendedTask$new(function(playlist, track_vs_artist_ratio, search_specificity) {
        future_promise(seed = NULL, {
            library(httr)
            
            search_result <- api$make_google_searches(playlist[["name"]], playlist, 1, 
                                                      save_results = TRUE, song_db = NULL, trawl = FALSE,
                                                      track_vs_artist_ratio = track_vs_artist_ratio, 
                                                      search_specificity = search_specificity)
            
            return(search_result)
        })
    })
}


# Server ------------------------------------------------------------------

server <- function(input, output, session) {
    rv <- reactiveValues(imported_playlist = NULL,
                         search_mode = FALSE,
                         search_results = NULL,
                         search_amount_pending = 0,
                         search_cancel_pending = FALSE,
                         search_credits_remaining = NULL)
    
    observeEvent(input$import_playlist, import_playlist_to_server(rv, input$playlist_link))
    
    output$playlist_header <- renderUI({
        header_colour <- ifelse(is.null(rv$imported_playlist[["name"]]), "black", "green")
        
        return(div(style = sprintf("color: %s;", header_colour), h3("Playlist")))
    })
    
    # Update imported playlist details ----------------------------------------
    
    output$imported_playlist_name <- renderText({
        if(!is.null(rv$imported_playlist[["name"]])) {
            rv$imported_playlist[["name"]]
        }
    })
    
    output$imported_playlist_icon <- renderUI({
        if(!is.null(rv$imported_playlist[["icon"]])) {
            img(src = rv$imported_playlist[["icon"]], style = "width: 100px; height: 100px; object-fit: cover;")
        }
    })
    
    output$imported_playlist_details <- renderText({
        if(!is.null(rv$imported_playlist[["tracks"]])) {
            sprintf("Tracks: %d", length(rv$imported_playlist[["tracks"]]))
        }
    })
    
    output$imported_sample_tracks <- renderUI({
        if(!is.null(rv$imported_playlist[["tracks"]])) {
            tagList(lapply(1:min(6, length(rv$imported_playlist[["tracks"]])), function(i) {
                if(i < 6) {
                    return(create_track_ui_row(rv$imported_playlist[["tracks"]][[i]], (i-1)*22))
                } else {
                    return(create_track_ui_row(list("name" = "...", "artists" = "..."), (i-1)*22))
                }
            }))
        }
    })
    
    # Targeted Search ---------------------------------------------------------
    
    output$playlist_import_required_label <- renderUI({
        if(!is.null(rv$imported_playlist[["name"]])) {
            return("")
        } else {
            return(h6("Please import a playlist to begin!"))
        }
    })
    
    observeEvent(rv$imported_playlist, {
        if(!is.null(rv$imported_playlist[["name"]])) {
            shinyjs::show("targeted_search_content")
            
            # Load pre-made searches from disk
            f <- sprintf("R/search/results/%s.rds", rv$imported_playlist$id)
            if(file.exists(f)) {
                rv$search_results <- readRDS(f)[["search_results"]]
            }
        } else {
            shinyjs::hide("targeted_search_content")
        }
    })
    
    
    # Output panel ------------------------------------------------------------
    
    output$output_info_label <- renderUI({
        absolutePanel(top = "45px", left = "10px", h5("Use the Create tab to begin generating an output playlist."))
    })
    
    # Create search results ---------------------------------------------------
    
    search_result_task <- create_search_result()
    
    begin_search_mode <- function(num_searches) {
        # Start search
        updateActionButton(inputId = "create_searches", label = "Cancel Searches", icon = icon("stop"))
        rv[["search_amount_pending"]] <- num_searches
        rv[["search_cancel_pending"]] <- FALSE
        
        access_token <- api$get_new_access_token()
        
        playlist_name <- rv[["imported_playlist"]][["name"]]
        source_playlist <- rv[["imported_playlist"]]
        num_searches <- 1
        save_results <- TRUE
        song_db <- NULL
        trawl <- FALSE
        
        search_result_task$invoke(rv[["imported_playlist"]], input$track_vs_artist_slider / 100, input$search_specificity_slider)
    }
    
    exit_search_mode <- function() {
        # Stop search
        updateActionButton(inputId = "create_searches", label = "Create Searches", icon = icon("search"))
        rv[["search_amount_pending"]] <- 0
        rv[["search_cancel_pending"]] <- TRUE
    }
    
    observeEvent(input$create_searches, {
        rv[["search_mode"]] <- !rv[["search_mode"]]
        
        if(rv[["search_mode"]]) {
            print(input$num_searches)
            begin_search_mode(input$num_searches)
        } else {
            exit_search_mode()
        }
    })
    
    observeEvent(search_result_task$result(), {
        rv[["search_results"]] <- search_result_task$result()
        rv[["search_amount_pending"]] <- rv[["search_amount_pending"]] - 1
        rv[["search_credits_remaining"]] <- rv[["search_results"]][[length(rv[["search_results"]])]][["credits_remaining"]]
        
        if(rv[["search_amount_pending"]] > 0 && !rv[["search_cancel_pending"]]) {
            print(sprintf("%s | Pending = %s | Cancel? = %s", Sys.time(), rv[["search_amount_pending"]], rv[["search_cancel_pending"]]))
            search_result_task$invoke(rv[["imported_playlist"]], input$track_vs_artist_slider / 100, input$search_specificity_slider)
        } else {
            exit_search_mode()
        }
    })
    
    output$search_results_dt <- renderDT({
        
    })
    
    output$search_results_ui <- renderUI({
        if(length(rv[["search_results"]]) == 0) {
            return()
        }
        
        left_chars <- sum(nchar(c(as.character(c(length(rv[["search_results"]]), 
                                                 rv[["search_amount_pending"]] + length(rv[["search_results"]]))))))
        
        left_px_map <- c(`2` = 34, `3` = 42, `4` = 50, `5` = 58, `6` = 64)
        left_px <- left_px_map[[as.character(left_chars)]]
        
        tagList(lapply(1:min(8, length(rv[["search_results"]])), function(i) {
            search_result <- rev(rv[["search_results"]])[[i]]
            
            search_i <- length(rv[["search_results"]]) - i + 1
            total_i <- rv[["search_amount_pending"]] + length(rv[["search_results"]])
            
            search_string <- gsub("\\*", "-", paste(search_result[["search_titles"]], collapse = ", "))
            
            if(nchar(search_string) > 153) {
                search_string <- sprintf("%s...", substr(search_string, 1, 150))
            }
            
            if(nchar(search_string) > 100) {
                search_string_font_size <- 12
            } else {
                search_string_font_size <- 13
            }
            
            search_string_colour <- ifelse(search_result[["is_track_search"]], "#B2002F", "#3A0073")
            
            search_string_style <- sprintf("color: %s; font-size: %spx; font-weight: bold", 
                                           search_string_colour, search_string_font_size)
            
            num_results <- length(search_result[["search_playlists"]])
            
            top_px <- i*40 - 20
            
            absolutePanel(top = sprintf("%dpx", top_px), width = "100%",
                          absolutePanel(top = "0px", left = "0px", h4(sprintf("%d/%d", search_i, total_i))),
                          absolutePanel(top = "0px", left = sprintf("%dpx", left_px), width = "90%", div(style = search_string_style, search_string)),
                          absolutePanel(top = "20px", left = sprintf("%dpx", left_px), width = "100%", HTML(sprintf("Results: <b>%d</b>", num_results)))
            )
        }))
    })
    
    
    # Destroy searches --------------------------------------------------------
    
    observeEvent(input$destroy_searches, {
        if(!is.null(rv$imported_playlist[["name"]])) {
            f <- sprintf("R/search/results/%s.rds", rv$imported_playlist$id)
            if(file.exists(f)) {
                file.remove(f)
                rv$search_results <- NULL
            }
        }
    })
    
    # Misc UI
    
    output$search_credits_remaining_ui <- renderUI({
        if(is.null(rv[["search_credits_remaining"]])) {
            return(NULL)
        } else {
            return(strong(sprintf("Credits Remaining: %d", rv[["search_credits_remaining"]])))
        }
    })
}