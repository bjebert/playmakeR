
# Libraries and helpers ---------------------------------------------------

library(data.table)
api <- new.env()

sys.source("R/spotify.R", envir = api)


import_playlist_to_server <- function(rv, playlist_link) {
    playlist <- readRDS("tmpplaylist.rds")
    # playlist <- api$import_playlist(playlist_link, use_cache = FALSE, get_metadata = TRUE)
    
    if(!is.null(playlist)) {
        rv$imported_playlist <- copy(playlist)
    }
}


create_track_ui_row <- function(track, top_px) {
    div(style = "font-size: 12px", fluidRow(
        absolutePanel(top = top_px, left = "0px", width = "160px", div(style = "font-weight: bold", track[["name"]])),
        absolutePanel(top = top_px, left = "160px", width = "260px", paste(track[["artists"]], collapse = ", "))
    ))
}

# Server ------------------------------------------------------------------

server <- function(input, output) {
    rv <- reactiveValues(imported_playlist = NULL)
    
    observeEvent(input$import_playlist, import_playlist_to_server(rv, input$playlist_link))
    
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


}