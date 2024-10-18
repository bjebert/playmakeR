library(shiny)
library(shinyjs)
library(shinyWidgets)


version_text <- "playmakeR 0.1"
breaks <- function(n) tagList(lapply(1:n, function(x) br()))


ui <- fluidPage(
    useShinyjs(),
    
    titlePanel(version_text),
    absolutePanel(top = "58px", left = "89px", div(style = "font-size: 12px; color: #333333; font-weight: bold;", "by Blake Ebert")),
    
    tags$style(
        HTML(
            'h2 {
                 color: #333333;
                 font-family: "Segoe UI", sans-serif;
                 font-size: 38px;
            }
             h3 {
                 font-weight: bold;
                 margin: 0px;
             }
             h5 {
                 font-weight: bold;
             }
             .well {
                 padding-top: 12px;
                 padding-bottom: 12px;
                 border: 1px dashed grey;
             }
             '
        )
    ),
    
    tags$head(
        tags$link(rel = "shortcut icon", href = "favicon.ico")
    ),
    
    setBackgroundColor(
        color = c("#F5E8FF", "#CEE5FF"),
        gradient = "linear",
        direction = "right",
    ),
    
    absolutePanel(
        top = "85px", left = "10px", width = "480px",
        wellPanel(
            fluidRow(
                absolutePanel(top = "10px", left = "10px", h3("Playlist")),
                absolutePanel(top = "45px", left = "10px", width = "90%", textInput("playlist_link", label = NULL, value = "https://open.spotify.com/playlist/7b6ueoSafCYxT0Ibn5RFmU")),
                absolutePanel(top = "85px", left = "10px", actionButton("import_playlist", label = "Import Playlist", icon = icon("upload"))),
                
                # Imported playlist details -----------------------------------------------
                
                absolutePanel(top = "135px", left = "10px", htmlOutput(outputId = "imported_playlist_icon")),
                absolutePanel(top = "135px", left = "130px", div(style = "font-size: 16px; font-weight: bold;", htmlOutput(outputId = "imported_playlist_name"))),
                absolutePanel(top = "155px", left = "130px", htmlOutput(outputId = "imported_playlist_details")),
                
                absolutePanel(top = "245px", left = "10px", htmlOutput(outputId = "imported_sample_tracks"))
            ),
            breaks(18)
        )
    ),

    # Search panel (LHS) ------------------------------------------------------

    absolutePanel(
        top = "480px", left = "10px", width = "480px",
        wellPanel(
            fluidRow(
                absolutePanel(top = "10px", left = "10px", h3("Search")),
                absolutePanel(top = "45px", left = "10px", width = "120px", numericInput("num_searches", label = "# Searches", value = 50, min = 1, max = 250)),
            ),
            breaks(10)
        )
    ),
    

    # Output panel (RHS) ------------------------------------------------------

    absolutePanel(
        top = "85px", left = "1200px", width = "480px",
        wellPanel(
            fluidRow(
                absolutePanel(top = "10px", left = "10px", h3("Output")),
                absolutePanel(top = "45px", left = "10px", width = "120px", numericInput("num_output_tracks", label = "# Tracks", value = 100, min = 1, max = 500))
            ),
            breaks(18)
        )
    ),
)