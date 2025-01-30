
# Setup -------------------------------------------------------------------

library(shiny)
library(shinyjs)
library(shinyWidgets)

version_text <- "playmakeR 0.1"
breaks <- function(n) tagList(lapply(1:n, function(x) br()))

# UI ----------------------------------------------------------------------

ui <- fluidPage(
    useShinyjs(),
    
    titlePanel(version_text),
    absolutePanel(top = "58px", left = "89px", div(style = "font-size: 12px; color: #333333; font-weight: bold;", "by Blake Ebert")),
    
    tags$style(
        HTML(readLines("style.css"))
    ),
    
    tags$head(
        tags$link(rel = "shortcut icon", href = "favicon.ico")
    ),
    
    setBackgroundColor(
        color = c("#F5E8FF", "#CEE5FF"),
        gradient = "linear",
        direction = "right",
    ),
    
    # Logo --------------------------------------------------------------------

    absolutePanel(top = "19px", left = "270px", img(src = "favicon.ico")),    
    
    # Import playlist panel ---------------------------------------------------
    
    absolutePanel(
        top = "85px", left = "10px", width = "480px",
        wellPanel(
            absolutePanel(top = "10px", left = "10px", htmlOutput("playlist_header")),
            absolutePanel(top = "45px", left = "10px", width = "90%", textInput("playlist_link", label = NULL, value = "https://open.spotify.com/playlist/0DwT9xBTsBKjITvPPWk3tS")),
            absolutePanel(top = "85px", left = "10px", actionButton("import_playlist", label = "Import Playlist", icon = icon("upload"))),
            
            # Imported playlist details -----------------------------------------------
            
            absolutePanel(top = "135px", left = "10px", htmlOutput(outputId = "imported_playlist_icon")),
            absolutePanel(top = "135px", left = "130px", div(style = "font-size: 16px; font-weight: bold;", htmlOutput(outputId = "imported_playlist_name"))),
            absolutePanel(top = "155px", left = "130px", htmlOutput(outputId = "imported_playlist_details")),
            
            absolutePanel(top = "245px", left = "10px", htmlOutput(outputId = "imported_sample_tracks")),
            breaks(20)
        )
    ),
    
    
    # Playlist output panel ---------------------------------------------------
    
    absolutePanel(
        top = "502px", left = "10px", width = "480px",
        wellPanel(
            absolutePanel(top = "10px", left = "10px", h3("Output Playlist")),
            htmlOutput(outputId = "output_info_label"),
            breaks(20)
        )
    ),
    
    
    # Create/search/trawl panel -----------------------------------------------
    
    absolutePanel(
        top = "85px", left = "495px", width = "750px",
        wellPanel(
            absolutePanel(width = "98%",
                tabsetPanel(
                    tabPanel(h3("Create"),
                             absolutePanel(top = "75px", left = "10px", width = "120px", numericInput("num_output_tracks", label = "# Tracks", value = 100, min = 1, max = 500))
                    ),
                    tabPanel(h3("Search"),
                             absolutePanel(top = "30px", left = "10px", width = "90%",
                                           absolutePanel(top = "10px", left = "0px", h5("Targeted searches use the web to find playlists containing similar songs and artists to the source playlist.")),
                                           absolutePanel(top = "35px", left = "0px", htmlOutput(outputId = "playlist_import_required_label")),
                                           
                                           hidden(div(id = "targeted_search_content",
                                                      
                                                      absolutePanel(top = "35px", left = "0px", width = "72px", 
                                                                    numericInput("num_searches", label = "# Searches", value = 50, min = 1, max = 250)),
                                                      
                                                      absolutePanel(top = "105px", left = "0px",
                                                                    sliderInput("track_vs_artist_slider", label = "Track vs Artist %", min = 0, max = 100, value = 60, step = 1, 
                                                                                round = TRUE, ticks = TRUE, width = "260px")),
                                                      
                                                      absolutePanel(top = "195px", left = "0px",
                                                                    sliderInput("search_specificity_slider", label = "Search Specificity", min = 1, max = 4, value = 2, step = 0.01,
                                                                                round = FALSE, ticks = TRUE, width = "260px")),
                                                      
                                                      absolutePanel(top = "280px", left = "0px", width = "100%", 
                                                                    checkboxInput("import_playlists_from_search_users", label = "Import other playlists from creators of top search results?",
                                                                                  value = TRUE)),
                                                      
                                                      absolutePanel(top = "325px", left = "0px",
                                                                    actionButton("create_searches", label = "Create Searches", icon = icon("search"), width = "180px")),
                                                      
                                                      absolutePanel(top = "325px", left = "190px",
                                                                    actionButton("destroy_searches", label = "Destroy Searches", icon = icon("trash"), width = "180px")),
                                           ))
                             ),
                    ),
                    tabPanel(h3("Trawl"),
                             absolutePanel(top = "30px", left = "10px", width = "99%",
                                           absolutePanel(top = "10px", left = "0px", h5(HTML("Trawl imports playlists created by followers of a <b>starting user</b>. Then, it recursively imports from each of their followers."))),
                                           absolutePanel(top = "35px", left = "0px", width = "160px", 
                                                         textInput("trawl_user", label = "Starting User", value = "1254892983")),
                                           absolutePanel(top = "105px", left = "0px", width = "120px", 
                                                         numericInput("trawl_num_playlists", label = "# Playlists", value = 100, min = 1, max = 10000)),
                                           absolutePanel(top = "175px", left = "0px",
                                                         actionButton("trawl_search", label = "Trawl", icon = icon("search"), width = "180px"))
                             ),
                    ),
                )
            ),
            breaks(20)
        )
    ),
    
    # Search results panel ----------------------------------------------------
    
    absolutePanel(
        top = "502px", left = "495px", width = "750px",
        wellPanel(
            absolutePanel(top = "10px", left = "10px", h3("Search Results")),
            absolutePanel(top = "30px", left = "10px", width = "735px", htmlOutput("search_results_ui")),
            absolutePanel(top = "387px", left = "10px", htmlOutput("search_credits_remaining_ui")),
            breaks(20),
        )
    ),
)