library(shiny)
library(shinyjs)
library(bslib)
library(future)
library(promises)
future::plan(multisession)
# options(future.availableCores = 2)

ui <- fluidPage(
    p("The time is ", textOutput("current_time", inline=TRUE)),
    hr(),
    input_task_button("start", "Get 10 more numbers!"),
    actionButton("cancelBtn", "Cancel"),
    textOutput("resultsOutput"),
    numericInput(inputId = "maxN", label = "N", value = 10)
)

get_result <- function() {
    ExtendedTask$new(function(N) {
        future_promise(seed = NULL, {
            Sys.sleep(0.5)
            sample(1:N, 1)
        })
    }) |> bind_task_button("start")
}

server <- function(input, output, session) {
    observe({
        shinyjs::hide("cancelBtn")
    })
    
    rv <- reactiveValues(results = NULL, 
                         remaining = 0,
                         cancel = FALSE)
    
    output$current_time <- renderText({
        invalidateLater(1000)
        format(Sys.time(), "%H:%M:%S %p")
    })
    
    result_task <- get_result()
    
    observeEvent(input$start, {
        shinyjs::show("cancelBtn")
        rv$remaining <- 10
        rv$cancel <- FALSE
        result_task$invoke(input$maxN)
    })
    
    observeEvent(input$cancelBtn, {
        rv$cancel <- TRUE
    })
    
    observeEvent(result_task$result(), {
        rv$results <- c(rv$results, result_task$result())
        rv$remaining <- rv$remaining - 1
        
        if(rv$remaining > 0 && !rv$cancel) {
            print(sprintf("%s|%s|%s", Sys.time(), rv$remaining, rv$cancel))
            result_task$invoke(input$maxN)
        } else {
            shinyjs::hide("cancelBtn")
        }
    })
    
    output$resultsOutput <- renderText({
        paste(rv$results, collapse = ", ")
    })
}

shinyApp(ui = ui, server = server)
