server <- function(input, output) {
  ##bs_themer()
  bslib::toggle_dark_mode() ## would make sense to offer both dark and light mode to the user
  
  position_list <- shiny::reactiveVal(list(
    list(
      shiny::div(style = "padding: 10px; margin-bottom: 10px; border: 1px solid #4CAF50;",
          shiny::selectInput("rate_1", "Select Rate", choices = rates_list, selected = NULL),
          shiny::numericInput("amount_1", "Amount Allocated", value = 0, min = 0),
          shiny::selectInput("position_1", "Position", choices = c("Long", "Short"), selected = "Long")
      )
    )
  ))
  
  observeEvent(input$add, {
    n <- length(position_list()) + 1
    new_ui <- list(
      shiny::div(style = "padding: 10px; margin-bottom: 10px; border: 1px solid #4CAF50;",
          shiny::selectInput(paste0("rate_", n), "Select Rate", choices = rates_list, selected = NULL),
          shiny::numericInput(paste0("amount_", n), "Amount Allocated", value = 0, min = 0),
          shiny::selectInput(paste0("position_", n), "Position", choices = c("Long", "Short"), selected = "Long")
      ))
    position_list(append(position_list(), list(new_ui)))
  })
  
  output$dynamicUI <- renderUI({
    ui_list <- position_list()
    do.call(tagList, unlist(ui_list, recursive = FALSE))
  })
  
}  