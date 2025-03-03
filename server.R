server <- function(input, output, session) {
  ##bs_themer()
  bslib::toggle_dark_mode() ## would make sense to offer both dark and light mode to the user
  
  
  ## DATE FILTER <START>
  dates <- shiny::reactiveValues(start = Sys.Date() - 1825, end = Sys.Date())
  
  shiny::observeEvent(input$env_date_start, {
    dates$start <- input$env_date_start
    shiny::updateDateInput(session, "co_date_start", value = dates$start)
  })
  
  shiny::observeEvent(input$env_date_end, {
    dates$end <- input$env_date_end
    shiny::updateDateInput(session, "co_date_end", value = dates$end)
  })
  
  shiny::observeEvent(input$co_date_start, {
    dates$start <- input$co_date_start
    shiny::updateDateInput(session, "env_date_start", value = dates$start)
  })
  
  shiny::observeEvent(input$co_date_end, {
    dates$end <- input$co_date_end
    shiny::updateDateInput(session, "env_date_end", value = dates$end)
  })
  ## DATE FILTER <END>
  
}  