server <- function(input, output, session) {
  ##bs_themer()
  bslib::toggle_dark_mode() ## would make sense to offer both dark and light mode to the user
  
  ## DATE FILTER <START>
  dates <- shiny::reactiveValues(start = Sys.Date() - 30, end = Sys.Date())
  
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
  
  ## DATAFRAME FILTER <START>
  
  app_df <- shiny::reactive({
    df <- rates_df %>% 
      dplyr::filter(Date >= dates$start & Date <= dates$end)
    return(df)
  })
  
  ## DATAFRAME FILTER <END>
  
  
  ## CHARTS & VISUALS <START>
  
      # Yield Curve
    output$yield_curve <- shiny::renderPlot({
      df <- app_df() %>% 
        dplyr::filter(Date == max(Date) | Date == min(Date)) %>% 
        dplyr::arrange(t2m)
      
      ggplot2::ggplot(df, ggplot2::aes(x = as.factor(t2m), y = Rate, color = as.factor(Date), group = Date)) +
        ggplot2::geom_line() +
        ggplot2::labs(
          title = "",
          x = "Maturity",
          y = "Rate",
          color = "Date"
        )
    })
    
    
  
      # Time Series of Rates (Plotly)
    #output$ts_rates <- plotly::renderPlotly({
      #df <- app_df()
      
      #gg <- ggplot2::ggplot(df, ggplot2::aes(x = as.factor(Date), y = Rate, color = as.factor(Maturity), group = Maturity)) +
        #ggplot2::geom_line() +
        #ggplot2::labs(
          #title = "",
          #x = "Maturity",
          #y = "Rate",
          #color = "Date"
        #) 
      
      #plotly::ggplotly(gg)
    #})
    
    output$ts_rates <- shiny::renderPlot({
      df <- app_df()
      
      ggplot2::ggplot(df, ggplot2::aes(x = as.factor(Date), y = Rate, color = as.factor(Maturity), group = Maturity)) +
        ggplot2::geom_line() +
        ggplot2::labs(
          title = "",
          x = "Maturity",
          y = "Rate",
          color = "Date"
        )
    })
    
  ## CHARTS & VISUALS <END>
  
  
}  