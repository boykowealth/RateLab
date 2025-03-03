server <- function(input, output, session) {
  ##bs_themer()
  bslib::toggle_dark_mode() ## would make sense to offer both dark and light mode to the user
  
  ## DATE FILTER <START>
  dates <- shiny::reactiveValues(start = Sys.Date() - 30, end = Sys.Date())
  assets <- shiny::reactiveValues(series = c("US2Y", "US10Y", "US30Y"))
  
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
  
  ## ASSET FILTER <START>
  shiny::observeEvent(input$env_asset_select, {
    assets$series <- input$env_asset_select
    shiny::updateSelectInput(session, "co_asset_select", selected = assets$series)
  })
  
  shiny::observeEvent(input$co_asset_select, {
    assets$series <- input$co_asset_select
    shiny::updateSelectInput(session, "env_asset_select", selected = assets$series)
  })
  ## ASSET FILTER <END>
  
  ## DATAFRAME FILTER <START>
    # Main DataFrame (Reacts To Date Inputs - All Other DFs Should Be Based On This One)
  app_df <- shiny::reactive({
    df <- rates_df %>% 
      dplyr::filter(Date >= dates$start & Date <= dates$end)
    return(df)
  })
  
    # Time Series DataFrame (Used in Time Series Graphs)
  ts_df <- shiny::reactive({
    df <- app_df() %>% 
      dplyr::filter(Maturity == assets$series)
    return(df)
  })
  
    # DataFrame for Start and End Dates (Used In Yield Curve, Spread, Greeks, Volatility)
  firstLast_df <- shiny::reactive({
    df <- app_df() %>% 
    dplyr::filter(Date == max(Date) | Date == min(Date)) %>% 
    dplyr::arrange(t2m)
    return(df)
  })
  ## DATAFRAME FILTER <END>
  
  ## CHARTS & VISUALS <START>
      # Yield Curve
    output$yield_curve <- shiny::renderPlot({
      df <- firstLast_df()
      
      ggplot2::ggplot(df, ggplot2::aes(x = as.factor(t2m), y = Rate, color = as.factor(Date), group = Date)) +
        ggplot2::geom_line() +
        ggplot2::labs(
          title = "",
          x = "Maturity",
          y = "Rate",
          color = "Date"
        ) +
        ggplot2::theme(
          panel.background = ggplot2::element_rect(fill = "#222", color = NA),
          plot.background = ggplot2::element_rect(fill = "#222", color = NA),
          panel.grid.major = ggplot2::element_line(color = "#444"),
          panel.grid.minor = ggplot2::element_line(color = "#444"),
          axis.text = ggplot2::element_text(color = "white"),
          axis.title = ggplot2::element_text(color = "white"),
          legend.background = ggplot2::element_rect(fill = "#222", color = NA),
          legend.text = ggplot2::element_text(color = "white"),
          legend.title = ggplot2::element_text(color = "white"),
          legend.position = "bottom"
        )
    })
    
      # Time-Series of Rates
    output$ts_rates <- shiny::renderPlot({
      df <- ts_df()
      
      ggplot2::ggplot(df, ggplot2::aes(x = Date, y = Rate, color = as.factor(Maturity), group = Maturity)) +
        ggplot2::geom_line() +
        ggplot2::labs(
          title = "",
          x = "Maturity",
          y = "Rate",
          color = "Date"
        )+
        ggplot2::theme(
          panel.background = ggplot2::element_rect(fill = "#222", color = NA),
          plot.background = ggplot2::element_rect(fill = "#222", color = NA),
          panel.grid.major = ggplot2::element_line(color = "#444"),
          panel.grid.minor = ggplot2::element_line(color = "#444"),
          axis.text = ggplot2::element_text(color = "white"),
          axis.title = ggplot2::element_text(color = "white"),
          legend.background = ggplot2::element_rect(fill = "#222", color = NA),
          legend.text = ggplot2::element_text(color = "white"),
          legend.title = ggplot2::element_text(color = "white"),
          legend.position = "right"
        )
    })
    
  ## CHARTS & VISUALS <END>
  
  
}  