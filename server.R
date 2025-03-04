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
    shiny::updateDateInput(session, "co_asset_select", value = assets$series)
  })
  
  shiny::observeEvent(input$co_asset_select, {
    assets$series <- input$co_asset_select
    shiny::updateDateInput(session, "env_asset_select", value = assets$series)
  })
  ## ASSET FILTER <END>  
  
  ## DATAFRAME FILTER <START>
  
  app_df <- shiny::reactive({
    df <- rates_df %>% 
      dplyr::filter(Date >= dates$start & Date <= dates$end)
    return(df)
  })
  
  ts_df <- shiny::reactive({
    df <- app_df() %>% 
      dplyr::filter(Maturity == assets$series)
    return(df)
  })
  
  startEnd_df <- shiny::reactive({
    df <- app_df() %>% 
      dplyr::filter(Date == max(Date) | Date == min(Date)) %>% 
      dplyr::mutate(t2m = round(t2m, 3)) %>% 
      dplyr::arrange(t2m)
  })
  
  ## DATAFRAME FILTER <END>
  
  ## DT INPUT <START>
  
  values <- reactiveValues(port = NULL)
  
  observeEvent(input$numholding, {
    
    nrows <- input$numholding
    
    notional <- mat <- M <-  C <- ytm <- shock <- rep(NA, nrows)
    
    values$port <- tibble::tibble(
      Notional = notional,
      Maturity = mat,
      Coupon = C,
      Compound = M,
      Yield = ytm,
      Shock = shock
    )
  })

  output$postable <- DT::renderDataTable({
   
        if (!is.null(values$port)) {
          DT::datatable(values$port, editable = TRUE)
        } else {
          
          nrows <- input$numholding
          notional <- mat <- M <-  C <- ytm <- shock <- rep(NA, nrows)
          port <- tibble::tibble(
            Notional = notional,
            Maturity = mat,
            Coupon = C,
            Compound = M,
            Yield = ytm,
            Shock = shock
          )
          DT::datatable(port, editable = TRUE)
        }
      })
      
     
      proxy <- DT::dataTableProxy('postable')
      
     
      observeEvent(input$postable_cell_edit, {
        info <- input$postable_cell_edit
        values$port <- DT::editData(values$port, info)

      })
      
      observeEvent(input$calc,{
        req(values$port)

        values$port_tmp <- values$port %>%
          dplyr::mutate(Notional = as.numeric(Notional),
                        Maturity = as.numeric(Maturity),
                        Compound = as.numeric(Compound), 
                        Coupon = as.numeric(Coupon),
                        Yield = as.numeric(Yield),
                        Shock = as.numeric(Shock) / 10000,
                        index = dplyr::row_number())
        
        
        values$ytms <- seq(from = 0, to = max(values$port_tmp$Yield, na.rm = TRUE) + 0.03, by = 0.0001)
        
         
        values$f_matrix <- expand.grid(index = 1:nrow(values$port_tmp), ytm = values$ytms) %>% 
          dplyr::left_join(values$port_tmp, by = dplyr::join_by(index)) %>% 
          dplyr::select(-index) %>% 
          as.matrix()
        
        values$bond_prices <- bond_prices(values$f_matrix, 0.0001) 
        
        values$bond_sensitivities <- Sensitivities(values$bond_prices, 0.0001) 
        
        values$bond_data <- values$bond_prices %>% 
          tidyr::as_tibble() %>% 
          dplyr::left_join(., values$bond_sensitivities %>% tidyr::as_tibble(), dplyr::join_by(YTM,T2M))
        
        print(values$bond_data)
      
      })
    
  ## DT INPUT <END>
  
  
  ## CHARTS & VISUALS <START>
  
      # Yield Curve
    output$yield_curve <- shiny::renderPlot({
      df <- startEnd_df()
      
      ggplot2::ggplot(df, ggplot2::aes(x = as.factor(t2m), y = Rate, color = as.factor(Date), group = Date)) +
        ggplot2::geom_line() +
        ggplot2::labs(
          title = "",
          x = "Maturity (Years)",
          y = "",
          color = "Date"
        ) +
        ggplot2::scale_y_continuous(labels = scales::percent) +
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
    
      # Time Series Chart
    output$ts_rates <- shiny::renderPlot({
      df <- ts_df()
      
      time_diff <- as.numeric(difftime(dates$end, dates$start, units = "days"))
      
      if (time_diff <= 30) {
        breaks <- "1 day"
        format <- "%d-%b"    
      } else if (time_diff <= 365) {
        breaks <- "1 month"
        format <- "%b %Y"   
      } else if (time_diff <= 5 * 365) {
        breaks <- "6 months"
        format <- "%b %Y"    
      } else {
        breaks <- "1 year"
        format <- "%Y"       
      }
      
      ggplot2::ggplot(df, ggplot2::aes(x = Date, y = Rate, color = as.factor(Maturity), group = Maturity)) +
        ggplot2::geom_line() +
        ggplot2::labs(
          title = "",
          x = "",
          y = "",
          color = "Instrument"
        ) +
        ggplot2::scale_x_date(date_breaks = breaks, date_labels = format) +
        ggplot2::scale_y_continuous(labels = scales::percent) +
        ggplot2::theme(
          panel.background = ggplot2::element_rect(fill = "#222", color = NA),
          plot.background = ggplot2::element_rect(fill = "#222", color = NA),
          panel.grid.major = ggplot2::element_line(color = "#444"),
          panel.grid.minor = ggplot2::element_line(color = "#444"),
          axis.text = ggplot2::element_text(color = "white", size = 10),
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
          axis.title = ggplot2::element_text(color = "white"),
          legend.background = ggplot2::element_rect(fill = "#222", color = NA),
          legend.text = ggplot2::element_text(color = "white", size = 10),
          legend.title = ggplot2::element_text(color = "white", size = 10),
          
          legend.position = "right"
        )
    })
  ## CHARTS & VISUALS <END>
  
  
}  