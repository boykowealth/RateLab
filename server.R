server <- function(input, output, session) {
  ##bs_themer()
  bslib::toggle_dark_mode() ## would make sense to offer both dark and light mode to the user
  
  ## Dynamic Lists <START>
  dates <- shiny::reactiveValues(start = Sys.Date() - 30, end = Sys.Date())
  assets <- shiny::reactiveValues(series = c("US2Y", "US10Y", "US30Y"))
  measures <- shiny::reactiveValues(type = "Rate")
  ## Dynamic Lists <END>
  
  ## DATE FILTER <START>
  
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

  ## TS MEASURE FILTER <START>
  shiny::observeEvent(input$ts_select, {
    measures$type <- input$ts_select
  })
  ## TS MEASURE FILTER <END>  
  
  ## DATAFRAME FILTER <START>
  
  app_df <- shiny::reactive({
    df <- rates_df %>% 
      dplyr::filter(Date >= dates$start & Date <= dates$end)
    return(df)
  })
  
  ts_df <- shiny::reactive({
    df <- app_df() %>% 
      dplyr::filter(Maturity == assets$series) %>% 
      dplyr::mutate(ts_value = dplyr::case_when(
        measures$type == "Rate" ~ Rate,
        measures$type == "Price" ~ Price,
        measures$type == "Delta" ~ Delta,
        measures$type == "Gamma" ~ Gamma,
        TRUE ~ Rate  # Default case
      ))
    
    return(df)
  })
  
  startEnd_df <- shiny::reactive({
    df <- app_df() %>% 
      dplyr::filter(Date == max(Date) | Date == min(Date)) %>% 
      dplyr::mutate(t2m = round(t2m, 3)) %>% 
      dplyr::arrange(t2m)
  })
  
  spread_df <- shiny::reactive({
    
    ### GENERATE ALL COMBINATIONS (ORDER MATTERS)
    combinations <- expand.grid(assets$series, assets$series) %>%
      dplyr::filter(Var1 != Var2) %>%
      dplyr::mutate(name = paste(Var1, Var2, sep = "_")) 
    
    reverse_combinations <- combinations %>%
      dplyr::rename(Var1 = Var2, Var2 = Var1) %>%
      dplyr::mutate(name = paste(Var1, Var2, sep = "_"))
    
    all_combinations <- dplyr::bind_rows(combinations, reverse_combinations) %>%
      dplyr::distinct()
    
    combination_names <- all_combinations$name
    
    ### CONVERT RATES TO WIDE DF
    df <- startEnd_df() %>%
      dplyr::select(Date, Rate, Maturity) %>%
      tidyr::pivot_wider(names_from = Maturity, values_from = Rate)
    
    ### MAP ALL SPREADS  
    df <- df %>%
      dplyr::bind_cols(
        purrr::map_dfc(seq_len(nrow(all_combinations)), function(i) {
          var1 <- all_combinations$Var1[i]
          var2 <- all_combinations$Var2[i]
          tibble::tibble(
            !!paste0(var1, "_", var2) := df[[var1]] - df[[var2]]
          )
        })
      )
    
    ### ONLY DISPLAY THE COMBINATIONS SELECTED
    available_columns <- names(df)[-1]  # All spread columns after Date
    valid_combinations <- lubridate::intersect(combination_names, available_columns)
    
    df <- df %>%
      dplyr::select(Date, dplyr::all_of(valid_combinations))  # Select only matching columns
    
    return(df)
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
        ggplot2::geom_smooth(se = FALSE) +
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
      
      ggplot2::ggplot(df, ggplot2::aes(x = Date, y = ts_value, color = as.factor(Maturity), group = Maturity)) +
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
  
  ## UI KEY METRICS <START>
    output$spreads <- renderUI({
      df <- spread_df()
      
      combination_columns <- names(df)[-1]
      combination_ui <- lapply(combination_columns, function(col_name) {
        
        first_value <- df[[col_name]][1] * 10000
        second_value <- df[[col_name]][2] * 10000
        rate_diff <- second_value - first_value
        
        arrow_icon <- if (rate_diff > 0) {
          shiny::icon("arrow-up", lib = "glyphicon")
        } else {
          shiny::icon("arrow-down", lib = "glyphicon")
        }
        
        shiny::div(
          shiny::p(shiny::strong(col_name), paste(":", round(second_value, 0), "BPS"), arrow_icon, round(rate_diff, 0))
        )
      })
      
      shiny::tagList(combination_ui)
    })
    
    output$greeks <- renderUI({
      dfD <- startEnd_df() %>%
        dplyr::select(Date, Delta, Maturity) %>%
        dplyr::filter(Maturity == assets$series) %>%
        dplyr::mutate_all(~ ifelse(is.na(.), 0, .)) %>%
        tidyr::pivot_wider(names_from = Maturity, values_from = Delta)
      
      dfG <- startEnd_df() %>%
        dplyr::select(Date, Gamma, Maturity) %>%
        dplyr::filter(Maturity == assets$series) %>%
        dplyr::mutate_all(~ ifelse(is.na(.), 0, .)) %>%
        tidyr::pivot_wider(names_from = Maturity, values_from = Gamma)
      
      combination_ui <- lapply(names(dfD)[-1], function(col_name) {
        d_first <- dfD[[col_name]][1] * 10000
        d_sec <- dfD[[col_name]][2] * 10000
        
        g_first <- dfG[[col_name]][1] * 10000
        g_sec <- dfG[[col_name]][2] * 10000
        
        # Safely calculate differences with NA checks
        d_dif <- ifelse(is.na(d_sec) | is.na(d_first), 0, d_sec - d_first)
        g_dif <- ifelse(is.na(g_sec) | is.na(g_first), 0, g_sec - g_first)
        
        arrow_icon_d <- if (d_dif > 0) {
          shiny::icon("arrow-up", lib = "glyphicon")
        } else {
          shiny::icon("arrow-down", lib = "glyphicon")
        }
        
        arrow_icon_g <- if (g_dif > 0) {
          shiny::icon("arrow-up", lib = "glyphicon")
        } else {
          shiny::icon("arrow-down", lib = "glyphicon")
        }
        
        shiny::p(
          shiny::strong(col_name), 
          paste(": Delta:", round(d_sec, 0), "BPS"), arrow_icon_d, 
          paste(" Gamma:", round(g_sec, 2), "BPS"), arrow_icon_g
        )
      })
      
      shiny::div(combination_ui)
    })
    
    
  ## UI KEY METRICS <END>
    
    
}  