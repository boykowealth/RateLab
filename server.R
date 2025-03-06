server <- function(input, output, session) {
  ##bs_themer()
  bslib::toggle_dark_mode() ## would make sense to offer both dark and light mode to the user
  
  ## DYNAMIC LISTS <START>
  dates <- shiny::reactiveValues(start = Sys.Date() - 30, end = Sys.Date())
  assets <- shiny::reactiveValues(series = c("US2Y", "US10Y", "US30Y"))
  measures <- shiny::reactiveValues(type = "Rate")
  rollWin <- shiny::reactiveValues(num = 5)
  ## DYNAMIC LISTS <END>
  
  
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

  ## VOLATILITY ROLLBACK WINDOW <START>
  shiny::observeEvent(input$roll_num, {
    rollWin$num <- input$roll_num
  })
  ## VOLATILITY ROLLBACK WINDOW <END
  
  ## DATAFRAME FILTER <START>
  
  app_df <- shiny::reactive({
    df <- rates_df %>% 
      dplyr::filter(Date >= dates$start & Date <= dates$end)
    return(df)
  })
  
  ts_df <- shiny::reactive({
    df <- app_df() %>% 
      dplyr::filter(Maturity %in% assets$series) %>% 
      dplyr::mutate(ts_value = dplyr::case_when(
        measures$type == "Rate" ~ Rate,
        measures$type == "Price" ~ Price,
        measures$type == "Delta" ~ Delta,
        measures$type == "Gamma" ~ Gamma,
        TRUE ~ Rate  ## Default case
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
      dplyr::distinct() %>%
      dplyr::mutate(
        Var1_num = as.numeric(stringr::str_extract(Var1, "\\d+")),
        Var2_num = as.numeric(stringr::str_extract(Var2, "\\d+"))
      ) %>%
      filter(Var1_num < Var2_num)
    
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
  
  coDyn_df <- shiny::reactive({
    df <- rates_df %>% 
      dplyr::filter(Maturity %in% assets$series) %>% 
      dplyr::group_by(Maturity) %>% 
      dplyr::mutate(
        RateChg = (Rate / dplyr::lag(Rate)) -1,
        InfChg = (Inflation / dplyr::lag(Inflation)) -1,
        Sensitivity = RateChg / InfChg
      ) %>% 
      dplyr::ungroup() %>% 
      dplyr::filter(InfChg != 0) %>% 
      dplyr::arrange(Inflation)
  })
    
  
  ## DATAFRAME FILTER <END>
  
  ## DT INPUT <START>
  
  values <- reactiveValues(port = NULL)
  
  observeEvent(input$numholding, {
    
    nrows <- input$numholding
    
    nrows <- input$numholding
    notional <- c(1000000, -500000, -500000) #sync Push
    mat <- c(10,2,30)
    M <-  rep(2, nrows)
    C <- rep(0.03, nrows)
    ytm <- rep(0.03, nrows)
    shock <- c(75, 50, 100)
    
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
          notional <- mat <- M <- C <- ytm <- shock <- rep(NA, nrows)
          port <- tibble::tibble(
            Notional = notional,
            Maturity = mat,
            Coupon = C,
            Compound = M,
            Yield = ytm,
            Shock = shock
          )
          DT::datatable(port, rownames = FALSE, editable = TRUE)
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
        
        
        values$ytms <- round(seq(from = 0, to = max(values$port_tmp$Yield, na.rm = TRUE) + 0.03, by = 0.0001), 4)
        
         
        values$f_matrix <- expand.grid(index = 1:nrow(values$port_tmp), ytm = values$ytms) %>% 
          dplyr::left_join(values$port_tmp, by = dplyr::join_by(index)) %>% 
          dplyr::select(-index) %>% 
          as.matrix()
        
        values$bond_prices <- bond_prices(values$f_matrix, 0.0001) 
        
        values$bond_sensitivities <- Sensitivities(values$bond_prices, 0.0001) 
        
        values$bond_data <- values$bond_prices %>% 
          tidyr::as_tibble() %>% 
          dplyr::left_join(., values$bond_sensitivities %>% tidyr::as_tibble(), dplyr::join_by(YTM,T2M))
        
        values$port_prices <- values$bond_data %>% dplyr::group_by(YTM) %>% 
          dplyr::summarise(
            Port_price = sum(Price),
            Port_price_plus = sum(Price_Plus),
            Port_price_minus = sum(Price_Minus)) %>% 
          dplyr::mutate(
            Delta = (Port_price_plus - Port_price_minus) / (2 * 0.0001) / 10000,
            Gamma = (Port_price_plus - 2 * Port_price + Port_price_minus) / (2 * 0.0001)^2 / 10000^2
          )
        
        values$locals <- values$port_tmp %>% dplyr::select(T2M = Maturity, Yield, Notional, Shock) %>%
          dplyr::mutate(
            local_delta = purrr::map2_dbl(.x = T2M, .y = Yield, .f = ~pullsensitivity(values$bond_data, .x, .y, 'Delta')),
            local_gamma = purrr::map2_dbl(.x = T2M, .y = Yield, .f = ~pullsensitivity(values$bond_data, .x, .y, 'Gamma')),
            new_P = purrr::map2_dbl(.x = T2M, .y = round(Yield + Shock, 4), .f = ~pullsensitivity(values$bond_data, .x, .y, 'Price')),
            Actual = new_P - Notional,
            Delta_est = local_delta * (Shock * 10000),
            Gamma_est = local_gamma * Shock^2 * 10000^2,
            Explained = Delta_est + Gamma_est,
            Unexplained = abs(Actual) - abs(Explained)
          )
        
        
        output$expansion_table <- DT::renderDT({
          
          dat <- values$locals %>% 
            dplyr::select(T2M, Yield, Shock, Actual, Delta_est, Gamma_est, Explained, Unexplained)
          
          summary_row <- data.frame(
            T2M = "Portfolio",
            Yield = NA,
            Shock = NA,
            Actual = sum(dat$Actual, na.rm = TRUE),
            Delta_est = sum(dat$Delta_est, na.rm = TRUE),
            Gamma_est = sum(dat$Gamma_est, na.rm = TRUE),
            Explained = sum(dat$Explained, na.rm = TRUE),
            Unexplained = sum(dat$Unexplained, na.rm = TRUE)
          )
          
          dat <- rbind(dat, summary_row)
          
          DT::datatable(
            dat, 
            extensions = 'Buttons', 
            options = list(
              dom = 'Bfrtip',
              buttons = c('csv','pdf', 'print') 
            ),
            colnames = c("Delta Est" = "Delta_est", "Gamma Est" = "Gamma_est", "Actual PL" = "Actual"),
            rownames = F
              ) %>% 
            DT::formatCurrency(., c("Actual PL", "Delta Est", "Gamma Est", "Explained", "Unexplained"))
        })
        
        output$port_risk_vis <- shiny::renderPlot({
          dat <- values$locals %>%
              dplyr::select(T2M, local_delta, local_gamma, Yield) %>% 
              dplyr::left_join(values$bond_data, ., dplyr::join_by(T2M)) %>% 
              dplyr::mutate(
                Delta_Approx = local_delta * (YTM - Yield) * 10000,
                Gamma_Approx = .5 * local_gamma * (YTM - Yield)^2 * 10000^2
            ) %>% 
            dplyr::group_by(YTM) %>% 
            dplyr::summarise(
              Portfolio = sum(Price, na.rm = TRUE),
              `Delta Approx` = sum(Delta_Approx, na.rm = TRUE),
              `Gamma Approx` = sum(Gamma_Approx, na.rm = TRUE)
            ) %>% 
            tidyr::pivot_longer(-YTM, names_to = 'series', values_to = 'value')
          
          zero <- dat %>% dplyr::filter(series == 'Portfolio') %>%  filter(round(value, 0) == 0) %>% dplyr::pull(YTM)
          
          
          dat <- dat %>% dplyr::mutate(change = YTM - zero)
          
          dat %>% ggplot(aes(x = change, y = value, col = series)) + geom_line() +
            ggplot2::scale_y_continuous(labels = scales::dollar, breaks = scales::pretty_breaks(n = 8)) +
            ggplot2::scale_x_continuous(labels = scales::percent) +
            ggplot2::theme(
              panel.background = ggplot2::element_rect(fill = "#222", color = NA),
              plot.background = ggplot2::element_rect(fill = "#222", color = NA),
              panel.grid.major = ggplot2::element_line(color = "#444"),
              panel.grid.minor = ggplot2::element_line(color = "#444"),
              axis.text = ggplot2::element_text(color = "white", size = 15),
              axis.title = ggplot2::element_text(color = "white"),
              legend.background = ggplot2::element_rect(fill = "#222", color = NA),
              legend.text = ggplot2::element_text(color = "white", size = 20),
              legend.title = ggplot2::element_text(color = "white"),
              legend.position = "bottom",
              plot.title = ggplot2::element_text(color = "white", size = 25)
            ) +
            ggplot2::labs(
              title = paste0("PL vs Change in YTM relative to ", zero * 100, "%"),
              xaxis = "Change",
              y = ""
            )
          
          })
      
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
    
    # Sensitivity To Inflation
    output$coDynamic <- shiny::renderPlot({
      df <- coDyn_df()
      df$Inflation <- round(as.numeric(df$Inflation),2)
      
      ggplot2::ggplot(df, ggplot2::aes(x = Inflation, y = Sensitivity, color = as.factor(Maturity), group = Maturity)) +
        ggplot2::geom_smooth(se = FALSE) +
        ggplot2::labs(
          title = "",
          x = "Inflation",
          y = "Sensitivity To Inflation",
          color = "Maturity"
        ) +
        ggplot2::scale_y_continuous(
          labels = scales::percent,
          breaks = seq(-1, 1, by = 0.1)  # Adjust based on expected range of Sensitivity
        ) +
        ggplot2::scale_x_continuous(
          labels = scales::percent,
          breaks = seq(min(df$Inflation, na.rm = TRUE), max(df$Inflation, na.rm = TRUE), by = 0.02)
        ) +
        ggplot2::geom_hline(yintercept = 0, color = "white", size = 0.5) +
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
          tags$span(
            shiny::icon("arrow-up", lib = "glyphicon"),
            style = "color: green; font-size: 18px;"
          )
        } else {
          tags$span(
            shiny::icon("arrow-down", lib = "glyphicon"),
            style = "color: red; font-size: 18px;"
          )
        }
        
        shiny::div(
          shiny::p(shiny::strong(col_name), paste(":", round(second_value, 0), "BPS"), arrow_icon, round(rate_diff, 0), paste("BPS"))
        )
      })
      
      shiny::tagList(combination_ui)
    })
    
    output$greeks <- renderUI({
      dfD <- startEnd_df() %>%
        dplyr::select(Date, Delta, Maturity) %>%
        dplyr::filter(Maturity %in% assets$series) %>%
        tidyr::pivot_wider(id_cols = Date, names_from = Maturity, values_from = Delta)
      
      dfG <- startEnd_df() %>%
        dplyr::select(Date, Gamma, Maturity) %>%
        dplyr::filter(Maturity %in% assets$series) %>%
        tidyr::pivot_wider(id_cols = Date, names_from = Maturity, values_from = Gamma)
      
      combination_ui <- lapply(names(dfD)[-1], function(col_name) {
        d_first <- dfD[[col_name]][1] * 10000
        d_sec <- dfD[[col_name]][2] * 10000
        
        g_first <- dfG[[col_name]][1] * 10000
        g_sec <- dfG[[col_name]][2] * 10000
        
        d_dif <- ifelse(is.na(d_sec) | is.na(d_first), 0, ((d_sec / d_first) - 1) * 100)
        g_dif <- ifelse(is.na(g_sec) | is.na(g_first), 0, ((g_sec / g_first) -1) * 100)
        
        arrow_icon_d <- if (d_dif > 0) {
          tags$span(
            shiny::icon("arrow-up", lib = "glyphicon"),
            style = "color: green; font-size: 18px;"
          )
        } else {
          tags$span(
            shiny::icon("arrow-down", lib = "glyphicon"),
            style = "color: red; font-size: 18px;"
          )
        }
        
        arrow_icon_g <- if (g_dif > 0) {
          tags$span(
            shiny::icon("arrow-up", lib = "glyphicon"),
            style = "color: green; font-size: 18px;"
          )
        } else {
          tags$span(
            shiny::icon("arrow-down", lib = "glyphicon"),
            style = "color: red; font-size: 18px;"
          )
        }
        
        shiny::p(
          shiny::strong(col_name), 
          paste(": Delta:", round(d_dif, 2), "%"), arrow_icon_d, 
          paste(" Gamma:", round(g_dif, 2), "%"), arrow_icon_g
        )
      })
      
      shiny::div(combination_ui)
    })
    
    
    output$vols <- renderUI({
      df <- ts_df() %>% 
        dplyr::arrange(Date) %>%
        dplyr::group_by(Maturity) %>%
        dplyr::mutate(Chg = (ts_value / dplyr::lag(ts_value)) -1,
                      Vol = zoo::rollapply(
                        Chg,
                        width = rollWin$num,
                        FUN = sd,
                        align = "right",
                        fill = NA,
                        na.rm = TRUE   
                      ) * sqrt(t2m)
                      ) %>%
        tidyr::drop_na() %>% 
        dplyr::select(Date, Maturity, Vol) %>%
        dplyr::ungroup() %>% 
        tidyr::pivot_wider(id_cols = Date, names_from = Maturity, values_from = Vol)
      
      combination_ui <- lapply(names(df)[-1], function(col_name) {
        
        exvol <- df[[col_name]][nrow(df)] * 100
        prevol <- df[[col_name]][nrow(df)-rollWin$num] * 100
        
        vol_dif <- (exvol - prevol)
        
        arrow_icon_vol <- if (vol_dif > 0) {
          tags$span(
            shiny::icon("arrow-up", lib = "glyphicon"),
            style = "color: green; font-size: 18px;"
          )
        } else {
          tags$span(
            shiny::icon("arrow-down", lib = "glyphicon"),
            style = "color: red; font-size: 18px;"
          )
        }
        
    shiny::p(shiny::strong(col_name), paste(":", round(exvol, 2), "%"), arrow_icon_vol, round(vol_dif, 2), paste("%"))
      })
      
      shiny::div(combination_ui)
    })
  ## UI KEY METRICS <END>
    output$yield_dynamic <- plotly::renderPlotly({
      
      
      base <- app_df() %>% 
        tidyr::drop_na() %>% 
        ggplot(aes(x = as.factor(round(t2m, 3)), y = Rate, col = Inflation, group = Date, frame = Date)) +
        ggplot2::geom_line() +
        ggplot2::labs(
          title = "",
          x = "Maturity (Years)",
          y = "",
          color = "Inflation"
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
          legend.position = "right"
        )
      
      base <- plotly::ggplotly(base)
      
      base %>%
        plotly::animation_opts(easing = 'linear', redraw = FALSE) %>%
        plotly::animation_button(x = 1, xanchor = "right", y = 0, yanchor = "bottom") %>%
        plotly::animation_slider(currentvalue = list(prefix = "Date: ", font = list(color = 'white')))
      
    }
    )
 }  