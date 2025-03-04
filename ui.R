ui <- bslib::page_navbar(
  title = "RateLab",
  id = "main_navbar",
  theme = bslib::bs_theme(bootswatch = "yeti"),
  
  bslib::nav_panel(
    title = "Environment",
    bslib::layout_columns(
      col_widths = c(2, 10),
      bslib::card(
        bslib::card_header("Inputs"),
        
        ## Start Date
        shiny::dateInput(inputId = "env_date_start", 
                         label = "Start Date", 
                         value = start_date),
        ## End Date
        shiny::dateInput(inputId = "env_date_end", 
                         label = "End Date", 
                         value = end_date),
        ## Series Select
        shiny::selectInput(
          inputId = "env_asset_select",
          label = "Rates:",
          choices = c("US1M", "US3M", "US6M", "US1Y", "US2Y", "US5Y", "US7Y", "US10Y", "US20Y", "US30Y"),
          selected = c("US2Y", "US10Y", "US30Y"),
          multiple = TRUE,
          selectize = TRUE
        ),
        ## TS Visual Select
        shiny::selectInput(
          inputId = "ts_select",
          label = "Time Series Measure",
          choices = c("Rate", "Price", "Delta", "Gamma"),
          selected = "Rates",
          multiple = FALSE,
          selectize = FALSE
        )
        
      ),
      bslib::card(
        
        ## Top Layer - Key Metrics
        bslib::layout_columns(
          col_widths = c(4, 4, 4),
          bslib::card(
            bslib::card_header("Spread Movements")
          ),
          bslib::card(
            bslib::card_header("Greeks")
          ),
          bslib::card(
            bslib::card_header("Volatility")
          )
        ),
        
        ## Middle Layer - Yield Curve and Spreads
        bslib::layout_columns(
          col_widths = c(6, 6),
          bslib::card(
            bslib::card_header("Yield Curve Shift"),
            shiny::plotOutput("yield_curve")
          ),
          bslib::card(
            bslib::card_header("Time Series"),
            ##plotly::plotlyOutput("ts_rates")
            shiny::plotOutput("ts_rates")
          )
        ),
        
      )
    )
  ),
  bslib::nav_panel(
    title = "Co-Dynamics",
    bslib::layout_columns(
      col_widths = c(2, 10),
      bslib::card(
        bslib::card_header("Inputs"),
        
        ## Start Date
        shiny::dateInput(inputId = "co_date_start", 
                         label = "Start Date", 
                         value = start_date),
        ## End Date
        shiny::dateInput(inputId = "co_date_end", 
                         label = "End Date", 
                         value = end_date),
        ## Series Select
        shiny::selectInput(
          inputId = "co_asset_select",
          label = "Rates:",
          choices = c("US1M", "US3M", "US6M", "US1Y", "US2Y", "US5Y", "US7Y", "US10Y", "US20Y", "US30Y"),
          selected = c("US2Y", "US10Y", "US30Y"),
          multiple = TRUE,
          selectize = TRUE
        )
      ),
    bslib::card(
      
      ## Top Layer - Charts
      bslib::layout_columns(
        col_widths = c(6, 6),
        bslib::card(
        ),
        bslib::card(
        )
      ),
      
      ## Middle Layer - Heatmap / Matrix
      bslib::layout_columns(
        col_widths = c(6, 6),
        bslib::card(
          bslib::card_header("Delta")
        ),
        bslib::card(
          bslib::card_header("Gamma")
        )
      )
    )
    )
  ),
  bslib::nav_panel(
    title = "Portfolio",
    bslib::card(
    bslib::layout_columns(
      col_widths = c(5, 7, 12, 12),
      bslib::card(
        bslib::card_header("Manage Positions"),
        shiny::numericInput('numholding', "Number of Posistions", 3, min = 1),
        DT::DTOutput('postable'),
        shiny::actionButton('calc', 'Submit Portfolio')
      ),
      bslib::card(
        bslib::card_header("Taylor Series Expansion")
      ),
      bslib::card(
        bslib::card_header("Portfolio Exposure")
      ),
      bslib::card(
        bslib::card_header("Portfolio Risk")
      )
      )
    )
  ),
  bslib::nav_panel(
    title = "Documentation",
    bslib::layout_columns(
      bslib::card(
        bslib::card_header("About RateLab")
      ),
      bslib::card(
        bslib::card_header("Contacts")
      )
    )
  )
)
