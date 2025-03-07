ui <- bslib::page_navbar(
  title = "RateLab",
  id = "main_navbar",
  theme = bslib::bs_theme(bootswatch = "yeti"),
  
  bslib::nav_panel(
    title = "Environment",
    id = "env",
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
        ),
        
        shiny::numericInput(
          inputId = 'roll_num',
          label = 'Volatility Window',
          value = 5,
          min = 2,
          step = 1
        )
        
      ),
      bslib::card(
        
        ## Top Layer - Key Metrics
        bslib::layout_columns(
          col_widths = c(4, 4, 4),
          bslib::card(
            bslib::card_header("Spread Movements",
                               bslib::tooltip(
                                 bsicons::bs_icon("question-circle"),
                                 shiny::tagList(
                                 "First Figure: Current Spread",
                                 tags$br(),
                                 "Arrow Figure: Change over Period"),
                                 placement = 'right'
                               )),
            shiny::uiOutput("spreads")
          ),
          bslib::card(
            bslib::card_header("Greeks",
                               bslib::tooltip(
                                  bsicons::bs_icon("question-circle"),
                                  "Change Over Period",
                                  placement = 'right'
            )),
            shiny::uiOutput("greeks")
          ),
          bslib::card(
            bslib::card_header("Volatility",
                               bslib::tooltip(
                                 bsicons::bs_icon("question-circle"),
                                 "Rolling Volatility of Selected Time Series Measure",
                                 placement = 'right'
                               )),
            shiny::uiOutput("vols")
          )
        ),
        
        ## Middle Layer - Yield Curve and Spreads
        bslib::layout_columns(
          col_widths = c(6, 6),
          bslib::card(
            bslib::card_header("Yield Curve Shift",
                               bslib::tooltip(
                                 bsicons::bs_icon("question-circle"),
                                 "Shift from Period Start to End",
                                 placement = 'right'
                               )),
            shiny::plotOutput("yield_curve")
          ),
          bslib::card(
            bslib::card_header("Time Series",
                               bslib::tooltip(
                                 bsicons::bs_icon("question-circle"),
                                 "Facet Metric Using Input Card",
                                 placement = 'right'
                               )),
            ##plotly::plotlyOutput("ts_rates")
            shiny::plotOutput("ts_rates")
          )
        ),
        
      )
    )
  ),
  bslib::nav_panel(
    title = "Co-Dynamics",
    id = "co",
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
        ),
        
      ),
    bslib::card(
      
      ## Top Layer - Charts
      bslib::layout_columns(
        col_widths = c(6, 6),
        bslib::card(
          bslib::card_header("Yield Curve Dynamics",
                             bslib::tooltip(
                               bsicons::bs_icon("question-circle"),
                               "Play for Daily Change",
                               placement = 'right'
                             )),
          plotly::plotlyOutput('yield_dynamic')
        ),
        bslib::card(
          bslib::card_header("Rate Sensitivity To Inflation"),
          shiny::plotOutput("coDynamic")
        )
      )
    )
    )
  ),
  bslib::nav_panel(
    title = "Portfolio",
    bslib::card(
    bslib::layout_columns(
      col_widths = c(6, 6, 12),
      bslib::card(
        bslib::card_header("Manage Positions"),
        shiny::numericInput('numholding', "Number of Posistions", 3, min = 1), #sync Push
        DT::DTOutput('postable'),
        shiny::actionButton('calc', 'Submit Portfolio')
      ),
      bslib::card(
        bslib::card_header("Taylor Series Expansion"),
        DT::DTOutput('expansion_table')
      ),
      bslib::card(
        bslib::card_header("Portfolio Risk"),
        shiny::plotOutput("port_risk_vis")
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
