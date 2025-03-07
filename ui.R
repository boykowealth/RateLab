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
          choices = c("Rate", "Price", "Delta", "Gamma", "Volatility"),
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
        bslib::card_header("Manage Positions",
                           bslib::tooltip(
                             bsicons::bs_icon("question-circle"),
                             shiny::tagList(
                               "Enter Posistions with features",
                               tags$br(),
                               "Shock: Enter integer of Bips - i.e 100 = 0.01 or 1%"),
                             placement = 'right')
                           ),
          shiny::numericInput('numholding', "Number of Positions", 3, min = 1),
          DT::DTOutput('postable'),
        bslib::card_footer(shiny::actionButton('calc', 'Submit Portfolio'))
      ),
      bslib::card(
        bslib::card_header("Taylor Series Expansion",
                           bslib::tooltip(
                             bsicons::bs_icon("question-circle"),
                             "Compare Actual PL vs Risk Attributed PL - Export to Prefered Medium",
                             placement = 'right'
                           )),
        DT::DTOutput('expansion_table')),
      bslib::card(
        bslib::card_header("Portfolio Risk",
                           bslib::tooltip(
                             bsicons::bs_icon("question-circle"),
                             "Visualize Risk Concentrations",
                             placement = 'right'
                           )),
        shiny::plotOutput("port_risk_vis")
      )
      )
    )
  ),
  bslib::nav_panel(
    title = "Documentation",
    bslib::layout_columns(
      col_widths = c(9, 3),
      bslib::card(
        bslib::card_header("About RateLab"),
        shiny::h5("Overview"),
        shiny::p("RateLab is a fixed-income portfolio management tool designed for U.S. Treasury rate traders. 
                 It offers a comprehensive view of market conditions, interest rate co-dynamics, and portfolio 
                 exposure/risk. The application serves as an interactive sandbox for traders to test market shocks 
                 and stress scenarios on their portfolio positions."),
        shiny::h5("Environment"),
        shiny::p("The Environment tab enables users to simulate market conditions under various input parameters. 
                 Users are required to define a model's start and end date, with data availability extending back 
                 to the 1960s in most instances. Subsequently, they are prompted to select the key U.S. Constant 
                 Maturity Rates relevant to their analysis. Additionally, users can specify the data type to base 
                 rolling volatility and time series graphs on. Finally, users have the option to define their rolling 
                 volatility window. The tab visually represents changes in spreads, greeks, and volatility across 
                 the selected rates. Furthermore, the yield curve shift graph illustrates the variations in the 
                 yield curve over the chosen time frame."),
        shiny::p(shiny::HTML("<b>Please Note: inputs are dynamically updated between the Environment and Co-Dynamics pages, 
        this provides users with an efficent and painless experience.</b>")),
        shiny::h5("Co-Dynamics"),
        shiny::p("The Co-Dynamics tab offers users valuable insights into the co-movement of interest rates over time. This 
                 section is specifically designed to help users understand inflation as the fundamental pivot point of all 
                 rates. Users can either adjust inputs carried over from the Environment tab or analyze results under the 
                 same conditions. The tab features two key graphs. The first graph is an animated yield curve with integrated 
                 inflation change tracking. The second graph illustrates the historical relationship between rate changes and 
                 inflation changes at various inflation levels. Together, these tools provide users with a comprehensive view 
                 of how interest rates co-move within specific regimes."),
        shiny::h5("Portfolio"),
        shiny::p("The portfolio tab enables users to calculate profit and loss (P&L) attribution and visualize risk concentrations 
                 within their portfolios. Users can customize their portfolio by selecting the number of positions and inputting 
                 key bond attributes to generate results. These results leverage the principles of the Modern Risk Framework, 
                 utilizing Taylor series expansion. For accurate calculations, users must provide six essential attributes. The 
                 notional represents the principal amount of the bond. The maturity refers to the time remaining until the bond's 
                 repayment date. The coupon denotes the periodic interest payment, expressed as a percentage of the notional. The 
                 compound indicates the frequency at which interest is compounded over time. The yield is the rate of return anticipated 
                 on the bond. Finally, the shock simulates stress or shifts in market conditions for risk assessment.
                 "),
        shiny::p(shiny::HTML("<b>RateLab Provides Traders The Necceasary Tools For Managing Exposure and Risk In The United States Bond Markets</b>"))
      ),
      bslib::card(
        bslib::card_header("Contacts"),
        shiny::h5("Brayden Boyko"),
        shiny::a(href = "mailto:bnboyko@ualberta.ca", "Contact", target = "_blank"),
        shiny::a(href = "https://www.linkedin.com/in/brayden-boyko/", "LinkedIn", target = "_blank"),
        tags$hr(),
        shiny::h5("Mitch Greer"),
        shiny::a(href = "mailto:magreer@ualberta.ca", "Contact", target = "_blank"),
        shiny::a(href = "https://www.linkedin.com/in/mitchgreer-commodities-trendfollowing-energy-trading-quant-auspice/", "LinkedIn", target = "_blank"),
        tags$hr(),
        shiny::h5("Alberta School of Business, Honors Finance"),
        shiny::a(href = "financechair@ualberta.ca", "Contact", target = "_blank"),
        shiny::a(href = "https://sites.ualberta.ca/~avdis/fin_honors/", "Web Page", target = "_blank")
      )
    )
  )
)
