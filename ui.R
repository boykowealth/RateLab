ui <- bslib::page_navbar(
  
  title = "RateLab",
  id = "main_navbar",
  theme = bslib::bs_theme(bootswatch = "yeti"),
  
  bslib::nav_panel(
    title = "Enviorment",
    bslib::layout_columns(
        col_widths = c(2, 10),
        bslib::card(
          bslib::card_header("Inputs")
        ),
        bslib::card(
          bslib::card_header("Market Enviorment"),
          
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
              bslib::card_header("Macro-environment")
            )
          ),
          
          ## Middle Layer - Yield Curve and Spreads
          bslib::layout_columns(
            col_widths = c(6, 6),
            bslib::card(
              bslib::card_header("Yield Curve vs. Historical")
            ),
            bslib::card(
              bslib::card_header("Benchmark Spreads")
            )
          ),
          bslib::card(
            bslib::card_header("Time Series Of Rates")
          )
        )
    )
  ),
  bslib::nav_panel(
    title = "Co-Dynamics"
  ),
  bslib::nav_panel(
    title = "Portfolio",
    bslib::layout_columns(
      col_widths = c(2, 5, 5),
      bslib::card(
        bslib::card_header("Manage Positions"),
        shiny::uiOutput("dynamicUI"),
        shiny::actionButton("add", "Add Position")
      ),
      bslib::card(
        bslib::card_header("Portfolio Exposure")
      ),
      bslib::card(
        bslib::card_header("Portfolio Risk")
      )
    )
  ),
  bslib::nav_panel(
    title = "Documentation",
    bslib::layout_columns(
      bslib::card(
        bslib::card_header("About RateLab")
      )
    )
  )
  
)