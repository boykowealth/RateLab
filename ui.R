ui <- bslib::page_navbar(
  
  title = "RateLab",
  id = "main_navbar",
  theme = bslib::bs_theme(bootswatch = "yeti"),
  
  bslib::nav_panel(
    title = "Enviorment",
    bslib::layout_columns(
      bslib::card(
        bslib::card_header("Filters")
      )
    )
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