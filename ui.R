ui <- bslib::page_navbar(
  
  title = "RateLab",
  id = "main_navbar",
  theme = bslib::bs_theme(bootswatch = "yeti"),
  
  bslib::nav_panel(
    title = "Portfolio",
    bslib::layout_columns(
      bslib::card(
        bslib::card_header("Filters")
      )
    )
  ),
  
  bslib::nav_panel(
    title = "Exposure",
    bslib::layout_columns(
      bslib::card(
        bslib::card_header("Filters")
      )
    )
  ), 
  
  
  bslib::nav_panel(
    title = "Risk",
    bslib::layout_columns(
      bslib::card(
        bslib::card_header("Filters")
      )
    )
  ), 
  
  bslib::nav_panel(
    title = "Trading",
    bslib::layout_columns(
      bslib::card(
        bslib::card_header("Filters")
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