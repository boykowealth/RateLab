ui <- bslib::page_navbar(
  
  title = "RateLab",
  id = "main_navbar",
  theme = bslib::bs_theme(bootswatch = "zephyr"),
  
  bslib::nav_panel(
    title = "Test",
    bslib::layout_columns(
      bslib::card(
        bslib::card_header("Filters")
      )
    )
  )
)