body_tab = dashboardBody(
  
  # use a bit of shinyEffects
  setShadow(class = "dropdown-menu"),
  setShadow(class = "box"),
  
  # some styling
  tags$head(
    tags$style(
      rel = "stylesheet",
      type = "text/css",
      href = "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/styles/qtcreator_dark.min.css"
    ),
    tags$script(
      src = "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/highlight.min.js"
    ),
    tags$script(
      "$(function() {
            $('.sidebar-toggle').on('click', function() {
              $('.skinSelector-widget').toggle();
            });
          });
          "
    )
  ),
  
  # All tabs
  tabItems(
    home_tab,
    dda_plots_tab,
    dda_model_tab,
    fda_plots_tab,
    fda_explor_tab,
    fda_panel_model_tab
  )
  
)
