header_tab = dashboardHeader(
  fixed = TRUE,
  title = tagList(
    span(class = "logo-lg", "Analysis of Covid-19"), 
    
    
    leftUI = tagList(
      dropdownBlock(
        id = "dropdown1",
        title = "Dropdown 1",
        icon = icon("sliders")
      ),
      
      dropdownBlock(
        id = "dropdown2",
        title = "Dropdown 2",
        icon = icon("sliders")
      )
      
    )
  ),
  
  
  userOutput("user")
)