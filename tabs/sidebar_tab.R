sidebar_tab = dashboardSidebar(
  id = "tabs",
  
  sidebarMenu(
    
    menuItem(
      text = "Home", 
      tabName = "home",
      icon = icon("home")
    ),
    
    menuItem(
      text = "Discrete data analysis", 
      tabName = "dda",
      # badgeLabl = "new", 
      badgeColor = "green",
      icon = icon("chart-bar"),
      menuSubItem("Plots", tabName = "dda_plots"),
      menuSubItem("Model", tabName = "dda_model")
      
    ),
    
    menuItem(
      text = "Functional data analysis", 
      tabName = "fda",
      # badgeLabel = "new", 
      badgeColor = "green",
      icon = icon("chart-line"),
      menuSubItem("Plots", tabName = "fda_plots"),
      menuSubItem("Exploratory and fPC analysis", tabName = "fda_explor"),
      menuSubItem("Simple panel model", tabName = "fda_panel_model")
    )
  ),
  
  hr()
  #skinSelector()
)