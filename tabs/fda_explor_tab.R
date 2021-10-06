fda_explor_tab <- tabItem(
  tabName = "fda_explor",
  fluidRow(
    box(
      span(tags$i(h6("Select region.")), style="color:#d2d5d6"),
      tags$head(tags$style(HTML('
                            #country+ div>.selectize-dropdown{background: #5b6063; color: #ffffff;}
                            #country+ div>.selectize-input{background: #5b6063; color: #ffffff;}
                            #outcome+ div>.selectize-dropdown{background: #5b6063; color: #ffffff;}
                            #outcome+ div>.selectize-input{background: #5b6063; color: #ffffff;}                            
                            #scale+ div>.selectize-dropdown{background: #5b6063; color: #ffffff;}
                            #scale+ div>.selectize-input{background: #5b6063; color: #ffffff;}                            
                                '))),
      # selectizeInput("country", "Select country or region:",  choices = countries_list,
      #                selected = countries_list$`Northern Europe`[c(3,7,8)], multiple = TRUE),
      
      pickerInput("level_select_fda_explor", "Select level:",   
                  choices = c("Continent", "US state"), 
                  selected = c("Continent"),
                  multiple = FALSE),
      
      pickerInput("region_select_fda_explor", "Select region:",   
                  choices = names(countries_list), 
                  options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!", `live-search`=TRUE),
                  selected = names(countries_list)[11],
                  multiple = FALSE),
      
      
      pickerInput("outcome_fda_explor", "Select outcome:", 
                  choices =  c("Cases per 100,000",
                               "Deaths per 100,000",
                               "Vaccinated per 100,000"),
                  multiple=FALSE),
      
      tags$style(type = "text/css", 
                 ".irs-grid-text:nth-child(-2n+18) {color: white}",
                 ".irs-grid-text:nth-child(2n+20) {color: white}",
                 ".irs-grid-pol:nth-of-type(-n+18) {background:white}",
                 ".irs-grid-pol:nth-of-type(n+18) {background:white}")
      
      
    ),
    
    box(
      plotlyOutput("plot_fda_explor", height = 500)
    )
    
  ),
  
  fluidRow(
    box(
      plotlyOutput("plot_fda_fPCA_harm", height = 500)
    ),
    box(
      plotlyOutput("plot_fda_fPCA_harmD1", height = 500)
    )
  ),
  
  fluidRow(
    box(
      plotlyOutput("plot_fda_fPCA_harmD2", height = 500)
    ),
    box(
      plotlyOutput("plot_fda_fPCA_harmD3", height = 500)
    )
  ),
  
  fluidRow(
    column(width = 12,
    box(
      plotlyOutput("plot_fda_fPCA_scores", height = 600)
    )
    )
  )
  
  
  
  
)
