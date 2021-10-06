fda_plots_tab <- tabItem(
  tabName = "fda_plots",
  fluidRow(
    box(
      span(tags$i(h6("Select one or more countries/regions.")), style="color:#d2d5d6"),
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
      
      pickerInput("level_select_fda", "Select level:",   
                  choices = c("World", "Continent", "Country", "US state"), 
                  selected = c("Country"),
                  multiple = FALSE),
      
      pickerInput("region_select_fda", "Select country or region:",   
                  choices = countries_list, 
                  options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!", `live-search`=TRUE),
                  selected =  countries_list$`Northern Europe`[which(countries_list$`Northern Europe` %in% c("Lithuania", "Latvia", "Estonia"))],
                  multiple = TRUE),
      
      
      pickerInput("outcome_fda", "Select outcome:", 
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
           span(tags$i(h3("Level")), style="color:#d2d5d6"),   
      plotlyOutput("plot_fda_level", height = 500)
    )
    
    
  ),
  
  
  fluidRow(
    box(
           span(tags$i(h3("First derivative")), style="color:#d2d5d6"), 
      plotlyOutput("plot_fda_first_deriv", height = 500)
    ),
    box(
           span(tags$i(h3("Second derivative")), style="color:#d2d5d6"), 
           plotlyOutput("plot_fda_second_deriv", height = 500)
    )
  )
  
)
