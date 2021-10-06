fda_panel_model_tab <- tabItem(
  tabName = "fda_panel_model",
  fluidRow(
    box(
      span(tags$i(h6("Select one or more countries/regions.")), style="color:#d2d5d6"),
      tags$head(tags$style(HTML('
                            #outcome_fda_panel_model+ div>.selectize-dropdown{background: #5b6063; color: #ffffff;}
                            #outcome_fda_panel_model+ div>.selectize-input{background: #5b6063; color: #ffffff;}                            
                                '))),
      # selectizeInput("country", "Select country or region:",  choices = countries_list,
      #                selected = countries_list$`Northern Europe`[c(3,7,8)], multiple = TRUE),
      
      pickerInput("outcome_fda_panel_model", "Select outcome:", 
                  choices =  c("Cases per 100,000",
                               "Deaths per 100,000",
                               "Vaccinated per 100,000"),
                  multiple=FALSE),
      
      
      tags$style(type = "text/css", 
                 ".irs-grid-text:nth-child(-2n+18) {color: white}",
                 ".irs-grid-text:nth-child(2n+20) {color: white}",
                 ".irs-grid-pol:nth-of-type(-n+18) {background:white}",
                 ".irs-grid-pol:nth-of-type(n+18) {background:white}")
      
      
    )
  ),
  
  fluidRow(
    span(tags$i(h3("Simple functional panel model")), style="color:#d2d5d6"),   
    plotlyOutput("plot_fda_panel_model", height = 1000)
  )
  
  
  
)
