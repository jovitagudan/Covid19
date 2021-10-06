dda_model_tab <- tabItem(
  tabName = "dda_model",
  
  fluidRow(
           h1("   Automatic parametric model"),
           
           box(
             # selectInput("country2", "Select country or region:",  choices = countries_list),
             # 
             # selectInput("outcome2", "Outcome:", 
             #             choices =  c("Cases (total)","Cases per 100,000")),
             # 
             # selectInput("scale2", "Scale:", 
             #             choices =  c("Original","Natural logarithm"))
             
             
             pickerInput("level_select2", "Select level:",   
                         choices = c("World", "Continent", "Country", "US state"), 
                         selected = c("Country"),
                         multiple = FALSE),
             
             pickerInput("region_select2", "Select country or region:",   
                         choices = countries_list, 
                         options = list(`none-selected-text` = "Please make a selection!", `live-search`=TRUE),
                         selected =  countries_list$`Northern Europe`[which(countries_list$`Northern Europe` %in% c("Lithuania"))],
                         multiple = FALSE),
             
             
             pickerInput("outcome2", "Select outcome:", 
                         choices =  c("Cases per 100,000",
                                      "Deaths per 100,000",
                                      "Vaccinated per 100,000"),
                         multiple=FALSE),
             
             pickerInput("scale2", "Select scale:", 
                         choices =  c("Original", 
                                      "Natural logarithm"),
                         multiple=FALSE)
             
             

             
           )
  ),
  
  fluidRow(
    plotlyOutput("plot2", height = 500)
  )
)




