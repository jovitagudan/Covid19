

country_reactive_db = function(){
  reactive({
    if (input$level_select=="World" & input$outcome=="Cases (total)") { 
      db = cases_world
    }
    if (input$level_select=="Continent" & input$outcome=="Cases (total)") { 
      db = cases_continents 
    }
    if (input$level_select=="Country" & input$outcome=="Cases (total)") { 
      db = cases_countries
    }
    if (input$level_select=="US state" & input$outcome=="Cases (total)") { 
      db = cases_US_states
    }
    
    if (input$level_select=="World" & input$outcome=="Cases per 100,000") { 
      db = cases_norm_world
    }
    if (input$level_select=="Continent" & input$outcome=="Cases per 100,000") { 
      db = cases_norm_continents 
    }
    if (input$level_select=="Country" & input$outcome=="Cases per 100,000") { 
      db = cases_norm_countries
    }
    if (input$level_select=="US state" & input$outcome=="Cases per 100,000") { 
      db = cases_norm_US_states
    }
    
    
    if (input$level_select=="World" & input$outcome=="Deaths (total)") { 
      db = deaths_world
    }
    if (input$level_select=="Continent" & input$outcome=="Deaths (total)") { 
      db = deaths_continents 
    }
    if (input$level_select=="Country" & input$outcome=="Deaths (total)") { 
      db = deaths_countries
    }
    if (input$level_select=="US state" & input$outcome=="Deaths (total)") { 
      db = deaths_US_states
    }
    
    if (input$level_select=="World" & input$outcome=="Deaths per 100,000") { 
      db = deaths_norm_world
    }
    if (input$level_select=="Continent" & input$outcome=="Deaths per 100,000") { 
      db = deaths_norm_continents 
    }
    if (input$level_select=="Country" & input$outcome=="Deaths per 100,000") { 
      db = deaths_norm_countries
    }
    if (input$level_select=="US state" & input$outcome=="Deaths per 100,000") { 
      db = deaths_norm_US_states
    }
    
    
    
    if (input$level_select=="World" & input$outcome=="Vaccinated (total)") { 
      db = vacc_world
    }
    if (input$level_select=="Continent" & input$outcome=="Vaccinated (total)") { 
      db = vacc_continents 
    }
    if (input$level_select=="Country" & input$outcome=="Vaccinated (total)") { 
      db = vacc_countries
    }
    if (input$level_select=="US state" & input$outcome=="Vaccinated (total)") { 
      db = vacc_US_states
    }
    
    if (input$level_select=="World" & input$outcome=="Vaccinated per 100,000") { 
      db = vacc_norm_world
    }
    if (input$level_select=="Continent" & input$outcome=="Vaccinated per 100,000") { 
      db = vacc_norm_continents 
    }
    if (input$level_select=="Country" & input$outcome=="Vaccinated per 100,000") { 
      db = vacc_norm_countries
    }
    if (input$level_select=="US state" & input$outcome=="Vaccinated per 100,000") { 
      db = vacc_norm_US_states
    }
    
    if (input$scale=="Natural logarithm") {
      db = log(db)
      for(i in 1:length(rownames(db))){
        db[i,which(db[i,] %in% -Inf)] <- NA
      }
    }
    
    db = db %>% filter(rownames(db) %in% input$region_select)
    db = db[, which(as.Date(colnames(db)) >= as.Date(input$plot_date))]
    
    
  })
  
}