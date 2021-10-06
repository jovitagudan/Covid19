


suppressMessages({ suppressWarnings ({ source("global.R") }) })

load("data/smoothed_data_20211004.RData")

shinyApp(
  ui = dashboardPage(
    
    md = FALSE,
    skin = "midnight",
    
    header = header_tab,
    sidebar = sidebar_tab,
    
    body = body_tab,
    
    controlbar = dashboardControlbar(),
    
    title = "Covid-19",
    footer = dashboardFooter(
      left = "By Jovita Gudan",
      right = "Vilnius, 2021"
    )
  ),
  server = function(input, output,session) {
    
    #<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
    #info of tab: "Home"
    output$markdown <- renderUI({
      file <- 'info/home.Rmd'
      includeMarkdown(file)
    })
    
    #<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
    #data input of tab: "Discrete data analysis" -> "Plots" 
    
    # update region selections
    observeEvent(input$level_select, {
      if (input$level_select=="World") {
        updatePickerInput(session = session, inputId = "region_select", 
                          choices = "World", selected = "World")
      }
      
      if (input$level_select=="Continent") {
        updatePickerInput(session = session, inputId = "region_select", 
                          choices = rownames(cases_continents), 
                          selected = rownames(cases_continents))
      }
      
      if (input$level_select=="US state") {
        updatePickerInput(session = session, inputId = "region_select", 
                          choices = rownames(cases_US_states), 
                          selected = rownames(cases_US_states)[which(rownames(cases_US_states) %in% c("Washington","New York State", "Illinois"))])
      }
      
      if (input$level_select=="Country") {
        updatePickerInput(session = session, inputId = "region_select", 
                          choices = countries_list, 
                          selected = countries_list$`Northern Europe`[which(countries_list$`Northern Europe` %in% c("Lithuania", "Latvia", "Estonia"))])
      }
    }, ignoreInit = TRUE)
    
    
    country_reactive_db = reactive({
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
    
    #plot of tab: "Discrete data analysis" -> "Plots" 
    output$plot1 <- renderPlotly({
      cumulative_plot(country_reactive_db(), input$outcome)
    })
    #<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
    
    #<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
    #data input of tab: "Discrete data analysis" -> "Model" 
    
    observeEvent(input$level_select2, {
      if (input$level_select2=="World") {
        updatePickerInput(session = session, inputId = "region_select2", 
                          choices = "World", selected = "World")
      }
      
      if (input$level_select2=="Continent") {
        updatePickerInput(session = session, inputId = "region_select2", 
                          choices = rownames(cases_continents), 
                          selected = rownames(cases_continents))
      }
      
      if (input$level_select2=="US state") {
        updatePickerInput(session = session, inputId = "region_select2", 
                          choices = rownames(cases_US_states), 
                          selected = rownames(cases_US_states)[which(rownames(cases_US_states) %in% c("Washington"))])
      }
      
      if (input$level_select2=="Country") {
        updatePickerInput(session = session, inputId = "region_select2", 
                          choices = countries_list, 
                          selected = countries_list$`Northern Europe`[which(countries_list$`Northern Europe` %in% c("Lithuania"))])
      }
    }, ignoreInit = TRUE)
    
    
    country_reactive_db2 = reactive({
      

      if (input$level_select2=="World" & input$outcome2=="Cases per 100,000" ) { 
        db2 = cases_norm_world
        db3 = cases_norm_world
      }
      if (input$level_select2=="Continent" & input$outcome2=="Cases per 100,000") { 
        db2 = cases_norm_continents 
        db3 = cases_norm_continents
      }
      if (input$level_select2=="Country" & input$outcome2=="Cases per 100,000") { 
        db2 = cases_norm_countries
        db3 = cases_norm_countries
      }
      if (input$level_select2=="US state" & input$outcome2=="Cases per 100,000") { 
        db2 = cases_norm_US_states
        db3 = cases_norm_US_states
      }
      
      


      if (input$level_select2=="World" & input$outcome2=="Deaths per 100,000" ) {
        db2 = deaths_norm_world
        db3 = deaths_norm_world
      }
      if (input$level_select2=="Continent" & input$outcome2=="Deaths per 100,000" ) {
        db2 = deaths_norm_continents
        db3 = deaths_norm_continents
      }
      if (input$level_select2=="Country" & input$outcome2=="Deaths per 100,000" ) {
        db2 = deaths_norm_countries
        db3 = deaths_norm_countries
      }
      if (input$level_select2=="US state" & input$outcome2=="Deaths per 100,000") {
        db2 = deaths_norm_US_states
        db3 = deaths_norm_US_states
      }



      if (input$level_select2=="World" & input$outcome2=="Vaccinated per 100,000" ) {
        db2 = vacc_norm_world
        db3 = vacc_norm_world
      }
      if (input$level_select2=="Continent" & input$outcome2=="Vaccinated per 100,000" ) {
        db2 = vacc_norm_continents
        db3 = vacc_norm_continents
      }
      if (input$level_select2=="Country" & input$outcome2=="Vaccinated per 100,000" ) {
        db2 = vacc_norm_countries
        db3 = vacc_norm_countries
      }
      if (input$level_select2=="US state" & input$outcome2=="Vaccinated per 100,000" ) {
        db2 = vacc_norm_US_states
        db3 = vacc_norm_US_states
      }
      

   
        db2 = log(db2)
        for(i in 1:length(rownames(db2))){
          db2[i,which(db2[i,] %in% -Inf)] <- NA
        }
        
      
      db2 = db2 %>% filter(rownames(db2) %in% input$region_select2)
      db3 = db3 %>% filter(rownames(db3) %in% input$region_select2)
      
      

      
      #Results table
      results <- t(db2)
      colnames(results)[1] <- "log_data"
      Date <- as.Date(rownames(t(db2)))
      results <- as.data.frame(results)
      results$Date <- Date
      results$Predicted <- NA
      results$Section_date <- NA
      results$Acc_data <- unname(t(db3))
      
      #pre virus period
      pre_virus <- results[!is.na(results$log_data) & is.na(results$Predicted),]
      if (!is.na(mean(pre_virus$log_data))){
        diff_log_data <- diff(pre_virus$log_data)
        begin_exp <- which(diff_log_data!=0)[1]
        pre_rss <- numeric(3)
        pre_rss[1] <- sum(residuals(lm(pre_virus$log_data[1:begin_exp] ~ 1))^2)
        x <- 1:begin_exp
        pre_rss[2] <- tryCatch(sum(residuals(lm(pre_virus$log_data[1:begin_exp] ~ poly(x,1)))^2), error=function(e) rss=NA)
        pre_rss[3] <- tryCatch(sum(residuals(lm(pre_virus$log_data[1:begin_exp] ~ poly(x,2)))^2), error=function(e) rss=NA)


        which_way <- which(pre_rss %in% max(na.omit(pre_rss)))
        if(which_way==1){
          model <- lm(pre_virus$log_data[1:begin_exp] ~ 1)
          pre_pred <- predict(model)
        }else{
          x <- 1:begin_exp
          model <- lm(pre_virus$log_data[1:begin_exp] ~ poly(x,which_way-1))
          pre_pred <- predict(model, list(x = x))
        }
        results$Predicted[which(results$Date %in% pre_virus$Date[1:begin_exp])] <- pre_pred
        results$Section_date[which(results$Date %in% pre_virus$Date[1:begin_exp])] <- pre_virus$Date[begin_exp]
      }

      pre_virus <- results[!is.na(results$log_data) & is.na(results$Predicted),]
      diff_log_data <- diff(pre_virus$log_data)
      while(mean(diff_log_data[1:5])==0){
        if (!is.na(mean(pre_virus$log_data))){
          diff_log_data <- diff(pre_virus$log_data)
          begin_exp <- which(diff_log_data!=0)[1]
          pre_rss <- numeric(3)
          pre_rss[1] <- sum(residuals(lm(pre_virus$log_data[1:begin_exp] ~ 1))^2)
          x <- 1:begin_exp
          pre_rss[2] <- tryCatch(sum(residuals(lm(pre_virus$log_data[1:begin_exp] ~ poly(x,1)))^2), error=function(e) rss=NA)
          pre_rss[3] <- tryCatch(sum(residuals(lm(pre_virus$log_data[1:begin_exp] ~ poly(x,2)))^2), error=function(e) rss=NA)


          which_way <- which(pre_rss %in% max(na.omit(pre_rss)))
          if(which_way==1){
            model <- lm(pre_virus$log_data[1:begin_exp] ~ 1)
            pre_pred <- predict(model)
          }else{
            x <- 1:begin_exp
            model <- lm(pre_virus$log_data[1:begin_exp] ~ poly(x,which_way-1))
            pre_pred <- predict(model, list(x = x))
          }
          results$Predicted[which(results$Date %in% pre_virus$Date[1:begin_exp])] <- pre_pred
          results$Section_date[which(results$Date %in% pre_virus$Date[1:begin_exp])] <- pre_virus$Date[begin_exp]
        }

        pre_virus <- results[!is.na(results$log_data) & is.na(results$Predicted),]
        diff_log_data <- diff(pre_virus$log_data)
      }


      #
      section_i <- results[!is.na(results$log_data) & is.na(results$Predicted) ,]
      while (!is.na(mean(section_i$log_data))){
        if (!is.na(mean(section_i$log_data))){
          #find structural breaks
          diff_log_data <- diff(section_i$log_data)
          fit_cpm = processStream(diff_log_data, cpmType = "ExponentialAdjusted", ARL0=50000)
          breaks_i <- c(fit_cpm$changePoints+20, length(diff_log_data)+1)

          #estimate different models to fit the data with minimal residuals
          if(length(breaks_i)!=0){
            #ncol=7 => number of models
            rss_all <- matrix(NA,ncol=7, nrow=length(breaks_i))
            colnames(rss_all) <- c("Begin","End","RSS_exp","RSS_mean","RSS_poly1","RSS_poly2","RSS_poly3")
            count <- 0
            for(j in 1:length(breaks_i)){
              count <- count + 1
              rss_exp <- tryCatch(sum(residuals(neg_expo_model(section_i$log_data, 1, breaks_i[j]))^2), error=function(e) rss=NA)
              rss_mean <- sum(residuals(lm(section_i$log_data[1:breaks_i[j]] ~ 1))^2)
              x <- 1:breaks_i[j]
              rss_poly_1 <- tryCatch(sum(residuals(lm(section_i$log_data[1:breaks_i[j]] ~ poly(x,1)))^2), error=function(e) rss=NA)
              rss_poly_2 <- tryCatch(sum(residuals(lm(section_i$log_data[1:breaks_i[j]] ~ poly(x,2)))^2), error=function(e) rss=NA)
              rss_poly_3 <- tryCatch(sum(residuals(lm(section_i$log_data[1:breaks_i[j]] ~ poly(x,3)))^2), error=function(e) rss=NA)
              #rss <- ifelse(is(model, "try-error"), NA, sum(residuals(model)^2))
              rss_all[count,1] <- 1
              rss_all[count,2] <- breaks_i[j]
              rss_all[count,3] <- rss_exp
              rss_all[count,4] <- rss_mean
              rss_all[count,5] <- rss_poly_1
              rss_all[count,6] <- rss_poly_2
              rss_all[count,7] <- rss_poly_3
            }
          }

          #optimal model's parameters
          row_nm <- which(min(rss_all[,3:7], na.rm=T) %in% rss_all[,3:7])
          col_nm <- which(rss_all[row_nm,3:7] %in% min(rss_all[row_nm,3:7], na.rm=T))

          #estimation of an optional model
          if(col_nm==1){
            x <- rss_all[row_nm,1]:rss_all[row_nm,2]
            model <- neg_expo_model(section_i$log_data, rss_all[row_nm,1],  rss_all[row_nm,2])
            results$Predicted[which(results$Date %in% section_i$Date[rss_all[row_nm,1]:rss_all[row_nm,2]])] <- predict(model, list(x = x))
            results$Section_date[which(results$Date %in% section_i$Date[rss_all[row_nm,1]:rss_all[row_nm,2]])] <- section_i$Date[rss_all[row_nm,2]]
          }else if(col_nm==2){
            model <- lm(section_i$log_data[rss_all[row_nm,1]:rss_all[row_nm,2]] ~ 1)
            results$Predicted[which(results$Date %in% section_i$Date[rss_all[row_nm,1]:rss_all[row_nm,2]])] <- predict(model)
            results$Section_date[which(results$Date %in% section_i$Date[rss_all[row_nm,1]:rss_all[row_nm,2]])] <- section_i$Date[rss_all[row_nm,2]]
          }else{
            x <- rss_all[row_nm,1]:rss_all[row_nm,2]
            model <- lm(section_i$log_data[rss_all[row_nm,1]:rss_all[row_nm,2]] ~ poly(x,col_nm-2))
            results$Predicted[which(results$Date %in% section_i$Date[rss_all[row_nm,1]:rss_all[row_nm,2]])] <- predict(model, list(x = x))
            results$Section_date[which(results$Date %in% section_i$Date[rss_all[row_nm,1]:rss_all[row_nm,2]])] <- section_i$Date[rss_all[row_nm,2]]
          }

          section_i <- results[!is.na(results$log_data) & is.na(results$Predicted) ,]
          if(nrow(section_i)==0){
            hor=10
            new_rows <- as.data.frame(cbind(NA, results$Date[dim(results)[1]]+1:hor, NA, NA, NA))
            colnames(new_rows) <- names(results)
            new_rows$Date <- as.Date(new_rows$Date, format="%Y-%m-%d", origin="1970-01-01")
            results <- rbind(results, new_rows)
            x_new <- (rss_all[row_nm,2]+1):(rss_all[row_nm,2]+hor)
            #+1:hor


            if(col_nm==2){
              results[(dim(results)[1]-(hor-1)):dim(results)[1],"Predicted"] <- predict(model)
            }else{
              results[(dim(results)[1]-(hor-1)):dim(results)[1],"Predicted"] <- predict(model, list(x = x_new))
            }


          }
        }
      }


      for(j in 1:(dim(results)[1]-1)){
        if(is.na(results[j+1,"Predicted"])){
          results[j+1,"Predicted"] <- NA

        }else if(!is.na(results[j,"Predicted"])) {
          if(results[j+1,"Predicted"]<results[j,"Predicted"]){
            results[j+1,"Predicted"] <- results[j,"Predicted"]
          }
        }
      }

      results$Pred_orig <- exp(results$Predicted)
      cases_per_pop <- 100000
      # pop_dat$Name_countr <- trimws(paste(pop_dat$"Country/Region", pop_dat$"Province/State"),"r")
      population <- population_info[which(population_info$location %in% input$region_select2),"population"]

      results$Cases_total <- (results$Acc_data*population)/cases_per_pop
      results$Cases_total_pred <- round((results$Pred_orig*population)/cases_per_pop,0)

      results$Cases_total_log <- log(results$Cases_total)
      results$Cases_total_pred_log <- log(results$Cases_total_pred)


      return(results)
    })
    
    # plot of tab: "Discrete data analysis" -> "Model"
    output$plot2 <- renderPlotly({

      prediction_plot(country_reactive_db2(), input$outcome2, input$scale2)

    })

    

    #<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
    #data input of tab: "Functional data analysis" -> "Plots" 
    
    observeEvent(input$level_select_fda, {
      if (input$level_select_fda=="World") {
        updatePickerInput(session = session, inputId = "region_select_fda", 
                          choices = "World", selected = "World")
      }
      
      if (input$level_select_fda=="Continent") {
        updatePickerInput(session = session, inputId = "region_select_fda", 
                          choices = rownames(cases_continents), 
                          selected = rownames(cases_continents))
      }
      
      if (input$level_select_fda=="US state") {
        updatePickerInput(session = session, inputId = "region_select_fda", 
                          choices = rownames(cases_US_states), 
                          selected = rownames(cases_US_states)[which(rownames(cases_US_states) %in% c("Washington","New York State", "Illinois"))])
      }
      
      if (input$level_select_fda=="Country") {
        updatePickerInput(session = session, inputId = "region_select_fda", 
                          choices = countries_list, 
                          selected = countries_list$`Northern Europe`[which(countries_list$`Northern Europe` %in% c("Lithuania", "Latvia", "Estonia"))])
      }
    }, ignoreInit = TRUE)
    
    
    country_reactive_db_fda = reactive({

      
      if (input$level_select_fda=="World" & input$outcome_fda=="Cases per 100,000") { 
        db = cases_norm_world_fd
      }
      if (input$level_select_fda=="Continent" & input$outcome_fda=="Cases per 100,000") { 
        db = cases_norm_continents_fd
      }
      if (input$level_select_fda=="Country" & input$outcome_fda=="Cases per 100,000") { 
        db = cases_norm_countries_fd
      }
      if (input$level_select_fda=="US state" & input$outcome_fda=="Cases per 100,000") { 
        db = cases_norm_US_states_fd
      }
      
      
      
      
      
      if (input$level_select_fda=="World" & input$outcome_fda=="Deaths per 100,000") { 
        db = deaths_norm_world_fd
      }
      if (input$level_select_fda=="Continent" & input$outcome_fda=="Deaths per 100,000") { 
        db = deaths_norm_continents_fd
      }
      if (input$level_select_fda=="Country" & input$outcome_fda=="Deaths per 100,000") { 
        db = deaths_norm_countries_fd
      }
      if (input$level_select_fda=="US state" & input$outcome_fda=="Deaths per 100,000") { 
        db = deaths_norm_US_states_fd
      }
      

      
      if (input$level_select_fda=="World" & input$outcome_fda=="Vaccinated per 100,000") { 
        db = vacc_norm_world_fd
      }
      if (input$level_select_fda=="Continent" & input$outcome_fda=="Vaccinated per 100,000") { 
        db = vacc_norm_continents_fd
      }
      if (input$level_select_fda=="Country" & input$outcome_fda=="Vaccinated per 100,000") { 
        db = vacc_norm_countries_fd
      }
      if (input$level_select_fda=="US state" & input$outcome_fda=="Vaccinated per 100,000") { 
        db = vacc_norm_US_states_fd
      }
      

      db_fd = db$fda_data$Wfdobj[which(db$fda_data$Wfdobj$fdnames$reps %in% input$region_select_fda)]
      db0 = as.data.frame(db$level) %>% filter(rownames(db$level) %in% input$region_select_fda)
      db1 = as.data.frame(t(db$first_deriv)) %>% filter(rownames(t(db$first_deriv)) %in% input$region_select_fda)
      db2 = as.data.frame(t(db$second_deriv)) %>% filter(rownames(t(db$second_deriv)) %in% input$region_select_fda)
      
      db = list("fd"=db_fd, "level"=db0, "first_deriv"=db1, "second_deriv"=db2)
      

    })
    
    #plot of tab: "Functional data analysis" -> "Plots" 
    # Level plot
    output$plot_fda_level <- renderPlotly({
      cumulative_plot_fda(country_reactive_db_fda(), input$outcome_fda, "level")
    })
    
    # First derivative plot
    output$plot_fda_first_deriv <- renderPlotly({
      cumulative_plot_fda(country_reactive_db_fda(), input$outcome_fda, "first_deriv")
    })
    
    # Second derivative plot
    output$plot_fda_second_deriv <- renderPlotly({
      cumulative_plot_fda(country_reactive_db_fda(), input$outcome_fda, "second_deriv")
    })
    
    
    #<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
    #data input of tab: "Functional data analysis" -> "Exploratory analysis"
    
    observeEvent(input$level_select_fda_explor, {
      if (input$level_select_fda_explor=="Continent") {
        updatePickerInput(session = session, inputId = "region_select_fda_explor", 
                          choices = names(countries_list),
                          selected = names(countries_list)[11])
      }
      
      if (input$level_select_fda_explor=="US state") {
        updatePickerInput(session = session, inputId = "region_select_fda_explor", 
                          choices = c("All"), 
                          selected = c("All"))
      }

    }, ignoreInit = TRUE)
    
    
    country_reactive_db_fda_explor = reactive({
      
      
      if (input$level_select_fda_explor=="Continent" & input$outcome_fda_explor=="Cases per 100,000") { 
        db = cases_norm_countries_fd
      }
      if (input$level_select_fda_explor=="Continent" & input$outcome_fda_explor=="Deaths per 100,000") { 
        db = deaths_norm_countries_fd
      }
      if (input$level_select_fda_explor=="Continent" & input$outcome_fda_explor=="Vaccinated per 100,000") { 
        db = vacc_norm_countries_fd
      }

      
      
      if (input$level_select_fda_explor=="US state" & input$outcome_fda_explor=="Cases per 100,000") { 
        db = cases_norm_US_states_fd
      }
      if (input$level_select_fda_explor=="US state" & input$outcome_fda_explor=="Deaths per 100,000") { 
        db = deaths_norm_US_states_fd
      }
      if (input$level_select_fda_explor=="US state" & input$outcome_fda_explor=="Vaccinated per 100,000") { 
        db = vacc_norm_US_states_fd
      }
      
      
      if(input$level_select_fda_explor=="Continent"){
      # countries_in_region <- countries_list[which(names(countries_list) %in% 'Northern Europe')][[1]]
      countries_in_region <- countries_list[which(names(countries_list) %in% input$region_select_fda_explor)][[1]]
      fd_region <- db$fda_data$Wfdobj[which(db$fda_data$Wfdobj$fdnames$reps %in% countries_in_region)]
      fd_reg <- Data2fd(seq(0,1, length.out=length(colnames(db$level))), t(db$level[which(rownames(db$level) %in% countries_in_region),]))
      fdata_region <- fdata(fd_reg,argvals=seq(0,1, length.out=length(colnames(db$level))), rangeval=range(0,1))
      level_region = db$level[which(rownames(db$level) %in% countries_in_region),]
      }else{
        fd_region <- db$fda_data$Wfdobj 
        fd_reg <- Data2fd(seq(0,1, length.out=length(colnames(db$level))), t(db$level))
        fdata_region <- fdata(fd_reg,argvals=seq(0,1, length.out=length(colnames(db$level))), rangeval=range(0,1))
        level_region = db$level
      }
      
      db = list("fda"=fd_region, "fdata"=fdata_region, "level" = level_region)
      


      
      fdata_region <- db$fdata
      fd_region <- db$fda
      
      #Depth
      md =  depth.FM(fdata_region)
      cur <- c(md$lmed)
      # #Mean
      mean_EU <- mean.fd(fd_region)
      # #outliers
      out2<-outliers.depth.pond(fdata_region,nb=100,dfunc=depth.FM)$outliers
      # #SD
      sd_EU <- sd.fd(fd_region)
      
      
      
      #evaluate curves at specific points
      # fd_eval = eval.monfd(db$fdata$argvals, fd_region)
      EU_curves =  db$level
      rownames(EU_curves) <- rownames(fdata_region$data)
      EU_curves <- as.data.frame(t(EU_curves))
      date_seq <- seq(as.Date(min(as.Date(colnames(db$level))),format="%Y-%m-%d"),
                      by=1, length.out=dim(EU_curves)[1])
      EU_curves$Date <- date_seq
      
      only_data_EU <- EU_curves[,-which(colnames(EU_curves) %in% "Date")]
      vars_to_sum =  names(only_data_EU)
      sd_EU <- only_data_EU %>%
        group_by(row_number()) %>%
        do(data.frame(SD = sd(unlist(.[vars_to_sum]), na.rm=T)))
      
      to_plot_table <- EU_curves %>%
        pivot_longer(!Date, names_to="country", values_to="var"
        ) 
      
      to_plot_table <- to_plot_table[order(to_plot_table$country, to_plot_table$Date),]
      
      #mean data
      filtr_mean <- data.frame("Date"=unique(to_plot_table$Date),
                               "var" = rowMeans(EU_curves[-which(colnames(EU_curves) %in% "Date")]),
                               "country"="Mean")
      # #depth data
      filtr_depth <- to_plot_table[which(to_plot_table$country %in% names(cur)),]
      # #Outliers data
      filtr_outliers <- to_plot_table[which(to_plot_table$country %in% out2),]
      # #CI data
      filtr_CI <- data.frame("Date"=unique(to_plot_table$Date),
                             "CI_lower" = ifelse(filtr_mean$var-1.96*sd_EU$SD<0,0,filtr_mean$var-1.96*sd_EU$SD),
                             "CI_upper" = filtr_mean$var+1.96*sd_EU$SD,
                             "country"="CI",
                             var=2)
      
      #data with no outliers
      if(length(out2) != 0){
        data_region_wo <-db$level[-which(rownames(db$level) %in% out2), ]
      }else{
        data_region_wo <- db$level
      }
      
      
      #fPCA
      fit.fpca = fpca.sc(Y = fdata_region$data, pve=0.99)
      
      
      return(list("to_plot_table"=to_plot_table, "depth"=filtr_depth, "outliers"=filtr_outliers, "CI"=filtr_CI, "mean"=filtr_mean, "fPCA"=fit.fpca))
      
    })
    
    
    
    
    #plot of tab: "Functional data analysis" -> "Exploratory analysis and fPCA" 
    output$plot_fda_explor <- renderPlotly({
      withProgress(message = 'Calculation in progress',
                   detail = 'This may take a while...', value = 0, {
                     for (i in 1:350) {
                       incProgress(1/350)
                       Sys.sleep(1)
                     }
                   })
      explor_plot_fda(country_reactive_db_fda_explor(), input$outcome_fda_explor)

    })
    
    
    #plot of tab: "Functional data analysis" -> "Exploratory analysis and fPCA" 
    output$plot_fda_fPCA_harm <- renderPlotly({
      fPCA_harm_plot_fda(country_reactive_db_fda_explor(), input$outcome_fda_explor)
    })
    
    #plot of tab: "Functional data analysis" -> "Exploratory analysis and fPCA" 
    output$plot_fda_fPCA_harmD1 <- renderPlotly({
      fPCA_harmD_plot_fda(country_reactive_db_fda_explor(), 1, input$outcome_fda_explor)
    })
    
    #plot of tab: "Functional data analysis" -> "Exploratory analysis and fPCA" 
    output$plot_fda_fPCA_harmD2 <- renderPlotly({
      fPCA_harmD_plot_fda(country_reactive_db_fda_explor(), 2, input$outcome_fda_explor)
    })
    
    #plot of tab: "Functional data analysis" -> "Exploratory analysis and fPCA" 
    output$plot_fda_fPCA_harmD3 <- renderPlotly({
      fPCA_harmD_plot_fda(country_reactive_db_fda_explor(), 3, input$outcome_fda_explor)
    })
    
    
    #plot of tab: "Functional data analysis" -> "Exploratory analysis and fPCA" 
    output$plot_fda_fPCA_scores <- renderPlotly({
      fPCA_scores_plot_fda(country_reactive_db_fda_explor(), input$outcome_fda_explor)
    })
    
    
    
    
    
    country_reactive_db_panel_model = reactive({
      if (input$outcome_fda_panel_model=="Cases per 100,000") { 
        db = cases_norm_countries_fd
      }
      if (input$outcome_fda_panel_model=="Deaths per 100,000") { 
        db = deaths_norm_countries_fd 
      }
      if (input$outcome_fda_panel_model=="Vaccinated per 100,000") { 
        db = vacc_norm_countries_fd
      }

   
      db = db$level[which(rownames(db$level) %in% unname(unlist(continents_list))),]
      regions <- merge(data.frame("location"=rownames(db)),Countries_info_1[,c("location", "region")], by="location")
      return(list("data"=db, "regions"=regions))
      
    })
    
    
    
    #plot of tab: "Functional data analysis" -> "Exploratory analysis and fPCA" 
    output$plot_fda_panel_model <- renderPlotly({
      withProgress(message = 'Calculation in progress',
                   detail = 'This may take a while...', value = 0, {
                     for (i in 1:250) {
                       incProgress(1/250)
                       Sys.sleep(1)
                     }
                   })
      panel_model_plot_fda(country_reactive_db_panel_model(), input$outcome_fda_panel_model)
    })
    
    
    
    
    
    
  }
)






