
# update data with automript
source("data/data_import.R")


# Load packages
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinyjqui)) install.packages("shinyjqui", repos = "http://cran.us.r-project.org")
if(!require(shinyAce)) install.packages("shinyAce", repos = "http://cran.us.r-project.org")
if(!require(styler)) install.packages("styler", repos = "http://cran.us.r-project.org")
if(!require(shinyEffects)) install.packages("shinyEffects", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinydashboardPlus)) install.packages("shinydashboardPlus", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(grid)) install.packages("grid", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(bslib)) install.packages("bslib", repos = "http://cran.us.r-project.org")
if(!require(cpm)) install.packages("cpm", repos = "http://cran.us.r-project.org")
if(!require(dashboardthemes)) install.packages("dashboardthemes", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(jcolors)) install.packages("jcolors", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(markdown)) install.packages("markdown", repos = "http://cran.us.r-project.org")
if(!require(fda.usc)) install.packages("fda.usc", repos = "http://cran.us.r-project.org")
if(!require(fda)) install.packages("fda", repos = "http://cran.us.r-project.org")
if(!require(shinybusy)) install.packages("shinybusy", repos = "http://cran.us.r-project.org")
if(!require(refund)) install.packages("refund", repos = "http://cran.us.r-project.org")


source("data/subdivision_of_countries.R")

#functions for dda
source("dda/neg_expo_model.R")
source("dda/cumulative_plot.R")
source("dda/prediction_plot.R")

#functions for fda
source("fda/cumulative_plot_fda.R")
source("fda/add_legend.R")


#initial parameters
min_date <- as.Date(min(colnames(cases_countries)))
current_date <- as.Date(max(colnames(cases_countries)))


#################################################################
locat <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/jhu/locations.csv"))
# head(locat)

locat <- unique(locat[,c("Country/Region", "location")])
countries <- data.frame("location"=rownames(cases_countries))
countries <- merge(countries, locat, by="location", all.x=T)
colnames(countries)[2] <- "Country_Prov"

# countries[which(countries %in% "United States")] <- "US"
#information about regions
Countries_info <- subdivision_of_countries(countries)



regions <- unique(Countries_info$region)
subregions <- unique(Countries_info$`sub-region`)

###################################################################

temp_countries <- countries

Countries_info_1 <- merge(temp_countries,Countries_info,by=c("Country_Prov"), all.x = T)
colnames(Countries_info_1)[which(colnames(Countries_info_1) %in% 'sub-region')] <- "sub_region"
colnames(Countries_info_1)[which(colnames(Countries_info_1) %in% 'location.x')] <- "location"


reg_subreg <- unique(Countries_info_1[,c("sub_region","region")])
subregions <- arrange(reg_subreg,region,sub_region)[,1]

regions <- unique(arrange(reg_subreg,region,sub_region)[,2])
regions <- regions[!is.na(regions)]


countries_list <- list()
for(subreg_index in 1:length(subregions)) {
  
  countries_subreg <- Countries_info_1 %>%
    filter(sub_region %in% subregions[subreg_index]) %>%
    dplyr::select(location)
  colnames(countries_subreg) <- paste0(subregions[subreg_index])
  countries_list <- c(countries_list, countries_subreg)
  
}



continents_list <- list()
for(reg_index in 1:length(regions)) {
  
  countries_reg <- Countries_info_1 %>%
    filter(region %in% regions[reg_index]) %>%
    dplyr::select(location)
  colnames(countries_reg) <- paste0(regions[reg_index])
  continents_list <- c(continents_list, countries_reg)
  
}



# tabs
source("tabs/home_tab.R")
source("tabs/header_tab.R")
source("tabs/dda_plots_tab.R")
source("tabs/dda_model_tab.R")
source("tabs/fda_plots_tab.R")
source("tabs/fda_explor_tab.R")
source("tabs/fda_panel_model_tab.R")
source("tabs/sidebar_tab.R")
source("tabs/body_tab.R")



source("fda/explor_plot_fda.R")
source("fda/fPCA_harm_plot_fda.R")
source("fda/fPCA_harmD_plot_fda.R")
source("fda/fPCA_scores_plot_fda.R")
source("fda/panel_model_plot_fda.R")