

# data_explor <- list("to_plot_table"=to_plot_table, "depth"=filtr_depth, "outliers"=filtr_outliers, "CI"=filtr_CI, "mean"=filtr_mean, "fPCA"=fit.fpca)
# ylab_text <- "o"


fPCA_harm_plot_fda <- function(data_explor, ylab_text){
  
  fpca.obj <- data_explor$fPCA
  
  harmonics <- data.frame(fpca.obj$efunctions)
  colnames(harmonics) = c(paste0("fPC", 1:fpca.obj$npc))
  harmonics$Date <- sort(unique((data_explor$to_plot_table$Date)))
  
  to_plot_table <- harmonics %>%
    pivot_longer(!Date, names_to="fPCs", values_to="var"
    )
  
  un_colors <- length(unique(to_plot_table$fPCs))
  
  fig <- plot_ly()
  
  fig <- fig %>%  add_trace(
    data = to_plot_table,
    x = ~Date,
    y = ~var,
    type = "scatter",
    mode = "lines",
    color = ~fPCs,
    text=~fPCs,
    colors = colorRampPalette(unname(jcolors('pal2')))(un_colors),
    line = list(width = 2),
    showlegend = TRUE) 
  

  fig <- fig %>% layout(title = list(text='',
                                     family="Agency FB",
                                     size = 30,
                                     color = '#ffffff'),
                        xaxis = list(title = 'Date', 
                                     titlefont = list(size = 12),
                                     type = 'date',
                                     tickformat = "%Y-%m-%d",
                                     color = '#ffffff'),
                        yaxis=list(title = paste(ylab_text),
                                   titlefont = list(size = 12),
                                   color = '#ffffff',
                                   tickformat=",d"),
                        margin = list(l = 50, r = 50, t = 60, b = 150),
                        paper_bgcolor='#353c42',
                        plot_bgcolor='#353c42',
                        hovermode = "x unified",
                        hoverlabel=list(font=list(color="white")),
                        legend = list(font=list(color="white"))
  )
  fig
  
  
}