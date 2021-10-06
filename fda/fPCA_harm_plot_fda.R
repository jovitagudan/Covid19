



fPCA_harm_plot_fda <- function(data_explor, ylab_text){
  
  fpca.obj <- data_explor$fPCA
  
  harmonics <- data.frame(fpca.obj$efunctions)
  colnames(harmonics) <- c("fPC1","fPC2", "fPC3")
  harmonics$Date <- sort(unique((data_explor$to_plot_table$Date)))
  
  to_plot_table <- harmonics %>%
    pivot_longer(!Date, names_to="PCs", values_to="var"
    )
  
  un_colors <- length(unique(to_plot_table$PCs))
  
  fig <- plot_ly()
  
  fig <- fig %>%  add_trace(
    data = to_plot_table,
    x = ~Date,
    y = ~var,
    type = "scatter",
    mode = "lines",
    color = ~PCs,
    text=~PCs,
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