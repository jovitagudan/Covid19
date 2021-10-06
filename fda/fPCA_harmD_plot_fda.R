


# data_explor <- list("to_plot_table"=to_plot_table, "depth"=filtr_depth, "outliers"=filtr_outliers, "CI"=filtr_CI, "mean"=filtr_mean, "fPCA"=fit.fpca)
# ylab_text <- "o"

fPCA_harmD_plot_fda <- function(data_explor, pc_choice, ylab_text){

  obj <- data_explor$fPCA
  
# for(i in 1:obj$npc){
  

pc_choice <- pc_choice

efunctions = matrix(obj$efunctions, ncol = obj$npc)
sqrt.evalues = diag(sqrt(obj$evalues), obj$npc, obj$npc)
scaled_efunctions = efunctions %*% sqrt.evalues
scaled_efuncs = scaled_efunctions[,pc_choice]
date_seq <- sort(unique((data_explor$to_plot_table$Date)))

df = data.frame(id = pc_choice,
                plus = obj$mu + 2 * scaled_efuncs,
                minus = obj$mu - 2 * scaled_efuncs,
                mu = obj$mu,
                Date = date_seq)
df$plus <- ifelse(df$plus<0, 0, df$plus)
df$minus <- ifelse(df$minus<0, 0, df$minus)




fig <- plot_ly()

fig <- fig %>%  add_trace(
  data = df,
  x = ~df$Date,
  y = ~df$mu,
  type = "scatter",
  mode = "lines",
  name= "Mean",
  line = list(width = 4, color = "grey"),
  showlegend = TRUE) 


fig <- fig %>% add_markers(
  data = df,
  x = ~df$Date,
  y = ~df$plus,
  type = "scatter",
  mode = "markers",
  name="Effect of adding fPC",
  marker = list(size = 4, color = "#6959CD", symbol = 'x'),
  showlegend = TRUE
)

fig <- fig %>% add_markers(
  data = df,
  x = ~df$Date,
  y = ~df$minus,
  # type = "scatter",
  # mode = "markers",
  name="Effect of substracting fPC",
  marker = list(size = 3, color = "#4EEE94", symbol = "x"),
  showlegend = TRUE
)




fig <- fig %>% layout(title = list(text=paste0((100*round(obj$evalues[pc_choice]/sum(obj$evalues),3)), "% Variance"),
                                   family="Agency FB",
                                   size = 30,
                                   font=list(color = '#ffffff')),
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
                      # hovermode = "x unified",
                      # hoverlabel=list(font=list(color="white")),
                      legend = list(font=list(color="white"))
)
# assign(paste0("fig",i),fig)
# 
# }
#   
#   if(obj$npc == 1){
#     fig <- subplot(fig1, nrows = obj$npc) 
#   }else if(obj$npc == 2){
#     fig <- subplot(fig1, style(fig2, showlegend = F), nrows = obj$npc) 
#   }else if(obj$npc == 3){
#   fig <- subplot(fig1,  style(fig2, showlegend = F),  style(fig3, showlegend = F), nrows =  obj$npc)
#   }else if(obj$npc == 4){
#     fig <- subplot(fig1,  style(fig2, showlegend = F),  style(fig3, showlegend = F),  style(fig4, showlegend = F), nrows =  obj$npc) 
#   }
#   
#   fig
  

}
