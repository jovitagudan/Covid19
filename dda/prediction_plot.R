

prediction_plot  = function(var_to_plot, text, which_var) {
  vline <- function(x = 0, color = "red") {
    list(
      type = "line", 
      y0 = 0, 
      y1 = 1, 
      yref = "paper",
      x0 = x, 
      x1 = x, 
      line = list(color = color, dash="dot")
    )
  }
  
  if(text=="Cases per 100,000" | text=="Deaths per 100,000" | text=="Vaccinated per 100,000"){
    if(which_var=="Original"){
      true_val <- var_to_plot$Acc_data
      predicted <- var_to_plot$Pred_orig
    }else{
      true_val <- var_to_plot$log_data
      predicted <- var_to_plot$Predicted
    }
  }else if(which_var=="Original"){
    true_val <- var_to_plot$Cases_total
    predicted <- var_to_plot$Cases_total_pred
  }else{
    true_val <- var_to_plot$Cases_total_log
    predicted <- var_to_plot$Cases_total_pred_log
  }
  
  #sections
  sec_dates_n <- length(unique(na.omit(var_to_plot$Section_date)))
  
  sections_inner <- list()
  for(k in 1:(sec_dates_n-1)){
    sections_inner[[k]] <- vline(as.Date(unique(na.omit(var_to_plot$Section_date))[k], origin="1970-01-01"), color="grey")
  }
  
  sections_fore <- list()
  for(k in  1:1){
    sections_fore[[k]] <- vline(as.Date(unique(na.omit(var_to_plot$Section_date))[sec_dates_n], origin="1970-01-01"), color="green")
  }
  
  
  
  fig <- plot_ly() 
  
  fig <- fig %>% add_trace(x = var_to_plot$Date, 
                           y = true_val,
                           name = paste(text),
                           type = 'scatter',
                           mode = "lines+markers",
                           line = list(
                             color = '#7F7F7F'
                           ),
                           marker= list(
                             color = '#7F7F7F'
                           ),
                           hovertemplate = paste('<b>',text,'</b>: %{y:,.0f}',
                                                 "<extra></extra>"))
  fig <- fig %>% add_trace(x = var_to_plot$Date, 
                           y = predicted,
                           name = paste("Predicted"),
                           type = 'scatter',
                           mode = "lines+markers",
                           line = list(
                             color = '#CC0000'
                           ),
                           marker= list(
                             color = '#CC0000'
                           ),
                           hovertemplate = paste('<b>Predicted</b>: %{y:,.0f}',
                                                 "<extra></extra>"))
  
  
  
  fig <- fig %>% layout(title = '',
                        shapes = c(sections_inner,sections_fore),
                        xaxis = list(title = 'Date', 
                                     titlefont = list(size = 12),
                                     type = 'date',
                                     tickformat = "%Y-%m-%d",
                                     color = '#ffffff'),
                        yaxis=list(title = paste(text),
                                   titlefont = list(size = 12),
                                   color = '#ffffff',
                                   tickformat=",d"),
                        legend = list(font = list(color = '#ffffff')),
                        margin = list(l = 50, r = 50, t = 60, b = 150),
                        paper_bgcolor='#353c42',
                        plot_bgcolor='#353c42',
                        hovermode = "x unified",
                        hoverlabel=list(font=list(color="white")),
                        legend = list(font=list(color="white"))
                        
  )
  
  
  
  
  
  
  fig
}