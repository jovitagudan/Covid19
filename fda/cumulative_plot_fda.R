

# var_to_plot = db
# y_text = "Cases per 100,000"
# which_level = "level"

cumulative_plot_fda  = function(var_to_plot, y_text, which_level) {
  
  if(which_level=="level"){

    table_to_plot = var_to_plot$level
    
  }else if(which_level=="first_deriv"){
    table_to_plot = var_to_plot$first_deriv
  }else{
    table_to_plot = var_to_plot$second_deriv
  }
  
  
  var_to_plot0=data.frame(t(table_to_plot), "Date"=colnames(table_to_plot))
  to_plot_table <- var_to_plot0 %>%
    pivot_longer(!Date, names_to="country", values_to="var"
    )
  
  un_colors <- length(unique(to_plot_table$country))
  
  fig <- plot_ly(data = to_plot_table,
                 x = ~Date, 
                 y = ~var,
                 type = "scatter",
                 mode = "lines", 
                 color = ~country,
                 text=~country,
                 colors = colorRampPalette(unname(jcolors('pal2')))(un_colors),
                 hovertemplate = paste('<b>%{text}</b>: %{y:,.0f}',
                                       "<extra></extra>"),
                 line = list(width = 2)
                 # height = 500, 
                 # width = 800
  ) 
  
  
  fig <- fig %>% layout(title = list(text='',
                                     family="Agency FB",
                                     size = 30,
                                     color = '#ffffff'),
                        xaxis = list(title = 'Date', 
                                     titlefont = list(size = 12),
                                     type = 'date',
                                     tickformat = "%Y-%m-%d",
                                     color = '#ffffff'),
                        yaxis=list(title = paste(y_text),
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