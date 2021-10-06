

# db = cases_norm_countries_fd
# 
# 
# 
# regions_expl <- names(countries_list)
# 
# 
# 
# 
# 
# which_c <- 11
# countries_in_region <- countries_list[which(names(countries_list) %in% regions_expl[which_c])][[1]]
# fd_region <- db$fda_data$Wfdobj[which(db$fda_data$Wfdobj$fdnames$reps %in% countries_in_region)]
# fdata_region <- fdata(fd_region,argvals=seq(0,1, length.out=length(colnames(db$level))), rangeval=range(0,1))
# 
# 
# data_explor <- list("to_plot_table"=to_plot_table, "depth"=filtr_depth, "outliers"=filtr_outliers, "CI"=filtr_CI, "mean"=filtr_mean, "fPCA"=fit.fpca)
# ylab_text <- "o"

explor_plot_fda <- function(data_explor, ylab_text){
  

  
  to_plot_table <- data_explor$to_plot_table
  filtr_CI <- data_explor$CI
  filtr_depth <- data_explor$depth
  filtr_outliers <- data_explor$outliers
  filtr_mean <- data_explor$mean
  
  #plot
  #line types
  how_many_all <- length(as.numeric(factor(unique(to_plot_table$country))))*length(unique(to_plot_table$Date))
  linetype_vec <- rep(rep(1:6,each=length(unique(to_plot_table$Date))),length.out=how_many_all)



un_colors <- length(unique(to_plot_table$country))


fig <- plot_ly()
  
fig <- fig %>%  add_trace(
    data = to_plot_table,
    x = ~Date,
    y = ~var,
    type = "scatter",
    mode = "lines",
    color = ~country,
    text=~country,
    colors = gray.colors(un_colors),
    linetype = ~country,
    hovertemplate = paste('<b>%{text}</b>: %{y:,.0f}',
                          "<extra></extra>"),
    line = list(width = 2),
    showlegend = FALSE) 

fig <- fig  %>% add_ribbons(data = filtr_CI,
                  x=      filtr_CI$Date,    
              ymin = ~filtr_CI$CI_lower,
              ymax = ~filtr_CI$CI_upper,
              line = list(color = 'rgba(7, 164, 181, 0.05)'),
              fillcolor = 'rgba(7, 164, 181, 0.1)',
              name = '95% CI') 



fig <- fig %>%  add_trace(
  x=filtr_depth$Date,
  y = ~filtr_depth$var,
  type = "scatter",
  name = paste0('Depth: ', unique(filtr_depth$country)),
  mode = 'lines',
  line = list(color = '#7FFFD4', width = 4),
  hovertemplate = paste('<b>Depth </b>: %{y:,.0f}',
                        "<extra></extra>")) 

 fig <- fig %>%  add_trace(
            x=filtr_mean$Date,
            y = ~filtr_mean$var,
            type = "scatter",
            name = 'Mean',
            mode = 'lines',
            line = list(color = '#EE9A00', width = 4),
            hovertemplate = paste('<b>Mean </b>: %{y:,.0f}',
                                  "<extra></extra>")) 
if(nrow(filtr_outliers) != 0){
  fig <- fig %>%  add_trace(
    x=filtr_outliers$Date,
    y = ~filtr_outliers$var,
    color = ~filtr_outliers$country,
    # colors = rep('#A52A2A',length(unique(filtr_outliers$country))),
    type = "scatter",
    name = paste0("Outliers: ", filtr_outliers$country),
    mode = 'lines',
    line = list( width = 4, color = '#A52A2A'),
    hovertemplate = paste('<b>Outliers </b>: %{y:,.0f}',
                          "<extra></extra>")) 
  
}
 
 
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


 # ggplot(to_plot_table, aes(x = Date, y = var, color = country, group = country)) +
 # geom_line(linetype = linetype_vec)
  #CI
#   geom_ribbon(data=filtr_CI,aes(ymin=CI_lower, ymax=CI_upper), fill="lightseagreen", alpha=0.15, linetype = 0)+
#   #Outliers
#   geom_line(data=filtr_outliers, aes(x=Date, y=var), size=1, colour="red", linetype="dashed") +
#   #Mean
#   geom_line(data=filtr_mean, aes(x=Date, y=var), size=1.2, colour="seashell2") + 
#   #Depth
#   geom_line(data=filtr_depth, aes(x=Date, y=var), size=1.2, colour="turquoise1") + 
#   # Add labels at the end of the line
#   # geom_text(data = filter(to_plot_table, Date == max(Date)  & country %in% c("Lithuania", "Estonia", "Latvia", "Poland")),
#   #           aes(label = country),
#   #           hjust = -0.2, nudge_x =0.1, size=2.5) +
#   # 
#   # geom_text_repel(data = filter(to_plot_table, Date == max(Date) & country %in% c("Lithuania", "Estonia", "Latvia", "Poland")),
#   #                 aes(label = country),
#   #                 hjust = 0, nudge_x =5, size=5)+
#   scale_color_grey(start = 0.7, end = 0.5) +
#   # Allow labels to bleed past the canvas boundaries
#   coord_cartesian(clip = 'off') +
#   # Remove legend & adjust margins to give more space for labels
#   # Remember, the margins are t-r-b-l
#   scale_y_continuous(labels = scales::comma) +
#   scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
#   labs(title="",
#        x ="Date", y = ylab_text)+
#   dark_theme_gray()  +
#   theme(legend.position = 'none',
#         plot.margin = margin(0.1, 2.6, 0.1, 0.1, "cm"),
#         axis.text.x=element_text(angle=45, hjust=1, size=12),
#         axis.text.y=element_text(size=12),
#         axis.title=element_text(size=12))+
#   #Outliers annotations
#   #  if(nrow(filtr_outliers) != 0){
#   # geom_text(data=filter(filtr_outliers, Date==max(Date)),
#   #           aes(x = max(Date) + 10, y = var, label=country), hjust = 0, vjust = c(0.5), size=5, colour="red") +
#   # 
#   # geom_segment(data=filter(filtr_outliers, Date==max(Date)),
#   #              aes(x = Date + 3, xend = max(Date) + 9, y = var, yend = var), size = 0.2, colour="red")}+
#   #Depth annotation
#   geom_text(data=filter(filtr_depth, Date==max(Date)),
#             aes(x = max(Date) + 10, y = var, label=country), hjust = 0, vjust = c(0.5), size=5, colour="turquoise1") +
#   
#   geom_segment(data=filter(filtr_depth, Date==max(Date)),
#                aes(x = Date + 3, xend = max(Date) + 9, y = var, yend = var), size = 0.2, colour="turquoise1")
# # annotation_custom(legend_pic, xmin=min(to_plot_table$Date)-120, xmax=max(to_plot_table$Date)+30, ymin=9600, ymax=10300)
# 
# add_legend("topleft", c("Mean",
#                         "95% CI",
#                         paste("Depth",sep=""),
#                         "Outliers"),
#            lwd = 2, lty = c(1,NA,1,2) ,col = c("seashell2", NA, "turquoise1", "red"),
#            pt.bg = adjustcolor(c(NA,"lightseagreen",NA,NA), alpha=0.2), pch = c(NA,22,NA,NA),
#            cex=0.85, bty="n", horiz=T, text.width=c(0,0.15,0.19,0.19), pt.cex=c(1,2,1,1), text.col="white")

}
