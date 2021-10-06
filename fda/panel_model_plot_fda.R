

# 
# input_data=list("data"=db, "regions"=regions)
# ylab_text = "Cases"



panel_model_plot_fda <- function(input_data, ylab_text){

modmat = cbind(1, model.matrix(~ factor(input_data$regions$region) - 1))
constraints = matrix(c(0,1,1,1,1,1), 1)



panel_mod = fosr(Y= input_data$data, X= modmat, con=constraints)
# plot(panel_mod$fd[1], pages=1, scale=0)
# par(mfrow=c(2,3), bg = '#353c42')
# plot(panel_mod, split=1, set.mfrow=FALSE,titles=c("Overall", levels(factor(input_data$regions$region))),ylab=ylab_text, xlab="Day",
#      col.lab = "white",    # X and Y-axis labels color
#      col.axis = "white",   # Tick labels color
#      fg = "white",
#      col.main = "white",
#      col = rep("white",3))


for(i in 1:(length(unique(input_data$regions$region))+1)){
  
  
  t1 <- list(
    family="Times New Roman",
    size = 30,
    color = '#ffffff'
  )
  
fig <- plot_ly(height=1000)

fig <- fig %>% add_trace(data=panel_mod,
                           x=as.Date(colnames(input_data$data)),
                         # x=panel_mod$argvals,
                           y=panel_mod$est.func[,i],
                           type='scatter',
                           mode = "lines",
                           name="Estimate",
                           line = list(width = 2, color ="white"),
                           showlegend=F
                           )

fig <- fig %>% add_trace(data=panel_mod,
                           x=as.Date(colnames(input_data$data)),
                         # x=panel_mod$argvals,
                           y=panel_mod$est.func[,i] + 1.96*panel_mod$se.func[,i],
                           type='scatter',
                           mode = "lines",
                           name="95% UCI",
                           line = list(width = 2, dash = 'dash', color ="white"),
                           showlegend=F
                           )

fig <- fig %>% add_trace(data=panel_mod,
                           x=as.Date(colnames(input_data$data)),
                         # x=panel_mod$argvals,
                           y=panel_mod$est.func[,i] - 1.96*panel_mod$se.func[,i],
                           type='scatter',
                           mode = "lines",
                           name="95% LCI",
                           line = list(width = 2, dash = 'dash', color ="white"),
                           showlegend=F
)

fig <- fig %>% layout(title= list(text = "",font = t1),
                      xaxis = list(title = 'Date', 
                                   titlefont = list(size = 12),
                                   type = 'date',
                                   dtick = "M1",
                                   tickformat = "%b<br>%Y",
                                   color = '#ffffff',
                                   zerolinecolor = '#ffff',
                                   zerolinewidth = 2,
                                   gridcolor = 'grey'),
                      yaxis=list(title = paste(ylab_text),
                                 titlefont = list(size = 12),
                                 color = '#ffffff',
                                 tickformat=",d"),
                      margin = list(l = 50, r = 50, t = 60, b = 150),
                      paper_bgcolor='#353c42',
                      plot_bgcolor='#353c42'
                      # hovermode = "x unified",
                      # hoverlabel=list(font=list(color="white")),
                      # legend = list(font=list(color="white"))
)



assign(paste0("fig", i), fig)
}



fig <- subplot(fig1, fig2, fig3, fig4, fig5, fig6,  nrows = 2, titleY = TRUE, titleX = TRUE, margin = c(0.07, 0.07, 0.1, 0.2) )
fig



# Update title
annotations = list( 
  list( 
    x = 0.03,  
    y = 1,  
    text = "Overall",  
    xref = "paper",  
    yref = "paper",  
    xanchor = "center",
    yanchor = "bottom",
    showarrow = FALSE,
    font = list(color="white")
    
  ),  
  list( 
    x = 0.45,  
    y = 1,  
    text = "Africa",  
    xref = "paper",  
    yref = "paper",  
    xanchor = "center",  
    yanchor = "bottom",  
    showarrow = FALSE,
    font = list(color="white") 
  ),  
  list( 
    x = 0.8,  
    y = 1,  
    text = "Americas",  
    xref = "paper",  
    yref = "paper",  
    xanchor = "center",  
    yanchor = "bottom",  
    showarrow = FALSE,
    font = list(color="white") 
  ),  
  list( 
    x = 0.03,  
    y = 0.4,  
    text = "Asia",  
    xref = "paper",  
    yref = "paper",  
    xanchor = "center",  
    yanchor = "bottom",  
    showarrow = FALSE,
    font = list(color="white")  
  ),
  list( 
    x = 0.45,  
    y = 0.4,  
    text = "Europe",  
    xref = "paper",  
    yref = "paper",  
    xanchor = "center",  
    yanchor = "bottom",  
    showarrow = FALSE,
    font = list(color="white")  
  ),
  list( 
    x = 0.8,  
    y = 0.4,  
    text = "Oceania",  
    xref = "paper",  
    yref = "paper",  
    xanchor = "center",  
    yanchor = "bottom",  
    showarrow = FALSE,
    font = list(color="white")  
  ))

fig <- fig %>%layout(annotations = annotations) 
fig




# my_plot <- . %>% 
#   plot_ly(x = ~date, y = ~value) %>%
#   add_annotations(
#     text = ~unique(variable),
#     x = 0.5,
#     y = 1,
#     yref = "paper",
#     xref = "paper",
#     xanchor = "middle",
#     yanchor = "top",
#     showarrow = FALSE,
#     font = list(size = 15)
#   )
# 
# economics_long %>%
#   group_by(variable) %>%
#   do(p = my_plot(.)) %>%
#   subplot(nrows = NROW(.), shareX = TRUE)



}


