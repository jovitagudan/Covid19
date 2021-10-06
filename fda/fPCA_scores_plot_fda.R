
# data_explor <- list("to_plot_table"=to_plot_table, "depth"=filtr_depth, "outliers"=filtr_outliers, "CI"=filtr_CI, "mean"=filtr_mean, "fPCA"=fit.fpca)
# ylab_text <- "o"


fPCA_scores_plot_fda <- function(data_explor, ylab_text){

  
fit.fpca <- data_explor$fPCA
#Scores
scoredata = as.data.frame(cbind(fit.fpca$scores))
colnames(scoredata) = c(paste0("fPC", 1:fit.fpca$npc))
scoredata = mutate(scoredata, country = unique(rownames(fit.fpca$Y)))

filtr_text <- scoredata


if(fit.fpca$npc==3){
scene = list(camera = list(eye = list(x =1, y = 1.25, z = 0.6)),
             xaxis = list(title = 'fPC1', color = '#ffffff'),
             yaxis = list(title = 'fPC2', color = '#ffffff'),
             zaxis = list(title = 'fPC3', color = '#ffffff'))

fig <- plot_ly(data = scoredata,  x=scoredata$fPC1, 
               y=scoredata$fPC2, z=scoredata$fPC3, text=scoredata$country,
               hovertemplate = paste('<br><b>fPC1</b>: %{x:.2f}',
                                     '<br><b>fPC2</b>: %{y:.2f}',
                                     '<br><b>fPC3</b>: %{z:.2f}',
                                     '<br><b>Country</b>: %{text}')
)
fig <- fig %>% add_markers(alpha=0.5)
# fig <- fig %>% layout(scene = scene,
#                       showlegend = FALSE
# )
# title = paste0('fPCA for ', regions[which_c]))
fig <- fig %>% add_trace(data= filtr_text, x = filtr_text$fPC1, y = filtr_text$fPC2,
                         z = filtr_text$fPC3, type = "scatter3d",
                         text = filtr_text$country, mode = "text", 
                         textfont = list(size = 20, color = '#ffffff'))
fig <- fig %>% layout(paper_bgcolor='#353c42',
                      plot_bgcolor='#353c42',
                      scene = scene,
                      showlegend = FALSE)
}


if(fit.fpca$npc==2){

  
  fig <- plot_ly(data = scoredata,  x=scoredata$fPC1, 
                 y=scoredata$fPC2, text=scoredata$country,
                 type = "scatter",
                 mode="markers",
                 hovertemplate = paste('<br><b>fPC1</b>: %{x:.2f}',
                                       '<br><b>fPC2</b>: %{y:.2f}',
                                       '<br><b>Country</b>: %{text}')
  )
  fig <- fig %>% add_markers(alpha=0.5,
                             name="",
                             marker=list(color="cyan"),
                             hovertemplate = paste('<br><b>fPC1</b>: %{x:.2f}',
                                                   '<br><b>fPC2</b>: %{y:.2f}',
                                                   '<br><b>Country</b>: %{text}'))

  fig <- fig %>% add_trace(data= filtr_text, x = filtr_text$fPC1, y = filtr_text$fPC2+20,
                        type = "scatter",
                           text = filtr_text$country, mode = "text", 
                           textfont = list(size = 20, color = '#ffffff'))
  fig <- fig %>% layout(paper_bgcolor='#353c42',
                        plot_bgcolor='#353c42',
                        # scene = scene,
                        showlegend = FALSE,
                        xaxis = list(
                                     titlefont = list(size = 12),
                                     color = '#ffffff'),
                        yaxis=list(
                                   titlefont = list(size = 12),
                                   color = '#ffffff'))
}


fig

}
