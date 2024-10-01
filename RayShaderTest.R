#remotes::install_github("tylermorganwall/rayshader")
#install_github("tylermorganwall/rayshader")

library(rayshader)


x5_activeChemChartTEST <- ggplot(
  #testResults.big,
  #testResults,
  #ATTEMPTING to eliminate ZERO values from testResults.big with a filter
  tr.big.without.NotFound, #Get rid of all rows that have NO values set
  aes(
    y = factor(ParameterName,
               levels = sort(unique(ParameterName),
                             decreasing = TRUE)),
    x = SampleNumber,
    fill = quartile),
  ylab = NULL,
  xlab = NULL
) +
  geom_tile()  #+
  # scale_fill_manual(values = x5_colors,
  #                   name = "Relative \n Amount",
  #                   breaks=c("0","1", "2", "3","4"),
  #                   labels=c("None","Bottom 25%", "2nd 25%", "3rd 25%","Top 25%")
  # )



x5_activeChemChartTEST
plot_gg(x5_activeChemChartTEST,multicore=TRUE,width=5,height=5,scale=250)
