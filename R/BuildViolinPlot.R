#  NOTE that I'm setting width and heigh directly here.  THERE ARE OTHER WAYS including most likely
# Convert ggplot to Plotly
# p_plotly <- ggplotly(p)
# # # Set the width of the Plotly plot
# p_plotly <- p_plotly %>% layout(width = 800) # Adjust the width as needed


# Define Function to BUILD PLotly interactive PLot
### I made this a FILE so i could loop through it and print all the plots for all the compounds

###
##
# Additional Notes:
# htmltools::tagList and tagAppendChild: These functions from the htmltools package help in
#   appending and managing multiple HTML widgets (like Plotly charts) in an RMarkdown document.
# Explicit Rendering: By using tagList, each Plotly chart is managed as an individual HTML widget,
#   ensuring proper rendering in the knitted HTML output.
### taglist

#### NEW RESEARCH 9/27/2024:
# https://stackoverflow.com/questions/49725591/dynamically-control-number-of-tabsets-in-r-markdown
# https://stackoverflow.com/questions/43636120/how-to-embed-plots-into-a-tab-in-rmarkdown-in-a-procedural-fashion
# https://github.com/rstudio/rstudio/issues/1795#issuecomment-505779701
#
### THIS IS IT >>  https://github.com/rstudio/rmarkdown/issues/2075  and is good but I actually used:
###   THIS ONE GAVE ANSWER BELOW:  https://stackoverflow.com/questions/61906480/how-to-display-ggplotly-plots-with-dynamically-created-tabs-and-for-loops


buildPlotlyViolin <- function(chemOfConcern, testResults_ChemOfConcern) {
  # chemical_to_chart<-chemsOfConcern[7]
  # nrow()


  # NOW make a table of all the users that had the one chemical of concern that we selected which our one user had
  # testResults_ChemOfConcern <- testResults.big %>%
  #   filter(ParameterName == chemOfConcern) %>%
  #   select(SampleNumber, Result, ParameterName)

  # PICK the highlight point which is OUR ONE USERS result for this one chemical
  highlight_point <- testResults_ChemOfConcern[testResults_ChemOfConcern$SampleNumber ==
    subject, ]


  # Calculate mean and median for each group
  summary_stats <- testResults_ChemOfConcern %>%
    group_by(ParameterName) %>%
    summarise(
      mean_Result = mean(Result),
      median_Result = median(Result)
    )

  # Create the base ggplot with hover text which took lots of playing around to get it to work
  ### NOTE:  I'm was surpressing   "Ignoring unknown aesthetics: text" which for some reason shows up but i fixed maybe
  chemPlot <- ggplot(
    testResults_ChemOfConcern,
    aes(
      x = factor(ParameterName),
      y = Result,
      fill = factor(ParameterName)
    )
  ) +
    geom_violin(trim = FALSE) +
    # geom_boxplot(width=0.1, position=position_dodge(0.9)) +    # DELETE BOX PLOT as just confusing
    geom_point(
      data = summary_stats,
      aes(x = factor(ParameterName), y = mean_Result),
      color = "blue",
      size = 2,
      shape = 21,
      fill = NA
    ) +
    geom_point(
      data = summary_stats,
      aes(x = factor(ParameterName), y = median_Result),
      # color = "green",
      color = "#008000",
      size = 3,
      shape = 21,
      fill = NA
    ) +
    geom_point(
      data = highlight_point,
      aes(x = factor(ParameterName), y = Result),
      color = "red",
      size = 4,
      shape = 21,
      fill = NA
    ) +
    labs(
      # title = "Mean, Median, and Your exposure", y = "Nanograms per Gram Silicone", x = ""
      title = "<span style='color:blue;'>Mean</span>, <span style='color:green;'>Median</span>, and <span style='color:red;'>Your</span> exposure",
      y = "Nanograms per Gram Silicone",
      x = ""
    ) +
    theme_minimal() +
    theme(
      plot.title = element_markdown(size = 14, face = "bold")
    ) +
    scale_fill_brewer(palette = "Pastel1") +
    theme_minimal(base_size = 15) +
    theme(legend.position = "none") +
    coord_flip()

  # Display the interactive plot but try to style just ONE POINT
  text_MEDIAN_label <- paste("Median Result:", round(summary_stats$median_Result, 2))
  text_MEAN_label <- paste("Mean Result:", round(summary_stats$mean_Result, 2))
  text_YOURDATA_label <- paste("Your Result:", round(highlight_point$Result, 2))

  chemPlot <- chemPlot + scale_y_continuous(labels = scales::comma_format(big.mark = ","))

  # plotly_json(chemPlot)   # this is the command to run to figure out what "traces" to set to which info

  ### THIS SHOULD GET RID OF EXPONENTS
  ### because I had problems formating TICKs but I need to format in GGPLOT not PLOTLY... and then plotly just works
  # options(scipen = 999)

  chemPlot_interactive <- ggplotly(chemPlot) %>%
    style(hoverinfo = "none", traces = 1) %>%
    style(text = text_MEAN_label, traces = 2) %>%
    style(text = text_MEDIAN_label, traces = 3) %>%
    # style(text = text_MEDIAN_label, traces = 2)  %>%
    # style(text = text_MEAN_label, traces = 3) %>%
    style(text = text_YOURDATA_label, traces = 4) %>%
    config(displayModeBar = FALSE)

  # chemPlot_interactive
  return(chemPlot_interactive)
}
