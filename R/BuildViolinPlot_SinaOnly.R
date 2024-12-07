# Updated version to handle zero values with two distinct panels using patchwork
### ONLY tested with calling SINA function
# Updated version to handle zero values with two distinct panels using patchwork

# Function to build an interactive Sina plot with separate panels for zero and non-zero values
buildPlotlySina <- function(chemOfConcern, testResults_ChemOfConcern, subject) { ###
  logScale <- TRUE # Set the log scale option to TRUE for plotting

  # Separate zero and non-zero values
  zero_values <- testResults_ChemOfConcern %>% filter(Result == 0) # Filter out rows with zero values for separate handling
  non_zero_values <- testResults_ChemOfConcern %>% filter(Result > 0) # Filter out rows with non-zero values for plotting

  # Add a column to represent the x-axis as "Zero" if zero values exist
  if (nrow(zero_values) > 0) {
    zero_values <- zero_values %>%
      mutate(ZeroLabel = "Zero") # Create a new column to represent zero values on the x-axis
  }

  # Highlight the specific point for non-zero values (the subject's value)
  highlight_point <- non_zero_values[non_zero_values$SampleNumber == subject, ] # Extract the specific point to highlight based on the subject identifier
  if (nrow(highlight_point) == 0) {
    warning("Subject SampleNumber not found in non-zero values.")
  }

  # Summary statistics for non-zero values
  summary_stats <- non_zero_values %>%
    group_by(ParameterName) %>% # Group data by ParameterName to calculate statistics per group
    summarise(
      mean_Result = mean(Result), # Calculate the mean of non-zero values for each ParameterName
      median_Result = median(Result) # Calculate the median of non-zero values for each ParameterName
    )

  # Create the left-hand plot for zero values if they exist
  if (nrow(zero_values) > 0) {
    zeroPlot <- ggplot(zero_values, aes(x = ZeroLabel, y = 0)) +
      geom_jitter(color = "#696969", alpha = 0.4, width = 0.2, height = 0.01, shape = 21, fill = NA) + # Plot zero values with slight jitter for visibility
      scale_y_continuous(breaks = c(0)) + # Only show '0' on the y-axis
      labs(
        x = "", # Remove x-axis label
        y = "not detected" # Label for zero values
      ) +
      theme_minimal(base_size = 10) + # Set minimal theme with base font size
      theme(
        axis.title.y = element_text(size = 10, color = "#A9A9A9"), # Set y-axis title styling to smaller and lighter grey
        axis.text.y = element_blank(), # Remove y-axis text labels
        axis.ticks.y = element_blank(), # Remove y-axis ticks
        plot.title = element_text(size = 14, face = "bold"), # Set plot title styling
        legend.position = "none" # Remove legend
      ) +
      coord_flip() # Flip coordinates to switch x and y axes
  } else {
    # Create an empty plot if there are no zero values
    zeroPlot <- ggplot() +
      theme_void() + # Set an empty theme
      labs(title = "No Zero Values Available") # Indicate that there are no zero values available
  }

  # Create the right-hand plot
  nonZeroPlot <- ggplot(
    non_zero_values,
    aes(
      x = factor(ParameterName), # Use ParameterName as the x-axis
      y = Result, # Use Result as the y-axis
      fill = factor(ParameterName) # Fill color based on ParameterName
    )
  ) +
    geom_violin(trim = FALSE, fill = "#e3ebfa", color = "#a9c5f5") + # Create violin plot with light blue fill and border color
    geom_sina(alpha = 0.6, shape = 21, fill = NA) + # Add sina plot for non-zero values with some transparency
    geom_point(
      data = summary_stats,
      aes(x = factor(ParameterName), y = mean_Result),
      color = "blue", # Highlight mean with blue color
      stroke = 1.25,          # Thickness of the outline (adjust as desired)
      size = 2,
      shape = 21,
      fill = "lightblue" # was NA for Hollow point for mean
    ) +
    geom_point(
      data = summary_stats,
      aes(x = factor(ParameterName), y = median_Result),
      color = "#008000", # Highlight median with green color
      stroke = 1.25,          # Thickness of the outline (adjust as desired)
      size = 3,
      shape = 21,
      fill = "lightgreen" # was NA Hollow point for median
    ) +
    geom_point(
      data = highlight_point,
      aes(x = factor(ParameterName), y = Result),
      color = "red", # Highlight the specific subject with red color
      stroke = 1.25,          # Thickness of the outline (adjust as desired)
      size = 4,
      shape = 21,
      fill = "#ff6969" # now set to RED to fill circle, was NA for Hollow point for the highlighted subject
    ) +
    theme_minimal() + # Set minimal theme for the plot
    theme(
      plot.title = element_markdown(size = 14, face = "bold") # Set plot title styling with markdown support for rich text
    ) +
    scale_fill_brewer(palette = "Pastel1") + # Use a pastel color palette for the violin plot fill
    theme_minimal(base_size = 15) + # Set minimal theme with base font size
    theme(legend.position = "none") # Remove legend from the plot

  # Add log scale if required
  if (logScale) {
    nonZeroPlot <- nonZeroPlot + scale_y_log10(labels = scales::comma_format(big.mark = ",")) +
      labs(
        title = paste0("<span style='color:blue;'>Mean</span>, <span style='color:green;'>Median</span>, and <span style='color:red;'>Your</span> exposure to ", chemOfConcern), # Add color-coded title for mean, median, and subject, including chemOfConcern

        y = "Nanograms per Gram Silicone \nNumbers get rapidly bigger towards the right of the graph due to use of 'Log Scale.' \n", # Add y-axis label with explanation
        x = "" # Remove x-axis label
      )


    nonZeroPlot <- nonZeroPlot + coord_flip() # Flip coordinates to switch x and y axes
  } else {
    # Create an empty plot if there are no non-zero values
    nonZeroPlot <- ggplot() +
      theme_void() + # Set an empty theme
      labs(title = "No Non-Zero Values Available") # Indicate that there are no non-zero values available
  }

  # Convert each plot to an interactive plotly plot
  zeroPlot_interactive <- ggplotly(zeroPlot) %>%
    style(hoverinfo = "none") # # Remove hover tooltips for zero plot

  # Convert non-zero plot to an interactive plotly plot if non-zero values exist
    text_MEDIAN_label <- paste("Median Result:", round(summary_stats$median_Result, 2)) # Prepare label for median hover text
    text_MEAN_label <- paste("Mean Result:", round(summary_stats$mean_Result, 2)) # Prepare label for mean hover text
    text_YOURDATA_label <- paste("Your Result:", round(highlight_point$Result, 2)) # Prepare label for subject hover text

    nonZeroPlot_interactive <- ggplotly(nonZeroPlot) %>%
      style(hoverinfo = "none", traces = c(1, 2)) %>% # Remove hover tooltips for the violin and sina plots
      style(text = text_MEAN_label, traces = 3) %>% # Add hover text for mean points
      style(text = text_MEDIAN_label, traces = 4) %>% # Add hover text for median points
      style(text = text_YOURDATA_label, traces = 5)      # Add hover text for subject point

  # Combine the two interactive plots using subplot with adjusted widths
  chemPlot_interactive <- if (nrow(zero_values) > 0) {
    subplot(zeroPlot_interactive, nonZeroPlot_interactive, nrows = 1, shareY = TRUE, titleX = TRUE, widths = c(0.1, 0.9)) # Combine zero and non-zero plots, giving more space to non-zero plot
  } else {
    subplot(nonZeroPlot_interactive, nrows = 1, shareY = TRUE, titleX = TRUE, widths = c(1)) # If no zero values, use only non-zero plot
  }

  # Apply config only to the combined plot
  chemPlot_interactive <- chemPlot_interactive %>%
    config(
      displayModeBar = TRUE,
      displaylogo = FALSE,
      modeBarButtonsToRemove = list(
        "zoom2d",
        "pan2d",
        "select2d",
        "lasso2d",
        "zoomIn2d",
        "zoomOut2d",
        "autoScale2d",
        "resetScale2d",
        "hoverClosestCartesian",
        "hoverCompareCartesian"
      ),
      toImageButtonOptions = list(format = "png")
    )

  return(chemPlot_interactive) # Return the combined interactive plot
}



## NEW VERSION generating an output list
# Enhanced Output List:
#
# The output_list now includes:
#    _cat_message: The cat-style message.
#    chemItem: The tab content (HTML).
#    _message: The explanatory text.
#    _id: The auto-generated ID for the tab.


##chemsList <-chemsNOTinConcernGroup
#chemItem <- chemsList[1]
#chemItem <- chemsList[2]
#chemItem <- chemsList[3]


plotlyChems <- function(chemsList, testResults.big, oneResultCalifProp65Risk, oneResultEpaIris, oneResultIARCRisk, chemSourceMitigation, subject, howManyHaveParameterName, howManyWristbandsTested) {
  # Initialize a list to collect all generated content
  output_list <- list()

  if (length(chemsList) >= 1) {
    for (i in 1:length(chemsList)) {
      chemItem <- chemsList[i]

      # Filter data for the current chemical
      testResults_ChemItem <- testResults.big %>%
        filter(ParameterName == chemItem) %>%
        select(SampleNumber, Result, ParameterName)

      # Add the "cat" message (e.g., the tab header)
      output_list[[paste0(chemItem, "_cat_message")]] <- paste0("### ", prepareTabTitle(chemItem), " {-}\n\n")

      # Generate the explanatory message
      newMessage <- paste(
        chemItem,
        "was found on",
        lookup_non_zero_count(chemItem, howManyHaveParameterName),
        "out of the",
        howManyWristbandsTested,
        "wristbands."
      )

      # Add additional information if available
      testInPROP65 <- oneResultCalifProp65Risk %>%
        filter(`Chemical Name` == chemItem)
      if (nrow(testInPROP65) > 0) {
        PROP65_info <- str_trim(testInPROP65$`Risk Type Per California Prop 65`)
        Prop65_Link <- makeClickableURL("http://www.oehha.ca.gov/prop65.html", "California Proposition 65")

        newMessage <- paste(
          newMessage,
          "This chemical is classified per",
          Prop65_Link,
          "with the risk type noted as:",
          PROP65_info,
          "."
        )
      }

      testInIRIS <- oneResultEpaIris %>%
        filter(`Chemical Name` == chemItem)
      if (nrow(testInIRIS) > 0) {
        IRIS_info <- makeClickableURL(testInIRIS$`IRIS Summary Web Link`, "chemical of interest")
        newMessage <- paste(
          newMessage,
          "The Environmental Protection Agency has classified this chemical as a",
          IRIS_info,
          "."
        )
      }

      testInChemSourceMitigation <- chemSourceMitigation %>%
        filter(Chemical_Name == chemItem)
      if (nrow(testInChemSourceMitigation) > 0) {
        chemSourceMitigationMessage <- paste0(
          "Sources of Exposure to this compound: **",
          testInChemSourceMitigation$Sources_of_Exposure,
          "**. Possible ways to avoid exposure to this compound include: **",
          testInChemSourceMitigation$Mitigation_Strategies,
          "**."
        )
        if (!is.na(testInChemSourceMitigation$WIKIPEDIA_ARTICLE_URL)) {
          wikiURL <- makeClickableURL(testInChemSourceMitigation$WIKIPEDIA_ARTICLE_URL, "Wiki")
          chemSourceMitigationMessage <- paste0(chemSourceMitigationMessage, " Additional information: ", wikiURL)
        }

        newMessage <- paste(newMessage, chemSourceMitigationMessage)
      }

      # Add the explanatory message to the output list
      output_list[[paste0(chemItem, "_message")]] <- newMessage

      # Create tab content (Plotly Sina plot or placeholder)
      if (nrow(testResults_ChemItem[testResults_ChemItem$Result > 0, ]) > 1) {
        # Plotly Sina plot content
        htmlTagList <- htmltools::tagList(
          htmltools::tags$div(
            buildPlotlySina(chemItem, testResults_ChemItem, subject)
          )
        )
      } else {
        # Placeholder content for insufficient data
        htmlTagList <- htmltools::tagList(
          htmltools::tags$div(
            style = "display: flex; justify-content: center; align-items: center; height: 100%; padding: 50px; text-align: center;",
            htmltools::tags$p(
              "Insufficient data to display meaningful chart.",
              style = "font-size: 36px; font-weight: bold; color:blue; margin: 50px 0;"
            )
          )
        )
      }

      # Render the HTML and attempt to extract the auto-generated ID
      rendered_html <- htmltools::renderTags(htmlTagList)

      if (!is.null(rendered_html$html)) {
        # Extract ID from the rendered HTML
        matches <- regmatches(rendered_html$html, regexpr("id=\"[^\"]+\"", rendered_html$html))
        if (length(matches) > 0) {
          auto_generated_id <- sub("id=\"", "", sub("\"$", "", matches))
        } else {
          # If no ID is found, generate one manually
          auto_generated_id <- paste0("tab-", gsub(" ", "-", tolower(chemItem)))
        }
      } else {
        # Fallback for empty or invalid rendering
        auto_generated_id <- paste0("tab-", gsub(" ", "-", tolower(chemItem)))
      }

      # Store the ID in the output list
      output_list[[paste0(chemItem, "_id")]] <- auto_generated_id

      # Add the tab content to the output list
      output_list[[chemItem]] <- htmlTagList
    }
  }

  # Return the output list containing all generated tabs, messages, and IDs
  return(output_list)
}




