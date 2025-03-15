# Updated version to handle zero values with two distinct panels using patchwork
### ONLY tested with calling SINA function
# Updated version to handle zero values with two distinct panels using patchwork

# Function to build an interactive Sina plot with separate panels for zero and non-zero values
#  buildPlotlySina(chemItem, testResults_ChemItem, subject)
### NOTE that SUBJECT can be NULL and if is null then need to suppress all the subject specific stuff
#
buildPlotlySina <- function(chemOfConcern, testResults_ChemOfConcern, subject) { ###
  # chemOfConcern<- chemItem
  # testResults_ChemOfConcern <-testResults_ChemItem
  #subject<-"A240956"
  logScale <- TRUE # Set the log scale option to TRUE for plotting

  # Separate zero and non-zero values
  zero_values <- testResults_ChemOfConcern %>% filter(Result == 0) # Filter out rows with zero values for separate handling
  non_zero_values <- testResults_ChemOfConcern %>% filter(Result > 0) # Filter out rows with non-zero values for plotting

  ### THIS IS A HACK just showing one particular value as "Your Value" but it is not really your value.
  #subject <- non_zero_values[1]$SampleNumber


  # Add a column to represent the x-axis as "Zero" if zero values exist
  if (nrow(zero_values) > 0) {
    zero_values <- zero_values %>%
      mutate(ZeroLabel = "Zero") # Create a new column to represent zero values on the x-axis
  }

  if(!is.null(subject)){
    # Highlight the specific point for non-zero values (the subject's value)
    highlight_point <- non_zero_values[non_zero_values$SampleNumber == subject, ] # Extract the specific point to highlight based on the subject identifier
    if (nrow(highlight_point) == 0) {
      warning("Subject SampleNumber not found in non-zero values.")
    }
  } else {  # HERE SUBJECT IS NULL SO WE DO NOT DO ANYTHING
    # putting this here just to demonstrate we know we are leaving highlight_point undefined
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
  if (nrow(non_zero_values) > 1) {  # THIS SHOULD ALWAYS BE TRUE.  really should throw error if called with 1 or 0 data points
    nonZeroPlot <- ggplot(
      non_zero_values,
      aes(
        x = factor(ParameterName), # Use ParameterName as the x-axis
        y = Result, # Use Result as the y-axis
        fill = factor(ParameterName)    # Fill color based on ParameterName
      )
    ) # + geom_point(size = 3) # Add points with a specific size for visibility
    nonZeroPlot <- nonZeroPlot+
      geom_violin(trim = FALSE, fill = "#e3ebfa", color = "#a9c5f5") + # Create violin plot with light blue fill and border color
      geom_sina(alpha = 0.6,    #THIS WORKED previously.  TRIED below to do something else with shape but that didn't work at all
                shape = 21,
                fill = NA) + # Add sina plot for non-zero values with some transparency
      # geom_sina(
      #   aes(shape = factor(Lab_Submission_Batch)), # Shape based on Lab_Submission_Batch
      #   alpha = 0.6,
      #   fill = NA
      # ) + # Add sina plot with transparency and shape variation
      geom_point(
        data = summary_stats,
        aes(x = factor(ParameterName), y = mean_Result),
        color = "blue", # Highlight mean with blue color
        stroke = 1.25, # Thickness of the outline (adjust as desired)
        size = 2,
        shape = 21,
        fill = "lightblue" # was NA for Hollow point for mean
      )
    nonZeroPlot <- nonZeroPlot+
      geom_point(
        data = summary_stats,
        aes(x = factor(ParameterName), y = median_Result),
        color = "#008000", # Highlight median with green color
        stroke = 1.25, # Thickness of the outline (adjust as desired)
        size = 3,
        shape = 21,
        fill = "lightgreen" # was NA Hollow point for median
      )
    if (!is.null(subject)) {  # ADD HIGHLIGHT IFF we  have a non-null subject
      nonZeroPlot <- nonZeroPlot +
        geom_point(
          data = highlight_point,
          aes(x = factor(ParameterName), y = Result),
          color = "darkmagenta",
          # Highlight the specific subject with red color
          stroke = 1.25,
          # Thickness of the outline (adjust as desired)
          size = 4,
          shape = 21,
          fill = "magenta" # now set to RED to fill circle, was NA for Hollow point for the highlighted subject
        )
    }
    nonZeroPlot <- nonZeroPlot+
      theme_minimal() + # Set minimal theme for the plot
      theme(
        plot.title = element_markdown(size = 14, face = "bold") # Set plot title styling with markdown support for rich text
      ) +
      scale_fill_brewer(palette = "Pastel1") + # Use a pastel color palette for the violin plot fill
      theme_minimal(base_size = 15) + # Set minimal theme with base font size
      theme(legend.position = "none") # Remove legend from the plot

    # Add log scale if required
    if (logScale) {
      if (is.null(subject)){
        colorizedTitle = paste0("<span style='color:blue;'>Mean</span>, <span style='color:green;'>Median</span> exposure to ", chemOfConcern) # Add color-coded title for mean, median, , including chemOfConcern
      } else {
        colorizedTitle = paste0("<span style='color:blue;'>Mean</span>, <span style='color:green;'>Median</span>, and <span style='color:magenta;'>Your</span> exposure to ", chemOfConcern) # Add color-coded title for mean, median, and subject, including chemOfConcern
      }

      nonZeroPlot <- nonZeroPlot + scale_y_log10(labels = scales::comma_format(big.mark = ",")) +
        labs(
         title = colorizedTitle, # Add color-coded title for mean, median, and subject, including chemOfConcern.  DO NOT do subject if subject NULL
         # y = "Nanograms per Gram Silicone \nNumbers get rapidly bigger towards the right of the graph due to use of 'Log Scale.' \n", # Add y-axis label with explanation
         y = "Nanograms per Gram Silicone\n<span style='font-size:10pt;'>Numbers get rapidly bigger towards the right of the graph due to use of 'Log Scale.'</span>",
         x = "" # Remove x-axis label
        )
    }

    # THIS MAKES NOT SENSE.  if log scale we do the message but we should coord flip if or if not log scale and we're missing the nonzeroPLOT being created properly...
    nonZeroPlot <- nonZeroPlot + coord_flip() # Flip coordinates to switch x and y axes   #### CAUTION:  WHY DOES THIS GIVE ERROR about scale for Y already pre... SHOULD THIS BE DONE TO the EMPTY PLOT???? or to (most likely) the plot even if not log scale?
  } else {  # THIS SHOULD NEVER EVER EVER happen
    # Create an empty plot if there are no non-zero values
    nonZeroPlot <- ggplot() +
      theme_void() + # Set an empty theme
      labs(title = "Only One Value Available.  No Plot Provided.") # Indicate that there is only one  values available
  }

  # Convert each plot to an interactive plotly plot
  zeroPlot_interactive <- ggplotly(zeroPlot) %>%
    style(hoverinfo = "none") # # Remove hover tooltips for zero plot

  # Convert non-zero plot to an interactive plotly plot if non-zero values exist
  text_MEDIAN_label <- paste("Median Result:", round(summary_stats$median_Result, 2)) # Prepare label for median hover text
  text_MEAN_label <- paste("Mean Result:", round(summary_stats$mean_Result, 2)) # Prepare label for mean hover text
  if(!is.null(subject)){
    text_YOURDATA_label <- paste("Your Result:", round(highlight_point$Result, 2)) # Prepare label for subject hover text
  }


  nonZeroPlot_interactive <- ggplotly(nonZeroPlot) %>%
    style(hoverinfo = "none", traces = c(1, 2)) %>% # Remove hover tooltips for the violin and sina plots
    style(text = text_MEAN_label, traces = 3) %>% # Add hover text for mean points
    style(text = text_MEDIAN_label, traces = 4) # Add hover text for median points
  if (!is.null(subject)) {
    nonZeroPlot_interactive <- nonZeroPlot_interactive %>%
      style(text = text_YOURDATA_label, traces = 5) # Add hover text for subject point
  }
  nonZeroPlot_interactive <- nonZeroPlot_interactive %>%  layout(
    autosize = TRUE,
    # Automatically adjust size
    width = NULL,
    # Remove fixed width
    height = NULL,
    # Remove fixed height
    margin = list(
      l = 50,
      r = 50,
      t = 50,
      b = 50
    ) # Customize chart margins
  )

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

  ## Having problem with chart going off the page on right side
  #   tried to use layout on previous ggplotly calls but that didn't work
  #     but maybe doing it here AFTER combining the charts will work better?
  chemPlot_interactive <- chemPlot_interactive %>%
    layout(
      autosize = TRUE,
      width = NULL,
      height = NULL
    )

  return(chemPlot_interactive) # Return the combined interactive plot
}


# Function to create a checkbox table styled with 'gt'
# This function dynamically generates a table with headers, styles, and optional modal links.
# TODO:  Consider putting function in separate file
#' Create a Checkbox Table with GT Styling
#'
#' This function dynamically generates a table with styled headers, checkboxes, and optional modal links.
#'
#' @param labels Character vector. The column labels for the table.
#' @param checkbox_states Logical vector. TRUE/FALSE states for each checkbox (corresponding to labels).
#' @param description Character. The description text to show in the table header.
#' @param modal_trigger HTML. An optional modal trigger element to append to the description.
#'
#' @return A gt table object with formatted checkboxes and styling.
#'
#' @examples
#' \dontrun{
#' labels <- c("Reproduction", "Brain", "Cancer")
#' states <- c(TRUE, FALSE, TRUE)
#' desc <- "Possible health effects"
#' modal <- htmltools::tags$span(class="info-icon", "ⓘ")
#' create_checkbox_table(labels, states, desc, modal)
#' }
#'
create_checkbox_table <- function(labels, checkbox_states, description, modal_trigger) {
  # Map logical states to symbols for checkboxes
  values <- ifelse(checkbox_states, "☑", "")

  # Create a data frame for the table
  table_data <- as.data.frame(t(values)) # Transpose to make rows columns
  colnames(table_data) <- labels # Assign labels to columns

  # Include the modal trigger in the description
  full_description <- paste0(description, " ", as.character(modal_trigger))

  # Format the table using 'gt'
  formatted_table <- table_data %>%
    gt() %>%
    tab_header(
      title = md(full_description) # Add description and info icon to header
    ) %>%
    # Apply styles to title, column labels, and body
    tab_style(
      style = list(
        cell_fill(color = "lightblue"),
        cell_text(weight = "bold", align = "center")
      ),
      locations = list(
        cells_column_labels(),
        cells_title(groups = "title")
      )
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = "lightblue"),
        cell_text(align = "center")
      ),
      locations = cells_body(rows = 1)
    ) %>%
    # Add consistent border styling to the table
    tab_style(
      style = list(
        cell_borders(sides = "bottom", color = "black", weight = px(2))
      ),
      locations = cells_title(groups = "title")
    ) %>%
    tab_options(
      table.border.top.color = "black",
      table.border.bottom.color = "black",
      heading.border.bottom.color = "black",
      column_labels.border.bottom.color = "black"
    )
  return(formatted_table)
}


## NEW VERSION generating an output list
# Enhanced Output List:
#
# The output_list now includes:
#    _cat_message: The cat-style message which is the TAB HEADER INFORMATION
#    chemItem: The tab content (HTML).
#    _message: The explanatory text.
#    _id: The auto-generated ID for the tab.


## chemsList <-chemsNOTinConcernGroup
# chemItem <- chemsList[1]
# chemItem <- chemsList[2]
# chemItem <- chemsList[3]

# chemsList<-chemsOfConcern
# plotlyChems has the following variables passed into it:
#   chemsList,   :  JUST a list of all chemicals to do report about
#   testResults.big, :  ALL the collected data
#   oneResultCalifProp65Risk, : initially this is Prop65 FOR ONE SUBJECT but... can it be all prop65?
#   oneResultEpaIris, , : initially this is EPAIRIS FOR ONE SUBJECT but... can it be all EPAIRIS?
#   oneResultIARCRisk, Initially this is IARCrisk FOR ONE SUBJECT but... can it be all IARCrisk?
#   chemSourceMitigation, : FULL table for mitigation info including silent spring
#   subject, :          THE SUBJECT NAME (cant it also just be NULL to mean do-for-everyone?)
#   howManyHaveParameterName,
#   howManyWristbandsTested,
#   modal_trigger_test2


#GROUP CALL:
#plotlyC_Group_remaining <- plotlyChems(chemsGroupNOTinConcernGroup, testResults.big, oneResultCalifProp65Risk, oneResultEpaIris, oneResultIARCRisk, chemSourceMitigation, subject, howManyHaveParameterName, howManyWristbandsTested, modal_trigger_test3)
#plotlyC_concern <- plotlyChems(chemsOfConcern, testResults.big, oneResultCalifProp65Risk, oneResultEpaIris, oneResultIARCRisk, chemSourceMitigation, subject, howManyHaveParameterName, howManyWristbandsTested,modal_trigger_test2)
#plotlyC_Group_concern <-   plotlyChems(chemsOfGroupConcern,         testResults.big,  CalifProp65Hits,          EPAirisHits,      IARCHits,          chemSourceMitigation, NULL, howManyHaveParameterName, howManyWristbandsTested,  modal_trigger_test3)



#' Generate Plotly Chemical Visualizations
#'
#' Creates a list of visualizations and associated metadata for a list of chemicals.
#'
#' @param chemsList Character vector. List of chemicals to visualize.
#' @param testResults.big Data frame. Full test results dataset.
#' @param oneResultCalifProp65Risk Data frame. California Prop 65 risk information.
#' @param oneResultEpaIris Data frame. EPA IRIS risk information.
#' @param oneResultIARCRisk Data frame. IARC risk information.
#' @param chemSourceMitigation Data frame. Chemical source and mitigation information.
#' @param subject Character. Optional subject ID to highlight. Can be NULL for group analysis.
#' @param howManyHaveParameterName Data frame. Count of how many samples have each parameter.
#' @param howManyWristbandsTested Numeric. Total number of wristbands tested.
#' @param modal_trigger_test2 HTML. Modal trigger element for the info icon.
#' @param buildPlotlySina Function. Function to build the Sina plot visualization.
#' @param makeClickableURL Function. Function to create clickable URL links.
#' @param prepareTabTitle Function. Function to prepare tab titles.
#'
#' @return A list containing visualization components and metadata for each chemical.
#'
#' @examples
#' \dontrun{
#' results <- plotlyChems(chemsList, testResults.big, prop65Risk, epaIris, iarcRisk,
#'                        chemSourceMitigation, "A123", howMany, totalWristbands,
#'                        modalTrigger, buildPlotlySina, makeClickableURL, prepareTabTitle)
#' }
#'
plotlyChems <- function(chemsList, testResults.big, oneResultCalifProp65Risk, oneResultEpaIris,
                        oneResultIARCRisk, chemSourceMitigation, subject,
                        howManyHaveParameterName, howManyWristbandsTested, modal_trigger_test2,
                        buildPlotlySina, makeClickableURL, prepareTabTitle) {
  # SPECIAL HANDLING:  If "subject" is NULL (is.null(subject)) then we are doing GROUP CHART and not INDIVIDUAL CHART and so must suppress all that individual stuff
  # Initialize a list to collect all generated content
  output_list <- list()

  if (length(chemsList) >= 1) {
    for (i in 1:length(chemsList)) {
      chemItem <- chemsList[i]
      # Filter data for the current chemical
      testResults_ChemItem <- testResults.big %>%
        filter(ParameterName == chemItem) %>%
        select(SampleNumber, Result, ParameterName,endocrine_toxicity,respiratory_toxicity,carcinogenicity,genotoxicity,reproductive_toxicity,developmental_toxicity,neurotoxicity,pbt,calSaferURL,Lab_Submission_Batch)

      # Get the one row which is ths subject and this chem assuming we're working with specific subjectd OTHERWISE just get a representative ROW (first row)
      #   KEY POINT is that we're not actually using THIS for the SUBJECT information (i don't think) we're using this just to get other values that will be the same on every such row so pick one
      #### Really we can just pick the first one anyway?  We don't need to actually pick the subject row as far as I can see???
      if (is.null(subject) || (subject=="Never-Going-to-Match")) {
        testResults_ChemItem_AnyRow <- testResults_ChemItem %>%
          filter(Result>0)  %>%
          slice(1)# Grab just the first row.  we only need the toxicity values and calSafer URL
      } else {
        testResults_ChemItem_AnyRow <- testResults_ChemItem %>%
          filter(SampleNumber == subject)
      }

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

      #calSaferURL
      if(!is.na(testResults_ChemItem_AnyRow$calSaferURL)){
        calSafer_Link <- makeClickableURL(testResults_ChemItem_AnyRow$calSaferURL, "calSAFER Chemical Link")
        newMessage <- paste(
          newMessage,
          "This chemical is classified per the California Department of Toxic Substances Control here: ",
          calSafer_Link,
          "."
        )
      }

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

      ### SETUP STUFF FOR RISK TABLE...
      # Example: Define labels and states for the risk table.
      # These labels and states should reflect actual use-case data.
      # Define labels, states, and description
      labels <- c("Reproduction and fertility", "Brain and behavior", "Increased Cancer Risk", "Hormone disruption", "Development","PBT", "Harms DNA","Respiratory")

      # REPLACE WITH REAL VALUES
      #endocrine_toxicity,respiratory_toxicity,carcinogenicity,genotoxicity,reproductive_toxicity,developmental_toxicity,neurotoxicity,pbt
      repro_TrueFalse <-  testResults_ChemItem_AnyRow$reproductive_toxicity
      brain_TrueFalse <-  testResults_ChemItem_AnyRow$neurotoxicity
      cancer_TrueFalse <-  testResults_ChemItem_AnyRow$carcinogenicity
      hormone_TrueFalse <-  testResults_ChemItem_AnyRow$endocrine_toxicity
      develop_TrueFalse <-  testResults_ChemItem_AnyRow$developmental_toxicity
      pbt_TrueFalse <- testResults_ChemItem_AnyRow$pbt
      dna_TrueFalse <- testResults_ChemItem_AnyRow$genotoxicity
      respir_TrueFalse <-  testResults_ChemItem_AnyRow$respiratory_toxicity

      checkbox_states <- c(repro_TrueFalse, brain_TrueFalse, cancer_TrueFalse, hormone_TrueFalse, develop_TrueFalse,pbt_TrueFalse, dna_TrueFalse,respir_TrueFalse)
      description <- "Possible health effects depending on length of time and size of exposure"

      ##  Use table WITHOUT the icon and check box

      #risk_table <- create_checkbox_table(labels, checkbox_states, description,modal_trigger_test3 )
      risk_table <- create_checkbox_table(labels, checkbox_states, description,modal_trigger_test2 )
      risk_table <- risk_table %>%
        tab_style(
          style = list(
            cell_borders(
              sides = "all", # Apply to all sides of the cells
              color = "black", # Use black for the border color
              weight = px(1) # Border weight in pixels
            )
          ),
          locations = cells_body() # Apply to all body cells
        )
      risk_table <- risk_table %>%
        tab_style(
          style = list(
            cell_borders(
              sides = "all",
              color = "black",
              weight = px(1)
            )
          ),
          locations = cells_column_labels() # Apply to column labels
        )

      risk_table_html <- htmltools::tagList(
        # Modal for the current chemical
        # The table
        gt::as_raw_html(risk_table)
      )
      # Add to output list
      output_list[[paste0(chemItem, "_health_risk_table")]] <- risk_table_html

      # Create tab content (Plotly Sina plot or placeholder)
      if (nrow(testResults_ChemItem[testResults_ChemItem$Result > 0, ]) > 1) {   ### If there is more than one data point!
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
