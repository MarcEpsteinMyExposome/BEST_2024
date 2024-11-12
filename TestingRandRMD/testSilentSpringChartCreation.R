# Test New Plot Format
library(ggplot2)
library(dplyr)
library(plotly)

# Prepare the data
# Filter for positive values and for a specific ParameterName to plot
plot_data <- testResults.big %>%
  filter(Result > 0, ParameterName == "1,6-Dimethylnaphthalene")

# Identify the individual result to highlight (assuming it’s a specific "SampleNumber")
highlighted_sample <- "40-WB"  # Replace with the specific sample to highlight
highlighted_data <- plot_data %>% filter(SampleNumber == highlighted_sample)

# Add a column for tooltip text
highlighted_data <- highlighted_data %>% mutate(text = paste("Your result:", Result, "ng/g/week"))

# Create a subset of data for "not detected" values (Result == 0)
not_detected_data <- testResults.big %>% filter(Result == 0, ParameterName == "1,6-Dimethylnaphthalene")

# Create the plot with ggplot2
gg <- ggplot() +
  # Plot "not detected" values as grey points off to the far left side (use x = 0.1 to separate from log scale)
  geom_jitter(data = not_detected_data, aes(x = 0.1, y = ParameterName), color = "grey", alpha = 0.3, width = 0.1, height = 0.2) +

  # Plot all positive data points on the log scale
  geom_jitter(data = plot_data, aes(x = Result, y = ParameterName), color = "blue", alpha = 0.5, width = 0, height = 0.2, size = 3) +

  # Highlight the individual point with a different color and size (make it red and larger)
  geom_point(data = highlighted_data, aes(x = Result, y = ParameterName, text = text), color = "red", size = 4.5) +

  # Set x-axis to log scale with specific breaks for readability
  scale_x_log10(breaks = c(10, 100, 1000, 10000, 100000), labels = scales::comma) +

  # Customize labels and theme
  labs(
    title = "Chemical Exposure Results for 1,6-Dimethylnaphthalene",
    x = "ng/g/week (Log Scale)",
    y = "Chemical (ParameterName)"
  ) +
  theme_minimal() +
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_text(vjust = 0),  # Move x-axis title further down
    panel.grid.major = element_blank(),  # Remove grid lines for a cleaner look
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(size = 0.5, colour = "black", lineend = "butt"),  # Show the x-axis line
    axis.text.x = element_text(vjust = -0.1)  # Move x-axis labels below the line
  )

# Convert the ggplot to a plotly interactive plot
interactive_plot <- ggplotly(gg, tooltip = 'text') %>%
  style(hoverinfo = 'none', traces = c(1, 2)) %>%
  style(hoverinfo = 'text', traces = 3, hoverlabel = list(bgcolor = 'white'))

# Render the interactive plot
interactive_plot

