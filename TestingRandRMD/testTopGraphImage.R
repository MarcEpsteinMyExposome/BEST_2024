library(ggplot2)
library(ggtext)

# Prepare test data using mtcars mpg:
min_val    <- min(mtcars$mpg)
median_val <- median(mtcars$mpg)
max_val    <- max(mtcars$mpg)
you_val    <- mean(mtcars$mpg)

data <- data.frame(
  value = c(min_val, median_val, max_val, you_val),
  color = c("blue", "green", "red", "purple"),
  label = c("Minimum", "Median", "Maximum", "You")
)

# Define label vertical positions:
data$label_y <- ifelse(data$label == "You", 1.015, 1.01)

# Set x-axis parameters:
max_value <- ceiling(max_val) + 5  # add a little margin
increment <- 5

# Title message:
howManyChemicalsFoundThisWristband <- 4
numChemMessage <- paste0("The number of chemicals found in your wristband was **",
                         howManyChemicalsFoundThisWristband, "**.")

# Create triangle polygons for Minimum, Median, and Maximum:
triangle_data <- subset(data, label != "You")
triangle_points <- do.call(rbind, lapply(1:nrow(triangle_data), function(i) {
  v <- triangle_data$value[i]
  data.frame(
    x = c(v, v - 0.2, v + 0.2),
    y = c(1, 1 + 0.01, 1 + 0.01),
    group = triangle_data$label[i]
  )
}))

# Build the plot:
p <- ggplot() +
  # Draw the horizontal numberline:
  geom_hline(yintercept = 1, color = "black", linetype = "solid", linewidth = 1.5) +

  # Add triangles for Minimum/Median/Maximum:
  geom_polygon(data = triangle_points, aes(x = x, y = y, group = group),
               fill = "black") +

  # Add a point for "You":
  geom_point(data = subset(data, label == "You"),
             aes(x = value), y = 1,
             shape = 16, size = 3, color = "purple") +

  # Add labels for all points:
  geom_label(data = data,
             aes(x = value, y = label_y, label = label, color = color),
             fill = "white", label.size = 0.5) +

  # For "You", add a vertical segment connecting the point on the line to its label:
  geom_segment(data = subset(data, label == "You"),
               aes(x = value, xend = value),
               y = 1, yend = 1.013, color = "purple", linewidth = 0.8) +

  # Custom x-axis numbering: add text labels just under the numberline:
  geom_text(data = data.frame(x = seq(0, max_value, by = increment)),
            aes(x = x, y = 0.995, label = x), size = 3) +

  scale_color_identity() +
  scale_x_continuous(breaks = seq(0, max_value, by = increment),
                     limits = c(0, max_value)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = numChemMessage,
    x = "Number of chemicals",
    y = ""
  ) +
  # Prevent clipping of elements outside the plot panel:
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(
    plot.title   = element_markdown(),
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    aspect.ratio = 0.1,
    plot.margin = unit(c(0.2, 0.2, 0.5, 0.2), "cm")
  )

print(p)
