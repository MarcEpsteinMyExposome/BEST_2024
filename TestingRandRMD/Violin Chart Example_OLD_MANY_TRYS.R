# Experiment with VIOLIN PLOT
#
#
# VErsion 1:  https://r-graph-gallery.com/violin_horizontal_ggplot2.html
# Libraries

install.packages("hrbrthemes", dependencies = TRUE)
install.packages("viridis", dependencies = TRUE)
install.packages("Hmisc", dependencies = TRUE)
install.packages("plotly", dependencies = TRUE)



library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(hrbrthemes)
library(viridis)
library(Hmisc)

# Load dataset from github
data <- read.table("https://raw.githubusercontent.com/zonination/perceptions/master/probly.csv", header=TRUE, sep=",")

# Data is at wide format, we need to make it 'tidy' or 'long'
data <- data %>%
  gather(key="text", value="value") %>%
  mutate(text = gsub("\\.", " ",text)) %>%
  mutate(value = round(as.numeric(value),0)) %>%
  filter(text %in% c("Almost Certainly","Very Good Chance","We Believe","Likely","About Even", "Little Chance", "Chances Are Slight", "Almost No Chance"))

# Plot
p <- data %>%
  mutate(text = fct_reorder(text, value)) %>% # Reorder data
  ggplot( aes(x=text, y=value, fill=text, color=text)) +
  geom_violin(width=2.1, size=0.2) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none"
  ) +
  coord_flip() + # This switch X and Y axis and allows to get the horizontal version
  xlab("") +
  ylab("Assigned Probability (%)")

p

p + stat_summary(fun.y=mean, geom="point", shape=23, size=2,color="white")
p + stat_summary(fun.y=median, geom="point", size=2, color="red")


p + stat_summary(fun.y=mean, geom="point", shape=23, size=2,color="white") + stat_summary(fun.y=median, geom="point", size=2, color="red")

p + geom_boxplot(width=.5)


p + stat_summary(fun.data="mean_sdl", mult=2, geom="crossbar", width=0.2)
p + stat_summary(fun.data=mean_sdl, mult=2, geom="pointrange", color="red")

# 0.2 : degree of jitter in x direction (i think in the Y direction... cause i did cord-flip?)
p + geom_jitter(shape=16, position=position_jitter(0.2,0))

p + theme(legend.position="top")
#=======================
#=======================#=======================
#=======================

# New try using ChatGPT

library(ggplot2)

# Load the mtcars dataset
data("mtcars")

# Create the refined violin plot
ggplot(mtcars, aes(x=factor(cyl), y=mpg, fill=factor(cyl))) +
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, position=position_dodge(0.9)) +
  labs(title="Violin Plot of MPG by Number of Cylinders with Boxplot",
       x="Number of Cylinders",
       y="Miles per Gallon (MPG)") +
  scale_fill_brewer(palette="Pastel1") +
  theme_minimal(base_size=15) +
  theme(legend.position="none")


###
###
#Certainly! You can highlight and label a specific point on the violin plot
#by using the geom_point() and geom_text() functions from ggplot2.

#Here's an example where we'll highlight and label the point with the
#maximum mpg value for cars with 6 cylinders:

#Labeling the mean and median on the violin plot involves
#calculating these statistics for each group and then adding them to the plot
#using geom_point() and geom_text(). Here’s how you can do it:

#     Calculate the mean and median for each group.
#     Add these points and labels to the plot.


library(ggplot2)
library(dplyr)

# Load the mtcars dataset
data("mtcars")

# Calculate mean and median for each group
summary_stats <- mtcars %>%
  group_by(cyl) %>%
  summarise(mean_mpg = mean(mpg), median_mpg = median(mpg))


# Identify the specific point to highlight
highlight_point <- mtcars[mtcars$cyl == 6 & mtcars$mpg == max(mtcars$mpg[mtcars$cyl == 6]), ]


# Create the refined horizontal violin plot with mean and median labels
ggplot(mtcars, aes(x=factor(cyl), y=mpg, fill=factor(cyl))) +
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, position=position_dodge(0.9)) +
  geom_point(data=highlight_point, aes(x=factor(cyl), y=mpg), color="red", size=3) +
  geom_text(data=highlight_point, aes(x=factor(cyl), y=mpg, label=mpg), vjust=-1.5, color="red", size=5) +
  geom_point(data=summary_stats, aes(x=factor(cyl), y=mean_mpg), color="blue", size=3) +
  geom_text(data=summary_stats, aes(x=factor(cyl), y=mean_mpg, label=round(mean_mpg, 2)), vjust=-1.5, color="blue", size=5) +
  geom_point(data=summary_stats, aes(x=factor(cyl), y=median_mpg), color="green", size=3) +
  geom_text(data=summary_stats, aes(x=factor(cyl), y=median_mpg, label=round(median_mpg, 2)), vjust=1.5, color="green", size=5) +
  labs(title="Horizontal Violin Plot of MPG by Number of Cylinders with Mean and Median",
       x="Number of Cylinders",
       y="Miles per Gallon (MPG)") +
  scale_fill_brewer(palette="Pastel1") +
  theme_minimal(base_size=15) +
  theme(legend.position="none") +
  coord_flip()

####
####To achieve this interactive behavior, you can use the plotly package,
#which allows for interactive plots where text appears when you hover over points.
#Here’s how you can create the plot with the ggplot2 base and make it interactive with plotly.

library(ggplot2)
library(dplyr)
library(plotly)

# Load the mtcars dataset
data("mtcars")

# Calculate mean and median for each group
summary_stats <- mtcars %>%
  group_by(cyl) %>%
  summarise(mean_mpg = mean(mpg), median_mpg = median(mpg))

# Identify the specific point to highlight
highlight_point <- mtcars[mtcars$cyl == 6 & mtcars$mpg == max(mtcars$mpg[mtcars$cyl == 6]), ]

# Create the base ggplot
p <- ggplot(mtcars, aes(x=factor(cyl), y=mpg, fill=factor(cyl))) +
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, position=position_dodge(0.9)) +
  geom_point(data=summary_stats, aes(x=factor(cyl), y=mean_mpg), color="blue", size=3) +
  geom_point(data=summary_stats, aes(x=factor(cyl), y=median_mpg), color="green", size=3) +
  geom_point(data=highlight_point, aes(x=factor(cyl), y=mpg), color="red", size=3) +
  labs(title="Horizontal Violin Plot of MPG by Number of Cylinders with Mean, Median, and Highlighted Point",
       x="Number of Cylinders",
       y="Miles per Gallon (MPG)") +
  scale_fill_brewer(palette="Pastel1") +
  theme_minimal(base_size=15) +
  theme(legend.position="none") +
  coord_flip()

# Convert to an interactive plot using plotly
p_interactive <- ggplotly(p, tooltip=c("x", "y"))

# Customize the tooltip to show the desired labels
p_interactive <- p_interactive %>%
  layout(hoverlabel = list(bgcolor = "white", font = list(size = 12))) %>%
  style(hoverinfo = "text", text = ~paste("Mean MPG: ", round(summary_stats$mean_mpg, 2),
                                          "<br>Median MPG: ", round(summary_stats$median_mpg, 2),
                                          "<br>Highlighted MPG: ", highlight_point$mpg))

# Display the interactive plot
p_interactive

# In this code:
#
#   We use dplyr to calculate the mean and median for each group and identify the maximum mpg value for cars with 6 cylinders.
# We create the base plot using ggplot2.
# We convert the plot to an interactive plot using ggplotly from the plotly package.
# We customize the tooltips to show the mean, median, and highlighted mpg values when you hover over the points.
# Run this code in your R environment to get an interactive plot where text appears only when you hover over the points. Let me know if you need further customization or adjustments!


####
####
###


# It seems the issue is with how the tooltips are being handled.
#To correctly add hover text to specific points in a ggplot2 plot and
#then convert it to a plotly interactive plot, we need to ensure the
#hover information is included in the aes mappings.
#
# Here is the revised approach:
#
#   Include the label aesthetic in the aes mappings for the points where you want hover text.
# Use the tooltip parameter correctly in the ggplotly function to display these labels.

#To ensure that only the specified hover text for the mean, median, and maximum points is shown,
#and not the default density tooltips, you need to adjust the tooltip parameter in the
#ggplotly function more precisely.
#One approach is to modify the hoverinfo for each layer in the plotly object,
#setting it to none for layers where you don't want any tooltips.

#In this code:

#  We ensure text=NULL for the geom_boxplot to prevent default tooltips.
#After converting to a plotly object, we loop through each layer and set hoverinfo to "none"
#for the violin and boxplot layers.
#We set hoverinfo to "text" for the points with the specified hover text.
#This should ensure that only the specified hover text for the
#mean, median, and highlighted maximum points appears, without the default density tooltips.
library(ggplot2)
library(dplyr)
library(plotly)

# Load the mtcars dataset
data("mtcars")

# Filter the dataset to include only 6-cylinder cars
mtcars_6cyl <- mtcars %>% filter(cyl == 6)

# Calculate mean and median for 6-cylinder cars
summary_stats_6cyl <- mtcars_6cyl %>%
  summarise(mean_mpg = mean(mpg), median_mpg = median(mpg))

# Identify the specific point to highlight
highlight_point_6cyl <- mtcars_6cyl %>% filter(mpg == max(mpg))

# Create the base ggplot with hover text for 6-cylinder cars
p <- ggplot(mtcars_6cyl, aes(x=factor(cyl), y=mpg, fill=factor(cyl))) +
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, position=position_dodge(0.9)) +
  geom_point(data=summary_stats_6cyl, aes(x=factor(6), y=mean_mpg, text=paste("Mean MPG:", round(mean_mpg, 2))), color="blue", size=3) +
  geom_point(data=summary_stats_6cyl, aes(x=factor(6), y=median_mpg, text=paste("Median MPG:", round(median_mpg, 2))), color="green", size=3) +
  geom_point(data=highlight_point_6cyl, aes(x=factor(6), y=mpg, text=paste("Max MPG:", mpg)), color="red", size=3) +
  labs(title="Horizontal Violin Plot of MPG for 6-Cylinder Cars with Mean, Median, and Highlighted Point",
       x="Number of Cylinders",
       y="Miles per Gallon (MPG)") +
  scale_fill_brewer(palette="Pastel1") +
  theme_minimal(base_size=15) +
  theme(legend.position="none") +
  coord_flip()

# Convert to an interactive plot using plotly
p_interactive <- ggplotly(p)

# Customize tooltips to only show for specific points
for (i in 1:length(p_interactive$x$data)) {
  if (grepl("violin|box", p_interactive$x$data[[i]]$type)) {
    p_interactive$x$data[[i]]$hoverinfo <- "none"
  } else {
    p_interactive$x$data[[i]]$hoverinfo <- "text"
  }
}

# Display the interactive plot
p_interactive

#######
#It appears that the ggplot2 version you are using might not recognize the text aesthetic
#directly in the geom_point function. We can address this by
#setting the tooltip information within the ggplotly function directly using the text attribute.


library(ggplot2)
library(dplyr)
library(plotly)

# Load the mtcars dataset
data("mtcars")

# Filter the dataset to include only 6-cylinder cars
mtcars_6cyl <- mtcars %>% filter(cyl == 6)

# Calculate mean and median for 6-cylinder cars
summary_stats_6cyl <- mtcars_6cyl %>%
  summarise(mean_mpg = mean(mpg), median_mpg = median(mpg))

# Identify the specific point to highlight
highlight_point_6cyl <- mtcars_6cyl %>% filter(mpg == max(mpg))

# Create the base ggplot for 6-cylinder cars
p <- ggplot(mtcars_6cyl, aes(x=factor(cyl), y=mpg, fill=factor(cyl))) +
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, position=position_dodge(0.9)) +
  geom_point(data=summary_stats_6cyl, aes(x=factor(6), y=mean_mpg), color="blue", size=3) +
  geom_point(data=summary_stats_6cyl, aes(x=factor(6), y=median_mpg), color="green", size=3) +
  geom_point(data=highlight_point_6cyl, aes(x=factor(6), y=mpg), color="red", size=3) +
  labs(title="Horizontal Violin Plot of MPG for 6-Cylinder Cars with Mean, Median, and Highlighted Point",
       x="Number of Cylinders",
       y="Miles per Gallon (MPG)") +
  scale_fill_brewer(palette="Pastel1") +
  theme_minimal(base_size=15) +
  theme(legend.position="none") +
  coord_flip()

# Convert to an interactive plot using plotly and customize tooltips
p_interactive <- ggplotly(p) %>%
  layout(hoverlabel = list(bgcolor = "white", font = list(size = 12)))

# Add custom hover text
p_interactive$x$data[[2]]$text <- paste("Mean MPG:", round(summary_stats_6cyl$mean_mpg, 2))
p_interactive$x$data[[2]]$hoverinfo <- "text"
p_interactive$x$data[[3]]$text <- paste("Median MPG:", round(summary_stats_6cyl$median_mpg, 2))
p_interactive$x$data[[3]]$hoverinfo <- "text"
p_interactive$x$data[[4]]$text <- paste("Max MPG:", highlight_point_6cyl$mpg)
p_interactive$x$data[[4]]$hoverinfo <- "text"

# Display the interactive plot
p_interactive

######### TRYING AGAIN to fix Cyl error
library(ggplot2)
library(dplyr)
library(plotly)

# Load the mtcars dataset
data("mtcars")

# Filter the dataset to include only 6-cylinder cars
mtcars_6cyl <- mtcars %>% filter(cyl == 6)

# Calculate mean and median for 6-cylinder cars
summary_stats_6cyl <- mtcars_6cyl %>%
  summarise(mean_mpg = mean(mpg), median_mpg = median(mpg))

# Identify the specific point to highlight
highlight_point_6cyl <- mtcars_6cyl %>% filter(mpg == max(mpg))

# Create the base ggplot for 6-cylinder cars
p <- ggplot(mtcars_6cyl, aes(x=factor(cyl), y=mpg, fill=factor(cyl))) +
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, position=position_dodge(0.9)) +
  #geom_point(data=summary_stats_6cyl, aes(x=factor(6), y=mean_mpg), color="blue", size=3) +
  #geom_point(data=summary_stats_6cyl, aes(x=factor(6), y=median_mpg), color="green", size=3) +
  #geom_point(data=highlight_point_6cyl, aes(x=factor(6), y=mpg), color="red", size=3) +
  labs(title="Horizontal Violin Plot of MPG for 6-Cylinder Cars with Mean, Median, and Highlighted Point",
       x="Number of Cylinders",
       y="Miles per Gallon (MPG)") +
  scale_fill_brewer(palette="Pastel1") +
  theme_minimal(base_size=15) +
  theme(legend.position="none") +
  coord_flip()

# Convert to an interactive plot using plotly
p_interactive <- ggplotly(p)

# Customize tooltips
p_interactive$x$data[[2]]$hoverinfo <- "none" # For violin plot
p_interactive$x$data[[3]]$hoverinfo <- "none" # For boxplot
p_interactive$x$data[[4]]$text <- paste("Mean MPG:", round(summary_stats_6cyl$mean_mpg, 2))
p_interactive$x$data[[4]]$hoverinfo <- "text"
p_interactive$x$data[[5]]$text <- paste("Median MPG:", round(summary_stats_6cyl$median_mpg, 2))
p_interactive$x$data[[5]]$hoverinfo <- "text"
p_interactive$x$data[[6]]$text <- paste("Max MPG:", highlight_point_6cyl$mpg)
p_interactive$x$data[[6]]$hoverinfo <- "text"

# Display the interactive plot
p_interactive

#======================================
  #===============================

### THIS ONE w hich is from EARLIER is actually the best one to start with.
# It has the pop-up tool tips
# the only problems are the ERROR MESSAGE WARNING
# and
# THE FACT that it shows some tooltips we don't want (like density)


# Experiment with VIOLIN PLOT
#
#
# VErsion 1:  https://r-graph-gallery.com/violin_horizontal_ggplot2.html
# Libraries

install.packages("hrbrthemes", dependencies = TRUE)
install.packages("viridis", dependencies = TRUE)
install.packages("Hmisc", dependencies = TRUE)
install.packages("plotly", dependencies = TRUE)



library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(hrbrthemes)
library(viridis)
library(Hmisc)

# Load dataset from github
data <- read.table("https://raw.githubusercontent.com/zonination/perceptions/master/probly.csv", header=TRUE, sep=",")

# Data is at wide format, we need to make it 'tidy' or 'long'
data <- data %>%
  gather(key="text", value="value") %>%
  mutate(text = gsub("\\.", " ",text)) %>%
  mutate(value = round(as.numeric(value),0)) %>%
  filter(text %in% c("Almost Certainly","Very Good Chance","We Believe","Likely","About Even", "Little Chance", "Chances Are Slight", "Almost No Chance"))

# Plot
p <- data %>%
  mutate(text = fct_reorder(text, value)) %>% # Reorder data
  ggplot( aes(x=text, y=value, fill=text, color=text)) +
  geom_violin(width=2.1, size=0.2) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none"
  ) +
  coord_flip() + # This switch X and Y axis and allows to get the horizontal version
  xlab("") +
  ylab("Assigned Probability (%)")

p

p + stat_summary(fun.y=mean, geom="point", shape=23, size=2,color="white")
p + stat_summary(fun.y=median, geom="point", size=2, color="red")


p + stat_summary(fun.y=mean, geom="point", shape=23, size=2,color="white") + stat_summary(fun.y=median, geom="point", size=2, color="red")

p + geom_boxplot(width=.5)


p + stat_summary(fun.data="mean_sdl", mult=2, geom="crossbar", width=0.2)
p + stat_summary(fun.data=mean_sdl, mult=2, geom="pointrange", color="red")

# 0.2 : degree of jitter in x direction (i think in the Y direction... cause i did cord-flip?)
p + geom_jitter(shape=16, position=position_jitter(0.2,0))

p + theme(legend.position="top")
#=======================
#=======================#=======================
#=======================

# New try using ChatGPT

library(ggplot2)

# Load the mtcars dataset
data("mtcars")

# Create the refined violin plot
ggplot(mtcars, aes(x=factor(cyl), y=mpg, fill=factor(cyl))) +
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, position=position_dodge(0.9)) +
  labs(title="Violin Plot of MPG by Number of Cylinders with Boxplot",
       x="Number of Cylinders",
       y="Miles per Gallon (MPG)") +
  scale_fill_brewer(palette="Pastel1") +
  theme_minimal(base_size=15) +
  theme(legend.position="none")


###
###
#Certainly! You can highlight and label a specific point on the violin plot
#by using the geom_point() and geom_text() functions from ggplot2.

#Here's an example where we'll highlight and label the point with the
#maximum mpg value for cars with 6 cylinders:

#Labeling the mean and median on the violin plot involves
#calculating these statistics for each group and then adding them to the plot
#using geom_point() and geom_text(). Here’s how you can do it:

#     Calculate the mean and median for each group.
#     Add these points and labels to the plot.


library(ggplot2)
library(dplyr)

# Load the mtcars dataset
data("mtcars")

# Calculate mean and median for each group
summary_stats <- mtcars %>%
  group_by(cyl) %>%
  summarise(mean_mpg = mean(mpg), median_mpg = median(mpg))


# Identify the specific point to highlight
highlight_point <- mtcars[mtcars$cyl == 6 & mtcars$mpg == max(mtcars$mpg[mtcars$cyl == 6]), ]


# Create the refined horizontal violin plot with mean and median labels
ggplot(mtcars, aes(x=factor(cyl), y=mpg, fill=factor(cyl))) +
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, position=position_dodge(0.9)) +
  geom_point(data=highlight_point, aes(x=factor(cyl), y=mpg), color="red", size=3) +
  geom_text(data=highlight_point, aes(x=factor(cyl), y=mpg, label=mpg), vjust=-1.5, color="red", size=5) +
  geom_point(data=summary_stats, aes(x=factor(cyl), y=mean_mpg), color="blue", size=3) +
  geom_text(data=summary_stats, aes(x=factor(cyl), y=mean_mpg, label=round(mean_mpg, 2)), vjust=-1.5, color="blue", size=5) +
  geom_point(data=summary_stats, aes(x=factor(cyl), y=median_mpg), color="green", size=3) +
  geom_text(data=summary_stats, aes(x=factor(cyl), y=median_mpg, label=round(median_mpg, 2)), vjust=1.5, color="green", size=5) +
  labs(title="Horizontal Violin Plot of MPG by Number of Cylinders with Mean and Median",
       x="Number of Cylinders",
       y="Miles per Gallon (MPG)") +
  scale_fill_brewer(palette="Pastel1") +
  theme_minimal(base_size=15) +
  theme(legend.position="none") +
  coord_flip()

####
####To achieve this interactive behavior, you can use the plotly package,
#which allows for interactive plots where text appears when you hover over points.
#Here’s how you can create the plot with the ggplot2 base and make it interactive with plotly.

library(ggplot2)
library(dplyr)
library(plotly)

# Load the mtcars dataset
data("mtcars")

# Calculate mean and median for each group
summary_stats <- mtcars %>%
  group_by(cyl) %>%
  summarise(mean_mpg = mean(mpg), median_mpg = median(mpg))

# Identify the specific point to highlight
highlight_point <- mtcars[mtcars$cyl == 6 & mtcars$mpg == max(mtcars$mpg[mtcars$cyl == 6]), ]

# Create the base ggplot
p <- ggplot(mtcars, aes(x=factor(cyl), y=mpg, fill=factor(cyl))) +
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, position=position_dodge(0.9)) +
  geom_point(data=summary_stats, aes(x=factor(cyl), y=mean_mpg), color="blue", size=3) +
  geom_point(data=summary_stats, aes(x=factor(cyl), y=median_mpg), color="green", size=3) +
  geom_point(data=highlight_point, aes(x=factor(cyl), y=mpg), color="red", size=3) +
  labs(title="Horizontal Violin Plot of MPG by Number of Cylinders with Mean, Median, and Highlighted Point",
       x="Number of Cylinders",
       y="Miles per Gallon (MPG)") +
  scale_fill_brewer(palette="Pastel1") +
  theme_minimal(base_size=15) +
  theme(legend.position="none") +
  coord_flip()

# Convert to an interactive plot using plotly
p_interactive <- ggplotly(p, tooltip=c("x", "y"))

# Customize the tooltip to show the desired labels
p_interactive <- p_interactive %>%
  layout(hoverlabel = list(bgcolor = "white", font = list(size = 12))) %>%
  style(hoverinfo = "text", text = ~paste("Mean MPG: ", round(summary_stats$mean_mpg, 2),
                                          "<br>Median MPG: ", round(summary_stats$median_mpg, 2),
                                          "<br>Highlighted MPG: ", highlight_point$mpg))

# Display the interactive plot
p_interactive

# In this code:
#
#   We use dplyr to calculate the mean and median for each group and identify the maximum mpg value for cars with 6 cylinders.
# We create the base plot using ggplot2.
# We convert the plot to an interactive plot using ggplotly from the plotly package.
# We customize the tooltips to show the mean, median, and highlighted mpg values when you hover over the points.
# Run this code in your R environment to get an interactive plot where text appears only when you hover over the points. Let me know if you need further customization or adjustments!


####
####
###


# It seems the issue is with how the tooltips are being handled.
#To correctly add hover text to specific points in a ggplot2 plot and
#then convert it to a plotly interactive plot, we need to ensure the
#hover information is included in the aes mappings.
#
# Here is the revised approach:
#
#   Include the label aesthetic in the aes mappings for the points where you want hover text.
# Use the tooltip parameter correctly in the ggplotly function to display these labels.

#To ensure that only the specified hover text for the mean, median, and maximum points is shown,
#and not the default density tooltips, you need to adjust the tooltip parameter in the
#ggplotly function more precisely.
#One approach is to modify the hoverinfo for each layer in the plotly object,
#setting it to none for layers where you don't want any tooltips.

#In this code:

#  We ensure text=NULL for the geom_boxplot to prevent default tooltips.
#After converting to a plotly object, we loop through each layer and set hoverinfo to "none"
#for the violin and boxplot layers.
#We set hoverinfo to "text" for the points with the specified hover text.
#This should ensure that only the specified hover text for the
#mean, median, and highlighted maximum points appears, without the default density tooltips.
library(ggplot2)
library(dplyr)
library(plotly)

# Load the mtcars dataset
data("mtcars")

# Filter the dataset to include only 6-cylinder cars
mtcars_6cyl <- mtcars %>% filter(cyl == 6)

# Calculate mean and median for 6-cylinder cars
summary_stats_6cyl <- mtcars_6cyl %>%
  summarise(mean_mpg = mean(mpg), median_mpg = median(mpg))

# Identify the specific point to highlight
highlight_point_6cyl <- mtcars_6cyl %>% filter(mpg == max(mpg))

# Create the base ggplot with hover text for 6-cylinder cars
p <- ggplot(mtcars_6cyl, aes(x=factor(cyl), y=mpg, fill=factor(cyl))) +
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, position=position_dodge(0.9)) +
  geom_point(data=summary_stats_6cyl, aes(x=factor(6), y=mean_mpg, text=paste("Mean MPG:", round(mean_mpg, 2))), color="blue", size=3) +
  geom_point(data=summary_stats_6cyl, aes(x=factor(6), y=median_mpg, text=paste("Median MPG:", round(median_mpg, 2))), color="green", size=3) +
  geom_point(data=highlight_point_6cyl, aes(x=factor(6), y=mpg, text=paste("Max MPG:", mpg)), color="red", size=3) +
  labs(title="Horizontal Violin Plot of MPG for 6-Cylinder Cars with Mean, Median, and Highlighted Point",
       x="Number of Cylinders",
       y="Miles per Gallon (MPG)") +
  scale_fill_brewer(palette="Pastel1") +
  theme_minimal(base_size=15) +
  theme(legend.position="none") +
  coord_flip()

# Convert to an interactive plot using plotly
p_interactive <- ggplotly(p)

# Customize tooltips to only show for specific points
for (i in 1:length(p_interactive$x$data)) {
  if (grepl("violin|box", p_interactive$x$data[[i]]$type)) {
    p_interactive$x$data[[i]]$hoverinfo <- "none"
  } else {
    p_interactive$x$data[[i]]$hoverinfo <- "text"
  }
}

# Display the interactive plot
p_interactive

#######
#It appears that the ggplot2 version you are using might not recognize the text aesthetic
#directly in the geom_point function. We can address this by
#setting the tooltip information within the ggplotly function directly using the text attribute.


library(ggplot2)
library(dplyr)
library(plotly)

# Load the mtcars dataset
data("mtcars")

# Filter the dataset to include only 6-cylinder cars
mtcars_6cyl <- mtcars %>% filter(cyl == 6)

# Calculate mean and median for 6-cylinder cars
summary_stats_6cyl <- mtcars_6cyl %>%
  summarise(mean_mpg = mean(mpg), median_mpg = median(mpg))

# Identify the specific point to highlight
highlight_point_6cyl <- mtcars_6cyl %>% filter(mpg == max(mpg))

# Create the base ggplot for 6-cylinder cars
p <- ggplot(mtcars_6cyl, aes(x=factor(cyl), y=mpg, fill=factor(cyl))) +
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, position=position_dodge(0.9)) +
  geom_point(data=summary_stats_6cyl, aes(x=factor(6), y=mean_mpg), color="blue", size=3) +
  geom_point(data=summary_stats_6cyl, aes(x=factor(6), y=median_mpg), color="green", size=3) +
  geom_point(data=highlight_point_6cyl, aes(x=factor(6), y=mpg), color="red", size=3) +
  labs(title="Horizontal Violin Plot of MPG for 6-Cylinder Cars with Mean, Median, and Highlighted Point",
       x="Number of Cylinders",
       y="Miles per Gallon (MPG)") +
  scale_fill_brewer(palette="Pastel1") +
  theme_minimal(base_size=15) +
  theme(legend.position="none") +
  coord_flip()

# Convert to an interactive plot using plotly and customize tooltips
p_interactive <- ggplotly(p) %>%
  layout(hoverlabel = list(bgcolor = "white", font = list(size = 12)))

# Add custom hover text
p_interactive$x$data[[2]]$text <- paste("Mean MPG:", round(summary_stats_6cyl$mean_mpg, 2))
p_interactive$x$data[[2]]$hoverinfo <- "text"
p_interactive$x$data[[3]]$text <- paste("Median MPG:", round(summary_stats_6cyl$median_mpg, 2))
p_interactive$x$data[[3]]$hoverinfo <- "text"
p_interactive$x$data[[4]]$text <- paste("Max MPG:", highlight_point_6cyl$mpg)
p_interactive$x$data[[4]]$hoverinfo <- "text"

# Display the interactive plot
p_interactive

######### TRYING AGAIN to fix Cyl error
library(ggplot2)
library(dplyr)
library(plotly)

# Load the mtcars dataset
data("mtcars")

# Filter the dataset to include only 6-cylinder cars
mtcars_6cyl <- mtcars %>% filter(cyl == 6)

# Calculate mean and median for 6-cylinder cars
summary_stats_6cyl <- mtcars_6cyl %>%
  summarise(mean_mpg = mean(mpg), median_mpg = median(mpg))

# Identify the specific point to highlight
highlight_point_6cyl <- mtcars_6cyl %>% filter(mpg == max(mpg))

# Create the base ggplot for 6-cylinder cars
p <- ggplot(mtcars_6cyl, aes(x=factor(cyl), y=mpg, fill=factor(cyl))) +
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, position=position_dodge(0.9)) +
  #geom_point(data=summary_stats_6cyl, aes(x=factor(6), y=mean_mpg), color="blue", size=3) +
  #geom_point(data=summary_stats_6cyl, aes(x=factor(6), y=median_mpg), color="green", size=3) +
  #geom_point(data=highlight_point_6cyl, aes(x=factor(6), y=mpg), color="red", size=3) +
  labs(title="Horizontal Violin Plot of MPG for 6-Cylinder Cars with Mean, Median, and Highlighted Point",
       x="Number of Cylinders",
       y="Miles per Gallon (MPG)") +
  scale_fill_brewer(palette="Pastel1") +
  theme_minimal(base_size=15) +
  theme(legend.position="none") +
  coord_flip()

# Convert to an interactive plot using plotly
p_interactive <- ggplotly(p)

# Customize tooltips
p_interactive$x$data[[2]]$hoverinfo <- "none" # For violin plot
p_interactive$x$data[[3]]$hoverinfo <- "none" # For boxplot
p_interactive$x$data[[4]]$text <- paste("Mean MPG:", round(summary_stats_6cyl$mean_mpg, 2))
p_interactive$x$data[[4]]$hoverinfo <- "text"
p_interactive$x$data[[5]]$text <- paste("Median MPG:", round(summary_stats_6cyl$median_mpg, 2))
p_interactive$x$data[[5]]$hoverinfo <- "text"
p_interactive$x$data[[6]]$text <- paste("Max MPG:", highlight_point_6cyl$mpg)
p_interactive$x$data[[6]]$hoverinfo <- "text"

# Display the interactive plot
p_interactive

#####

# Experiment with VIOLIN PLOT
#
#
# VErsion 1:  https://r-graph-gallery.com/violin_horizontal_ggplot2.html
# Libraries

install.packages("hrbrthemes", dependencies = TRUE)
install.packages("viridis", dependencies = TRUE)
install.packages("Hmisc", dependencies = TRUE)
install.packages("plotly", dependencies = TRUE)



library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(hrbrthemes)
library(viridis)
library(Hmisc)

# Load dataset from github
data <- read.table("https://raw.githubusercontent.com/zonination/perceptions/master/probly.csv", header=TRUE, sep=",")

# Data is at wide format, we need to make it 'tidy' or 'long'
data <- data %>%
  gather(key="text", value="value") %>%
  mutate(text = gsub("\\.", " ",text)) %>%
  mutate(value = round(as.numeric(value),0)) %>%
  filter(text %in% c("Almost Certainly","Very Good Chance","We Believe","Likely","About Even", "Little Chance", "Chances Are Slight", "Almost No Chance"))

# Plot
p <- data %>%
  mutate(text = fct_reorder(text, value)) %>% # Reorder data
  ggplot( aes(x=text, y=value, fill=text, color=text)) +
  geom_violin(width=2.1, size=0.2) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none"
  ) +
  coord_flip() + # This switch X and Y axis and allows to get the horizontal version
  xlab("") +
  ylab("Assigned Probability (%)")

p

p + stat_summary(fun.y=mean, geom="point", shape=23, size=2,color="white")
p + stat_summary(fun.y=median, geom="point", size=2, color="red")


p + stat_summary(fun.y=mean, geom="point", shape=23, size=2,color="white") + stat_summary(fun.y=median, geom="point", size=2, color="red")

p + geom_boxplot(width=.5)


p + stat_summary(fun.data="mean_sdl", mult=2, geom="crossbar", width=0.2)
p + stat_summary(fun.data=mean_sdl, mult=2, geom="pointrange", color="red")

# 0.2 : degree of jitter in x direction (i think in the Y direction... cause i did cord-flip?)
p + geom_jitter(shape=16, position=position_jitter(0.2,0))

p + theme(legend.position="top")
#=======================
#=======================#=======================
#=======================

# New try using ChatGPT

library(ggplot2)

# Load the mtcars dataset
data("mtcars")

# Create the refined violin plot
ggplot(mtcars, aes(x=factor(cyl), y=mpg, fill=factor(cyl))) +
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, position=position_dodge(0.9)) +
  labs(title="Violin Plot of MPG by Number of Cylinders with Boxplot",
       x="Number of Cylinders",
       y="Miles per Gallon (MPG)") +
  scale_fill_brewer(palette="Pastel1") +
  theme_minimal(base_size=15) +
  theme(legend.position="none")


###
###
#Certainly! You can highlight and label a specific point on the violin plot
#by using the geom_point() and geom_text() functions from ggplot2.

#Here's an example where we'll highlight and label the point with the
#maximum mpg value for cars with 6 cylinders:

#Labeling the mean and median on the violin plot involves
#calculating these statistics for each group and then adding them to the plot
#using geom_point() and geom_text(). Here’s how you can do it:

#     Calculate the mean and median for each group.
#     Add these points and labels to the plot.


library(ggplot2)
library(dplyr)

# Load the mtcars dataset
data("mtcars")

# Calculate mean and median for each group
summary_stats <- mtcars %>%
  group_by(cyl) %>%
  summarise(mean_mpg = mean(mpg), median_mpg = median(mpg))


# Identify the specific point to highlight
highlight_point <- mtcars[mtcars$cyl == 6 & mtcars$mpg == max(mtcars$mpg[mtcars$cyl == 6]), ]


# Create the refined horizontal violin plot with mean and median labels
ggplot(mtcars, aes(x=factor(cyl), y=mpg, fill=factor(cyl))) +
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, position=position_dodge(0.9)) +
  geom_point(data=highlight_point, aes(x=factor(cyl), y=mpg), color="red", size=3) +
  geom_text(data=highlight_point, aes(x=factor(cyl), y=mpg, label=mpg), vjust=-1.5, color="red", size=5) +
  geom_point(data=summary_stats, aes(x=factor(cyl), y=mean_mpg), color="blue", size=3) +
  geom_text(data=summary_stats, aes(x=factor(cyl), y=mean_mpg, label=round(mean_mpg, 2)), vjust=-1.5, color="blue", size=5) +
  geom_point(data=summary_stats, aes(x=factor(cyl), y=median_mpg), color="green", size=3) +
  geom_text(data=summary_stats, aes(x=factor(cyl), y=median_mpg, label=round(median_mpg, 2)), vjust=1.5, color="green", size=5) +
  labs(title="Horizontal Violin Plot of MPG by Number of Cylinders with Mean and Median",
       x="Number of Cylinders",
       y="Miles per Gallon (MPG)") +
  scale_fill_brewer(palette="Pastel1") +
  theme_minimal(base_size=15) +
  theme(legend.position="none") +
  coord_flip()

####
####To achieve this interactive behavior, you can use the plotly package,
#which allows for interactive plots where text appears when you hover over points.
#Here’s how you can create the plot with the ggplot2 base and make it interactive with plotly.

library(ggplot2)
library(dplyr)
library(plotly)

# Load the mtcars dataset
data("mtcars")

# Calculate mean and median for each group
summary_stats <- mtcars %>%
  group_by(cyl) %>%
  summarise(mean_mpg = mean(mpg), median_mpg = median(mpg))

# Identify the specific point to highlight
highlight_point <- mtcars[mtcars$cyl == 6 & mtcars$mpg == max(mtcars$mpg[mtcars$cyl == 6]), ]

# Create the base ggplot
p <- ggplot(mtcars, aes(x=factor(cyl), y=mpg, fill=factor(cyl))) +
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, position=position_dodge(0.9)) +
  geom_point(data=summary_stats, aes(x=factor(cyl), y=mean_mpg), color="blue", size=3) +
  geom_point(data=summary_stats, aes(x=factor(cyl), y=median_mpg), color="green", size=3) +
  geom_point(data=highlight_point, aes(x=factor(cyl), y=mpg), color="red", size=3) +
  labs(title="Horizontal Violin Plot of MPG by Number of Cylinders with Mean, Median, and Highlighted Point",
       x="Number of Cylinders",
       y="Miles per Gallon (MPG)") +
  scale_fill_brewer(palette="Pastel1") +
  theme_minimal(base_size=15) +
  theme(legend.position="none") +
  coord_flip()

# Convert to an interactive plot using plotly
p_interactive <- ggplotly(p, tooltip=c("x", "y"))

# Customize the tooltip to show the desired labels
p_interactive <- p_interactive %>%
  layout(hoverlabel = list(bgcolor = "white", font = list(size = 12))) %>%
  style(hoverinfo = "text", text = ~paste("Mean MPG: ", round(summary_stats$mean_mpg, 2),
                                          "<br>Median MPG: ", round(summary_stats$median_mpg, 2),
                                          "<br>Highlighted MPG: ", highlight_point$mpg))

# Display the interactive plot
p_interactive

# In this code:
#
#   We use dplyr to calculate the mean and median for each group and identify the maximum mpg value for cars with 6 cylinders.
# We create the base plot using ggplot2.
# We convert the plot to an interactive plot using ggplotly from the plotly package.
# We customize the tooltips to show the mean, median, and highlighted mpg values when you hover over the points.
# Run this code in your R environment to get an interactive plot where text appears only when you hover over the points. Let me know if you need further customization or adjustments!


####
####
###


# It seems the issue is with how the tooltips are being handled.
#To correctly add hover text to specific points in a ggplot2 plot and
#then convert it to a plotly interactive plot, we need to ensure the
#hover information is included in the aes mappings.
#
# Here is the revised approach:
#
#   Include the label aesthetic in the aes mappings for the points where you want hover text.
# Use the tooltip parameter correctly in the ggplotly function to display these labels.

#To ensure that only the specified hover text for the mean, median, and maximum points is shown,
#and not the default density tooltips, you need to adjust the tooltip parameter in the
#ggplotly function more precisely.
#One approach is to modify the hoverinfo for each layer in the plotly object,
#setting it to none for layers where you don't want any tooltips.

#In this code:

#  We ensure text=NULL for the geom_boxplot to prevent default tooltips.
#After converting to a plotly object, we loop through each layer and set hoverinfo to "none"
#for the violin and boxplot layers.
#We set hoverinfo to "text" for the points with the specified hover text.
#This should ensure that only the specified hover text for the
#mean, median, and highlighted maximum points appears, without the default density tooltips.
library(ggplot2)
library(dplyr)
library(plotly)

# Load the mtcars dataset
data("mtcars")

# Filter the dataset to include only 6-cylinder cars
mtcars_6cyl <- mtcars %>% filter(cyl == 6)

# Calculate mean and median for 6-cylinder cars
summary_stats_6cyl <- mtcars_6cyl %>%
  summarise(mean_mpg = mean(mpg), median_mpg = median(mpg))

# Identify the specific point to highlight
highlight_point_6cyl <- mtcars_6cyl %>% filter(mpg == max(mpg))

# Create the base ggplot with hover text for 6-cylinder cars
p <- ggplot(mtcars_6cyl, aes(x=factor(cyl), y=mpg, fill=factor(cyl))) +
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, position=position_dodge(0.9)) +
  geom_point(data=summary_stats_6cyl, aes(x=factor(6), y=mean_mpg, text=paste("Mean MPG:", round(mean_mpg, 2))), color="blue", size=3) +
  geom_point(data=summary_stats_6cyl, aes(x=factor(6), y=median_mpg, text=paste("Median MPG:", round(median_mpg, 2))), color="green", size=3) +
  geom_point(data=highlight_point_6cyl, aes(x=factor(6), y=mpg, text=paste("Max MPG:", mpg)), color="red", size=3) +
  labs(title="Horizontal Violin Plot of MPG for 6-Cylinder Cars with Mean, Median, and Highlighted Point",
       x="Number of Cylinders",
       y="Miles per Gallon (MPG)") +
  scale_fill_brewer(palette="Pastel1") +
  theme_minimal(base_size=15) +
  theme(legend.position="none") +
  coord_flip()

# Convert to an interactive plot using plotly
p_interactive <- ggplotly(p)

# Customize tooltips to only show for specific points
for (i in 1:length(p_interactive$x$data)) {
  if (grepl("violin|box", p_interactive$x$data[[i]]$type)) {
    p_interactive$x$data[[i]]$hoverinfo <- "none"
  } else {
    p_interactive$x$data[[i]]$hoverinfo <- "text"
  }
}

# Display the interactive plot
p_interactive

#######
#It appears that the ggplot2 version you are using might not recognize the text aesthetic
#directly in the geom_point function. We can address this by
#setting the tooltip information within the ggplotly function directly using the text attribute.


library(ggplot2)
library(dplyr)
library(plotly)

# Load the mtcars dataset
data("mtcars")

# Filter the dataset to include only 6-cylinder cars
mtcars_6cyl <- mtcars %>% filter(cyl == 6)

# Calculate mean and median for 6-cylinder cars
summary_stats_6cyl <- mtcars_6cyl %>%
  summarise(mean_mpg = mean(mpg), median_mpg = median(mpg))

# Identify the specific point to highlight
highlight_point_6cyl <- mtcars_6cyl %>% filter(mpg == max(mpg))

# Create the base ggplot for 6-cylinder cars
p <- ggplot(mtcars_6cyl, aes(x=factor(cyl), y=mpg, fill=factor(cyl))) +
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, position=position_dodge(0.9)) +
  geom_point(data=summary_stats_6cyl, aes(x=factor(6), y=mean_mpg), color="blue", size=3) +
  geom_point(data=summary_stats_6cyl, aes(x=factor(6), y=median_mpg), color="green", size=3) +
  geom_point(data=highlight_point_6cyl, aes(x=factor(6), y=mpg), color="red", size=3) +
  labs(title="Horizontal Violin Plot of MPG for 6-Cylinder Cars with Mean, Median, and Highlighted Point",
       x="Number of Cylinders",
       y="Miles per Gallon (MPG)") +
  scale_fill_brewer(palette="Pastel1") +
  theme_minimal(base_size=15) +
  theme(legend.position="none") +
  coord_flip()

# Convert to an interactive plot using plotly and customize tooltips
p_interactive <- ggplotly(p) %>%
  layout(hoverlabel = list(bgcolor = "white", font = list(size = 12)))

# Add custom hover text
p_interactive$x$data[[2]]$text <- paste("Mean MPG:", round(summary_stats_6cyl$mean_mpg, 2))
p_interactive$x$data[[2]]$hoverinfo <- "text"
p_interactive$x$data[[3]]$text <- paste("Median MPG:", round(summary_stats_6cyl$median_mpg, 2))
p_interactive$x$data[[3]]$hoverinfo <- "text"
p_interactive$x$data[[4]]$text <- paste("Max MPG:", highlight_point_6cyl$mpg)
p_interactive$x$data[[4]]$hoverinfo <- "text"

# Display the interactive plot
p_interactive

######### TRYING AGAIN to fix Cyl error
library(ggplot2)
library(dplyr)
library(plotly)

# Load the mtcars dataset
data("mtcars")

# Filter the dataset to include only 6-cylinder cars
mtcars_6cyl <- mtcars %>% filter(cyl == 6)

# Calculate mean and median for 6-cylinder cars
summary_stats_6cyl <- mtcars_6cyl %>%
  summarise(mean_mpg = mean(mpg), median_mpg = median(mpg))

# Identify the specific point to highlight
highlight_point_6cyl <- mtcars_6cyl %>% filter(mpg == max(mpg))

# Create the base ggplot for 6-cylinder cars
p <- ggplot(mtcars_6cyl, aes(x=factor(cyl), y=mpg, fill=factor(cyl))) +
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, position=position_dodge(0.9)) +
  #geom_point(data=summary_stats_6cyl, aes(x=factor(6), y=mean_mpg), color="blue", size=3) +
  #geom_point(data=summary_stats_6cyl, aes(x=factor(6), y=median_mpg), color="green", size=3) +
  #geom_point(data=highlight_point_6cyl, aes(x=factor(6), y=mpg), color="red", size=3) +
  labs(title="Horizontal Violin Plot of MPG for 6-Cylinder Cars with Mean, Median, and Highlighted Point",
       x="Number of Cylinders",
       y="Miles per Gallon (MPG)") +
  scale_fill_brewer(palette="Pastel1") +
  theme_minimal(base_size=15) +
  theme(legend.position="none") +
  coord_flip()

# Convert to an interactive plot using plotly
p_interactive <- ggplotly(p)

# Customize tooltips
p_interactive$x$data[[2]]$hoverinfo <- "none" # For violin plot
p_interactive$x$data[[3]]$hoverinfo <- "none" # For boxplot
p_interactive$x$data[[4]]$text <- paste("Mean MPG:", round(summary_stats_6cyl$mean_mpg, 2))
p_interactive$x$data[[4]]$hoverinfo <- "text"
p_interactive$x$data[[5]]$text <- paste("Median MPG:", round(summary_stats_6cyl$median_mpg, 2))
p_interactive$x$data[[5]]$hoverinfo <- "text"
p_interactive$x$data[[6]]$text <- paste("Max MPG:", highlight_point_6cyl$mpg)
p_interactive$x$data[[6]]$hoverinfo <- "text"

# Display the interactive plot
p_interactive


#####################################
# THISIS THE BEST ONE TO START WITH... it is from BEFORE....  ANDIT MOSTLY WORKS
#  EXCEPT FOR ONE WEIRD WARNING message
######## here is link exaplining weird error maybe: https://stackoverflow.com/questions/42974231/ggplot-geom-point-ignoring-unknown-aesthetics-text-issue

library(ggplot2)
library(dplyr)
library(plotly)

# Load the mtcars dataset
data("mtcars")

mtcars <- mtcars %>% filter(cyl==6)
# Calculate mean and median for each group
summary_stats <- mtcars %>%
  group_by(cyl) %>%
  summarise(mean_mpg = mean(mpg), median_mpg = median(mpg))

# Identify the specific point to highlight
highlight_point <- mtcars[mtcars$cyl == 6 & mtcars$mpg == max(mtcars$mpg[mtcars$cyl == 6]), ]

# Create the base ggplot with hover text
p <- ggplot(mtcars , aes(x=factor(cyl), y=mpg, fill=factor(cyl))) +
  geom_violin(trim=FALSE) +
  #geom_boxplot(width=0.1, position=position_dodge(0.9)) +    # DELETE BOX PLOT as just confusing
  geom_point(data=summary_stats, aes(x=factor(cyl), y=mean_mpg, text=paste("Mean MPG:", round(mean_mpg, 2))), color="blue", size=3) +
  geom_point(data=summary_stats, aes(x=factor(cyl), y=median_mpg, text=paste("Median MPG:", round(median_mpg, 2))), color="green", size=3) +
  geom_point(data=highlight_point, aes(x=factor(cyl), y=mpg, text=paste("Max MPG for 6 cyl:", mpg)), color="red", size=3) +
  labs(title="Horizontal Violin Plot of MPG by Number of Cylinders with Mean, Median, and Highlighted Point",
       x="Number of Cylinders",
       y="Miles per Gallon (MPG)") +
  scale_fill_brewer(palette="Pastel1") +
  theme_minimal(base_size=15) +
  theme(legend.position="none") +
  coord_flip()

# Convert to an interactive plot using plotly
p_interactive <- ggplotly(p, tooltip="text")

# Display the interactive plot
p_interactive
#

#####################################
######################################
###########thisis marc playing with the one directly above as the best one to start with...
#

#####################################
# THISIS THE BEST ONE TO START WITH... it is from BEFORE....  ANDIT MOSTLY WORKS
#  EXCEPT FOR ONE WEIRD WARNING message
######## here is link exaplining weird error maybe: https://stackoverflow.com/questions/42974231/ggplot-geom-point-ignoring-unknown-aesthetics-text-issue

library(ggplot2)
library(dplyr)
library(plotly)

# Load the mtcars dataset
data("mtcars")

mtcars <- mtcars %>% filter(cyl==6)
# Calculate mean and median for each group
summary_stats <- mtcars %>%
  group_by(cyl) %>%
  summarise(mean_mpg = mean(mpg), median_mpg = median(mpg))

# Identify the specific point to highlight
highlight_point <- mtcars[mtcars$cyl == 6 & mtcars$mpg == max(mtcars$mpg[mtcars$cyl == 6]), ]

# Create the base ggplot with hover text
p <- ggplot(mtcars , aes(x=factor(cyl), y=mpg, fill=factor(cyl))) +
  geom_violin(trim=FALSE) +
  #geom_boxplot(width=0.1, position=position_dodge(0.9)) +    # DELETE BOX PLOT as just confusing
  geom_point(data=summary_stats, aes(x=factor(cyl), y=mean_mpg, text=paste("Mean MPG:", round(mean_mpg, 2))), color="blue", size=3) +
  geom_point(data=summary_stats, aes(x=factor(cyl), y=median_mpg, text=paste("Median MPG:", round(median_mpg, 2))), color="green", size=3) +
  geom_point(data=highlight_point, aes(x=factor(cyl), y=mpg, text=paste("Max MPG for 6 cyl:", mpg)), color="red", size=3) +
  labs(title="Horizontal Violin Plot of MPG by Number of Cylinders with Mean, Median, and Highlighted Point",
       x="Number of Cylinders",
       y="Miles per Gallon (MPG)") +
  scale_fill_brewer(palette="Pastel1") +
  theme_minimal(base_size=15) +
  theme(legend.position="none") +
  coord_flip()

# Convert to an interactive plot using plotly
p_interactive <- ggplotly(p, tooltip="text")

# Display the interactive plot
p_interactive
#

###############trying again:  SIMPLIFY
#####################################
# THISIS THE BEST ONE TO START WITH... it is from BEFORE....  ANDIT MOSTLY WORKS
#  EXCEPT FOR ONE WEIRD WARNING message
######## here is link exaplining weird error maybe: https://stackoverflow.com/questions/42974231/ggplot-geom-point-ignoring-unknown-aesthetics-text-issue

library(ggplot2)
library(dplyr)
library(plotly)

# Load the mtcars dataset
data("mtcars")

#mtcars <- mtcars %>% filter(cyl==6)
# Calculate mean and median for each group
summary_stats <- mtcars %>%
  summarise(mean_mpg = mean(mpg), median_mpg = median(mpg))

# Identify the specific point to highlight
highlight_point <- mtcars[mtcars$cyl == 6 & mtcars$mpg == max(mtcars$mpg[mtcars$cyl == 6]), ]
highlight_point2 <-mtcars[mtcars$cyl == 6 & mtcars$mpg == min(mtcars$mpg[mtcars$cyl == 6]), ]

# Create the base ggplot with hover text
p <- ggplot(mtcars , aes(x=factor(cyl), y=mpg, fill=factor(cyl))) +
  geom_violin(trim=FALSE) +
  #geom_boxplot(width=0.1, position=position_dodge(0.9)) +    # DELETE BOX PLOT as just confusing
  geom_point(data=summary_stats, aes(x=factor(cyl), y=mean_mpg) , color="blue", size=3) #+
  geom_text(data=summary_stats, aes(label=paste("Mean MPG:", round(mean_mpg, 2))))  +


  #geom_point(data=summary_stats,  aes(x=factor(cyl), y=median_mpg) color="green", size=3))   +
  #geom_text(data=summary_stats,aes(label=paste("Median MPG:", round(median_mpg, 2)))) +


  geom_point(data=highlight_point, aes(x=factor(cyl), y=mpg ), color="red", size=3) +
  geom_text(data=highlight_point,aes(label=paste("Max MPG for 6 cyl:", mpg)))  +



  geom_point(data=highlight_point2, aes(x=factor(cyl), y=mpg ), color="red", size=3) +
  geom_text(data=highlight_point2,aes(label=paste("MIN MPG for 6 cyl:", mpg)))


# +
# # labs(title="Horizontal Violin Plot of MPG by Number of Cylinders with Mean, Median, and Highlighted Point",
#        x="Number of Cylinders",
#        y="Miles per Gallon (MPG)") +
#   scale_fill_brewer(palette="Pastel1") +
#   theme_minimal(base_size=15) +
#   theme(legend.position="none") +
#   coord_flip()

# Convert to an interactive plot using plotly
p_interactive <- ggplotly(p, tooltip="text")

# Display the interactive plot
p_interactive
#


#### TRYING AGAIN... this time w/ ANNOTATE

#####################################
# THISIS THE BEST ONE TO START WITH... it is from BEFORE....  ANDIT MOSTLY WORKS
#  EXCEPT FOR ONE WEIRD WARNING message which is OK and I'm now using suppressmessages to hide
######## here is link exaplining weird error maybe: https://stackoverflow.com/questions/42974231/ggplot-geom-point-ignoring-unknown-aesthetics-text-issue

library(ggplot2)
library(dplyr)
library(plotly)

# Load the mtcars dataset
data("mtcars")

mtcars <- mtcars %>% filter(cyl==6)
# Calculate mean and median for each group
summary_stats <- mtcars %>%
  group_by(cyl) %>%
  summarise(mean_mpg = mean(mpg), median_mpg = median(mpg))

# Identify the specific point to highlight
highlight_point <- mtcars[mtcars$cyl == 6 & mtcars$mpg == max(mtcars$mpg[mtcars$cyl == 6]), ]

# Create the base ggplot with hover text
p <- suppressWarnings(   ### NOTE:  I'm surpressing   "Ignoring unknown aesthetics: text" which for some reason shows up
  ggplot(mtcars , aes(
    x = factor(cyl),
    y = mpg,
    fill = factor(cyl)
  )) +
    geom_violin(trim = FALSE) +
    #geom_boxplot(width=0.1, position=position_dodge(0.9)) +    # DELETE BOX PLOT as just confusing
    #annotate(geom=text,x=factor(cyl), y=6, label=paste("Mean MPG:", round(mean_mpg, 2)), color="blue", size=3) +
    geom_point(
      data = summary_stats,
      aes(
        x = factor(cyl),
        y = mean_mpg,
        text = paste("Mean MPG:", round(mean_mpg, 2))
      ),
      color = "blue",
      size = 3
    ) +
    geom_point(
      data = summary_stats,
      aes(
        x = factor(cyl),
        y = median_mpg,
        text = paste("Median MPG:", round(median_mpg, 2))
      ),
      color = "green",
      size = 3
    ) +
    geom_point(
      data = highlight_point,
      aes(
        x = factor(cyl),
        y = mpg,
        text = paste("Max MPG for 6 cyl:", mpg)
      ),
      color = "red",
      size = 3
    ) +

    #geom_text(data=highlight_point,aes(label=paste("-----------------------------------", mpg)))  +


    labs(title = "Horizontal Violin Plot of MPG by Number of Cylinders with Mean, Median, and Highlighted Point", x =
           "Number of Cylinders", y = "Miles per Gallon (MPG)") +
    scale_fill_brewer(palette = "Pastel1") +
    theme_minimal(base_size = 15) +
    theme(legend.position = "none") +
    coord_flip()
)

# Convert to an interactive plot using plotly
p_interactive <- ggplotly(p, tooltip="text")

#p_interactive <- ggplotly(p, hoverinfo ="text")

# Display the interactive plot
p_interactive


# USING THIS LINK:  https://plotly-r.com/improving-ggplotly.html
#    i can see that by inspecting P and finding out where the DATA is i want to control i can turn on/off tooltips
#         BUT at moment it replaces my created tip with its own idea...
plotly_json(p)
p2 <- style(p,hoverinfo="none",traces=1)
p2_interactive <- ggplotly(p2, tooltip="text")
p2_interactive



#

##### NEW WEBSITE:https://plotly-r.com/improving-ggplotly.html

p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() + geom_smooth()
ggplotly(p)
p2 <- style(p,hoverinfo="points")
ggplotly(p2)


### TRYING AGAIN COMBINING 2 immediatly above....





#####################################
# THISIS  a modification of BEST ONE TO START WITH..

###

###  THIS IS INOW THE BEST BEST BEST BESTone since it hides the tooltip on LINES and customizes the langauge on pop-up properly
###  THIS IS INOW THE BEST BEST BEST BESTone since it hides the tooltip on LINES and customizes the langauge on pop-up properly
###  THIS IS INOW THE BEST BEST BEST BESTone since it hides the tooltip on LINES and customizes the langauge on pop-up properly

# OTHER LINKS TO LOOK AT include:  https://gist.github.com/matt-dray/8e5d03e32318da90ee53cd35d81871c0


# USING THIS LINK:  https://plotly-r.com/improving-ggplotly.html
#    i can see that by inspecting P and finding out where the DATA is i want to control i can turn on/off tooltips
#         BUT at moment it replaces my created tip with its own idea...
#             BUT that i now fixed by just redefining the text


###
#  EXCEPT FOR ONE WEIRD WARNING message
######## here is link exaplining weird error maybe: https://stackoverflow.com/questions/42974231/ggplot-geom-point-ignoring-unknown-aesthetics-text-issue
##### ERROR FIXED
#####  NOW THIS IS THE BEST FOR SURE
library(ggplot2)
library(dplyr)
library(plotly)

# Load the mtcars dataset
data("mtcars")

mtcars <- mtcars %>% filter(cyl==6)
# Calculate mean and median for each group
summary_stats <- mtcars %>%
  group_by(cyl) %>%
  summarise(mean_mpg = mean(mpg), median_mpg = median(mpg))

# Identify the specific point to highlight
highlight_point <- mtcars[mtcars$cyl == 6 & mtcars$mpg == max(mtcars$mpg[mtcars$cyl == 6]), ]

# Create the base ggplot with hover text which took lots of playing around to get it to work
### NOTE:  I'm was surpressing   "Ignoring unknown aesthetics: text" which for some reason shows up but i fixed maybe
p <- ggplot(mtcars , aes(
    x = factor(cyl),
    y = mpg,
    fill = factor(cyl)
  )) +
    geom_violin(trim = FALSE) +
    #geom_boxplot(width=0.1, position=position_dodge(0.9)) +    # DELETE BOX PLOT as just confusing
    geom_point(
      data = summary_stats,
      aes(
        x = factor(cyl),
        y = mean_mpg
      ),
      color = "blue",
      size = 3
    ) +
    geom_point(
      data = summary_stats,
      aes(
        x = factor(cyl),
        y = median_mpg
      ),
      color = "green",
      size = 3
    ) +
    geom_point(
      data = highlight_point,
      aes(
        x = factor(cyl),
        y = mpg
      ),
      color = "red",
      size = 3
    ) +
    labs(title = "Horizontal Violin Plot of MPG by Number of Cylinders with Mean, Median, and Highlighted Point", x =
           "Number of Cylinders", y = "Miles per Gallon (MPG)") +
    scale_fill_brewer(palette = "Pastel1") +
    theme_minimal(base_size = 15) +
    theme(legend.position = "none") +
    coord_flip()



# Display the interactive plot but try to style just ONE POINT
text_MEDIAN_label<-  paste("Median MPG:", round(summary_stats$median_mpg, 2))
text_MEAN_label<-  paste("Mean MPG:", round(summary_stats$mean_mpg, 2))
text_YOURDATA_label<-  paste("Your actualactualactual MPG:", round(highlight_point$mpg, 2))

# USING THIS LINK:  https://plotly-r.com/improving-ggplotly.html
#    i can see that by inspecting P and finding out where the DATA is i want to control i can turn on/off tooltips
#         BUT at moment it replaces my created tip with its own idea...
# plotly_json(p)   # this is the command to run to figure out what "traces" to set to which info


p2_interactive <- ggplotly(p)  %>%
  style(hoverinfo = "none", traces = 1) %>%
  style(text = text_MEDIAN_label, traces = 2)  %>%
  style(text = text_MEAN_label, traces = 3) %>%
  style(text = text_YOURDATA_label, traces = 4)

p2_interactive

# USING THIS LINK:  https://plotly-r.com/improving-ggplotly.html
#    i can see that by inspecting P and finding out where the DATA is i want to control i can turn on/off tooltips
#         BUT at moment it replaces my created tip with its own idea...
#plotly_json(p)

#==============================================================
### another chatgpt attempt that I ignored or didn't work right either...
#==============================================================
### another chatgpt attempt that I ignored or didn't work right either...#==============================================================
### another chatgpt attempt that I ignored or didn't work right either...#==============================================================
### another chatgpt attempt that I ignored or didn't work right either...#==============================================================
### another chatgpt attempt that I ignored or didn't work right either...#==============================================================
### another chatgpt attempt that I ignored or didn't work right either...#==============================================================
### another chatgpt attempt that I ignored or didn't work right either...#==============================================================
### another chatgpt attempt that I ignored or didn't work right either...#==============================================================
### another chatgpt attempt that I ignored or didn't work right either...#==============================================================
### another chatgpt attempt that I ignored or didn't work right either...


library(ggplot2)
library(dplyr)
library(plotly)

# Load the mtcars dataset
data("mtcars")

# Filter the dataset to include only 6-cylinder cars
#mtcars_6cyl <- mtcars %>% filter(cyl == 6)
mtcars_6cyl <- mtcars


# Calculate mean and median for 6-cylinder cars
summary_stats_6cyl <- mtcars_6cyl %>%
  summarise(mean_mpg = mean(mpg), median_mpg = median(mpg))

# Identify the specific point to highlight
highlight_point_6cyl <- mtcars_6cyl %>% filter(mpg == max(mpg))

## Create the base ggplot for 6-cylinder cars
#p <- ggplot(mtcars_6cyl, aes(x=factor(cyl), y=mpg, fill=factor(cyl))) +   #####
p <-  ggplot(mtcars_6cyl, aes(
  x = factor(cyl),
  y = mpg #,
  #fill = factor(cyl)   ##### FOR SOME UNKNOWN REASON this FACTOR doesn't work although the x-factor(cyl) does work?
)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, position = position_dodge(0.9)) +
  geom_point(
    data = summary_stats_6cyl,
    aes(x = factor(6), y = mean_mpg),
    color = "blue",
    size = 3
  ) +
  geom_point(
    data = summary_stats_6cyl,
    aes(x = factor(6), y = median_mpg),
    color = "green",
    size = 3
  ) +
  geom_point(
    data = highlight_point_6cyl,
    aes(x = factor(6), y = mpg),
    color = "red",
    size = 3
  ) +
  labs(title = "Horizontal Violin Plot of MPG for 6-Cylinder Cars with Mean, Median, and Highlighted Point", x =
         "Number of Cylinders", y = "Miles per Gallon (MPG)") +
  scale_fill_brewer(palette = "Pastel1") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  coord_flip()

plotly_json(p)
# Convert to an interactive plot using plotly
p_interactive <- ggplotly(p)

# Customize tooltips    ============ THIS SHOWS ANOTHER WAY WITHOUT THE STYLE COMMAND
p_interactive$x$data[[1]]$hoverinfo <- "none" # For violin plot
p_interactive$x$data[[2]]$hoverinfo <- "none" # For boxplot
p_interactive$x$data[[3]]$text <- paste("Mean MPG:", round(summary_stats_6cyl$mean_mpg, 2))
p_interactive$x$data[[3]]$hoverinfo <- "text"
p_interactive$x$data[[4]]$text <- paste("Median MPG:", round(summary_stats_6cyl$median_mpg, 2))
p_interactive$x$data[[4]]$hoverinfo <- "text"
p_interactive$x$data[[5]]$text <- paste("Max MPG:", highlight_point_6cyl$mpg)
p_interactive$x$data[[5]]$hoverinfo <- "text"

# Display the interactive plot
p_interactive

#####################
###SIMPLIFY
### another chatgpt attempt that I ignored or didn't work right either...#==============================================================
### another chatgpt attempt that I ignored or didn't work right either...#==============================================================
### another chatgpt attempt that I ignored or didn't work right either...#==============================================================
### another chatgpt attempt that I ignored or didn't work right either...


library(ggplot2)
library(dplyr)
library(plotly)

# Load the mtcars dataset
data("mtcars")


# Calculate mean and median for 6-cylinder cars
summary_stats <- mtcars %>%
  summarise(mean_mpg = mean(mpg), median_mpg = median(mpg))



# Identify the specific point to highlight
highlight_point <- mtcars %>% filter(mpg == max(mpg))

## Create the base ggplot for 6-cylinder cars
p <-  ggplot(mtcars, aes(
  x = factor(cyl),
  y = mpg #,
  #fill = factor(cyl)   ##### FOR SOME UNKNOWN REASON this FACTOR doesn't work although the x-factor(cyl) does work?
)) +
  geom_violin(trim = FALSE,color="yellow",fill="pink") +
  geom_boxplot(width = 0.1, position = position_dodge(0.9),fill="red") +
  geom_point(
    data = summary_stats,
    aes(x =factor( 6), y = mean_mpg),
    color = "blue",
    size = 3
  ) +
  geom_point(
    data = summary_stats,
    aes(x = factor( 6), y = median_mpg),
    color = "green",
    size = 3
  ) +

    geom_point(
    data = highlight_point,
    aes(x = factor( 6), y = mpg),
    color = "red",
    size = 3
  ) +
  labs(title = "Horizontal Violin Plot of MPG for 6-Cylinder Cars with Mean, Median, and Highlighted Point", x =
         "Number of Cylinders", y = "Miles per Gallon (MPG)") +
  scale_fill_brewer(palette = "Pastel1") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none")   +
  coord_flip()

# Convert to an interactive plot using plotly
p_interactive <- ggplotly(p)

# Display the interactive plot
p_interactive



#============================
#============================
#============================
#============================
#============================
#   AND YET ANOTHER TRY this time showing points on all violins
#============================
#============================
#============================
#============================
#============================
#============================


library(ggplot2)
library(dplyr)
library(plotly)

# Load the mtcars dataset
data("mtcars")

# Calculate mean, median, and max for each cylinder group
summary_stats <- mtcars %>%
  group_by(cyl) %>%
  summarise(
    mean_mpg = mean(mpg),
    median_mpg = median(mpg),
    max_mpg = max(mpg)
  )

# Identify the specific points to highlight
highlight_points <- mtcars %>%
  group_by(cyl) %>%
  filter(mpg == max(mpg))

# Create the base ggplot with hover text for all cylinder groups
p <- ggplot(mtcars, aes(x=factor(cyl), y=mpg, fill=factor(cyl))) +
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, position=position_dodge(0.9)) +
  geom_point(data=summary_stats, aes(x=factor(cyl), y=mean_mpg, text=paste("Mean MPG:", round(mean_mpg, 2))), color="blue", size=3) +
  geom_point(data=summary_stats, aes(x=factor(cyl), y=median_mpg, text=paste("Median MPG:", round(median_mpg, 2))), color="green", size=3) +
  geom_point(data=highlight_points, aes(x=factor(cyl), y=mpg, text=paste("Max MPG:", mpg)), color="red", size=3) +
  labs(title="Horizontal Violin Plot of MPG by Number of Cylinders with Mean, Median, and Highlighted Points",
       x="Number of Cylinders",
       y="Miles per Gallon (MPG)") +
  scale_fill_brewer(palette="Pastel1") +
  theme_minimal(base_size=15) +
  theme(legend.position="none") +
  coord_flip()

#plotly_json(p)

# Convert to an interactive plot using plotly
p_interactive <- ggplotly(p)

# Customize tooltips to only show for specific points   ### NOTE THIS DOESN"T EXACTLY WORK RIGHT AT ALL... diff techniquest to show/hide info work better see above
for (i in 1:length(p_interactive$x$data)) {
  if (grepl("violin|box|line", p_interactive$x$data[[i]]$type)) {
    p_interactive$x$data[[i]]$hoverinfo <- "none"
  } else {
    p_interactive$x$data[[i]]$hoverinfo <- "text"
  }
}

# Display the interactive plot
p_interactive

#=
#####  NOW THIS IS THE BEST FOR SURE
#####  NOW THIS IS THE BEST FOR SURE
#=
#####  NOW THIS IS THE BEST FOR SURE
#####  NOW THIS IS THE BEST FOR SURE
#=
#####  NOW THIS IS THE BEST FOR SURE
#####  NOW THIS IS THE BEST FOR SURE
#=
#####  NOW THIS IS THE BEST FOR SURE
#####  NOW THIS IS THE BEST FOR SURE
#=
#####  NOW THIS IS THE BEST FOR SURE
#####  NOW THIS IS THE BEST FOR SURE
library(ggplot2)
library(dplyr)
library(plotly)

# Load the mtcars dataset
data("mtcars")

mtcars <- mtcars %>% filter(cyl==6) %>%
  select(mpg,cyl)


library(tibble)
mtcars <- tibble::rownames_to_column(mtcars, "CarType")


# Calculate mean and median for each group
summary_stats <- mtcars %>%
  group_by(cyl) %>%
  summarise(mean_mpg = mean(mpg), median_mpg = median(mpg))

# Identify the specific point to highlight
highlight_point <- mtcars[mtcars$cyl == 6 & mtcars$mpg == max(mtcars$mpg[mtcars$cyl == 6]), ]

# Create the base ggplot with hover text which took lots of playing around to get it to work
### NOTE:  I'm was surpressing   "Ignoring unknown aesthetics: text" which for some reason shows up but i fixed maybe
p <- ggplot(mtcars , aes(
  x = factor(cyl),
  y = mpg,
  fill = factor(cyl)
)) +
  geom_violin(trim = FALSE) +
  #geom_boxplot(width=0.1, position=position_dodge(0.9)) +    # DELETE BOX PLOT as just confusing
  geom_point(
    data = summary_stats,
    aes(
      x = factor(cyl),
      y = mean_mpg
    ),
    color = "blue",
    size = 3
  ) +
  geom_point(
    data = summary_stats,
    aes(
      x = factor(cyl),
      y = median_mpg
    ),
    color = "green",
    size = 3
  ) +
  geom_point(
    data = highlight_point,
    aes(
      x = factor(cyl),
      y = mpg
    ),
    color = "red",
    size = 3
  ) +
  labs(title = "Horizontal Violin Plot of MPG by Number of Cylinders with Mean, Median, and Highlighted Point", x =
         "Number of Cylinders", y = "Miles per Gallon (MPG)") +
  scale_fill_brewer(palette = "Pastel1") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  coord_flip()



# Display the interactive plot but try to style just ONE POINT
text_MEDIAN_label<-  paste("Median MPG:", round(summary_stats$median_mpg, 2))
text_MEAN_label<-  paste("Mean MPG:", round(summary_stats$mean_mpg, 2))
text_YOURDATA_label<-  paste("Your actualactualactual MPG:", round(highlight_point$mpg, 2))

# USING THIS LINK:  https://plotly-r.com/improving-ggplotly.html
#    i can see that by inspecting P and finding out where the DATA is i want to control i can turn on/off tooltips
#         BUT at moment it replaces my created tip with its own idea...
plotly_json(p)   # this is the command to run to figure out what "traces" to set to which info
plotly_json(chemPlot)   # this is the command to run to figure out what "traces" to set to which info


p2_interactive <- ggplotly(p)  %>%
  style(hoverinfo = "none", traces = 1) %>%
  style(text = text_MEDIAN_label, traces = 2)  %>%
  style(text = text_MEAN_label, traces = 3) %>%
  style(text = text_YOURDATA_label, traces = 4)

p2_interactive



