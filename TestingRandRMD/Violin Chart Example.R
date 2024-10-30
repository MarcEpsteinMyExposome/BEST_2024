# Experiment with VIOLIN PLOT
#
#

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

### BEGIN TEST MODIFY mtcars to see if i can use value from testResults_ChemOfConcern

#  df[(nrow(df)+1):newsize,] <- NA
mtcars[(nrow(mtcars)+1):38,] <- NA
mtcars$CarType<-testResults_ChemOfConcern$SampleNumber #%>% head(7)
mtcars$mpg <-testResults_ChemOfConcern$Result #%>% head(7)
mtcars$cyl <-testResults_ChemOfConcern$ParameterName #%>% head(7)
highlight_point <- mtcars[ mtcars$mpg == max(mtcars$mpg), ]

## DIFF TEST
mtcars <- testResults_ChemOfConcern %>%
  rename(CarType=SampleNumber, mpg=Result, cyl=ParameterName)

####
### END TEST MODIFY mtcars to see if i can use value from testResults_ChemOfConcern



# Calculate mean and median for each group
summary_stats <- mtcars %>%
  group_by(cyl) %>%
  summarise(mean_mpg = mean(mpg), median_mpg = median(mpg))

# Identify the specific point to highlight
#highlight_point <- mtcars[mtcars$cyl == 6 & mtcars$mpg == max(mtcars$mpg[mtcars$cyl == 6]), ]


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



#==================================
#==================================
#==================================
#==================================
#==================================

# make working chem of concern
  # Experiment with VIOLIN PLOT
  #
  #

library(ggplot2)
library(dplyr)
library(plotly)

# Load the mtcars dataset
#data("mtcars")

#mtcars <- mtcars %>% filter(cyl==6) %>%
#  select(mpg,cyl)


#library(tibble)
#mtcars <- tibble::rownames_to_column(mtcars, "CarType")

### BEGIN TEST MODIFY mtcars to see if i can use value from testResults_ChemOfConcern

#  df[(nrow(df)+1):newsize,] <- NA
#mtcars[(nrow(mtcars)+1):38,] <- NA
#mtcars$CarType<-testResults_ChemOfConcern$SampleNumber #%>% head(7)
#mtcars$mpg <-testResults_ChemOfConcern$Result #%>% head(7)
#mtcars$cyl <-testResults_ChemOfConcern$ParameterName #%>% head(7)
#highlight_point <- mtcars[ mtcars$mpg == max(mtcars$mpg), ]

## DIFF TEST
#mtcars <- testResults_ChemOfConcern %>%
#  rename(CarType=SampleNumber, mpg=Result, cyl=ParameterName)


highlight_point <- testResults_ChemOfConcern[ testResults_ChemOfConcern$Result == max(testResults_ChemOfConcern$Result), ]

####
### END TEST MODIFY mtcars to see if i can use value from testResults_ChemOfConcern



# Calculate mean and median for each group
summary_stats <- testResults_ChemOfConcern %>%
  group_by(ParameterName) %>%
  summarise(mean_mpg = mean(Result), median_mpg = median(Result))

# Identify the specific point to highlight
#highlight_point <- mtcars[mtcars$cyl == 6 & mtcars$mpg == max(mtcars$mpg[mtcars$cyl == 6]), ]


# Create the base ggplot with hover text which took lots of playing around to get it to work
### NOTE:  I'm was surpressing   "Ignoring unknown aesthetics: text" which for some reason shows up but i fixed maybe
chemPlot <- ggplot(testResults_ChemOfConcern , aes(
  x = factor(ParameterName),
  y = Result,
  fill = factor(ParameterName)
)) +
  geom_violin(trim = FALSE) +
  #geom_boxplot(width=0.1, position=position_dodge(0.9)) +    # DELETE BOX PLOT as just confusing
  geom_point(
    data = summary_stats,
    aes(
      x = factor(ParameterName),
      y = mean_mpg
    ),
    color = "blue",
    size = 3
  ) +
  geom_point(
    data = summary_stats,
    aes(
      x = factor(ParameterName),
      y = median_mpg
    ),
    color = "green",
    size = 3
  ) +
  geom_point(
    data = highlight_point,
    aes(
      x = factor(ParameterName),
      y = Result
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
text_YOURDATA_label<-  paste("Your actualactualactual MPG:", round(highlight_point$Result, 2))

# USING THIS LINK:  https://plotly-r.com/improving-ggplotly.html
#    i can see that by inspecting P and finding out where the DATA is i want to control i can turn on/off tooltips
#         BUT at moment it replaces my created tip with its own idea...
#plotly_json(p)   # this is the command to run to figure out what "traces" to set to which info
plotly_json(chemPlot)   # this is the command to run to figure out what "traces" to set to which info


chemPlot_interactive <- ggplotly(chemPlot)  %>%
  style(hoverinfo = "none", traces = 1) %>%
  style(text = text_MEDIAN_label, traces = 2)  %>%
  style(text = text_MEAN_label, traces = 3) %>%
  style(text = text_YOURDATA_label, traces = 4)

chemPlot_interactive


###################################################################














