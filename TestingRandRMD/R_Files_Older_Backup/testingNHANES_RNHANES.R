
# https://wwww.silentspring.org/devblog/2016/11/22/nhanes-made-simple-with-rnhanes.html

# testing
# Install stable version
install.packages("RNHANES")
library(tidyverse)
library(RNHANES)



library(RNHANES)
library(tidyverse)
library(jtools)

# Download PAH dataset
nhanes_dat <- nhanes_load_data("PAH_H", "2013-2014", demographics = TRUE)

# Build the survey design object
des <- nhanes_survey_design(nhanes_dat)

svycor(~log(URXP01) + log(URXP04) + log(URXP06) + log(URXP10), design = des, na.rm = TRUE)




# #################################














# Downloading PFAS data
# There is a comprehensive index online of the data available through NHANES. The index links to each file and the associated data dictionary. You can use the index to explore the data that is out there.
#
# In our case, we are interested in PFASs. The latest available data on PFASs in NHANES is from the 2011-2012 survey cycle. You can download a data file easily through RNHANES:

pfas_data <- nhanes_load_data("PFC_G", "2011-2012", demographics = TRUE)

# Now that we have the data, we can compute some summary statistics. A good first question to ask is how many people have a detectable level of PFOA in their blood samples, which we can find using the nhanes_detection_frequency function:

nhanes_detection_frequency(pfas_data, "LBXPFOA", "LBDPFOAL")

# value     cycle begin_year end_year file_name  column weights_column comment_column                name
# 1 0.9973545 2011-2012       2011     2012     PFC_G LBXPFOA        WTSA2YR       LBDPFOAL detection_frequency

quantiles <- nhanes_quantile(pfas_data, "LBXPFOA", "LBDPFOAL", quantiles = c(0.5, 0.95))
# ABOVE generates errors

nhanes_hist(pfas_data, "LBXPFOA", "LBDPFOAL")

nhanes_hist(pfas_data, "LBXPFOA", "LBDPFOAL", transform="log")



library(RNHANES)

# Download environmental phenols & parabens data from the 2011-2012 survey cycle
dat <- nhanes_load_data("EPH", "2011-2012")

# Download the same data, but this time include demographics data (which includes sample weights)
dat <- nhanes_load_data("EPH", "2011-2012", demographics = TRUE)

# Find the sample size for urinary triclosan
nhanes_sample_size(dat,
                   column = "URXTRS",
                   comment_column = "URDTRSLC",
                   weights_column = "WTSA2YR")

# Compute the detection frequency of urinary triclosan
nhanes_detection_frequency(dat,
                           column = "URXTRS",
                           comment_column = "URDTRSLC",
                           weights_column = "WTSA2YR")

# Compute 95th and 99th quantiles for urinary triclosan
nhanes_quantile(dat,
                column = "URXTRS",
                comment_column = "URDTRSLC",
                weights_column = "WTSA2YR",
                quantiles = c(0.95, 0.99))

# Compute geometric mean of urinary triclosan
nhanes_geometric_mean(dat,
                      column = "URXTRS",
                      weights_column = "WTSA2YR")

# Plot a histogram of the urinary triclosan distribution
nhanes_hist(dat,
            column = "URXTRS",
            comment_column = "URDTRSLC",
            weights_column = "WTSA2YR")

# Build a survey design object for use with survey package
design <- nhanes_survey_design(dat, weights_column = "WTSA2YR")
