# Testing DPLYR
if (!require("tidyr")){ install.packages("tidyr", dependencies = TRUE); suppressMessages(library(tidyr)) }#LOAD tidyr
if (!require("dplyr")) {  install.packages("dplyr", dependencies = TRUE); suppressMessages(library(dplyr))}
library(dplyr)
iris
dplyr::tbl_df(iris)
iris
dplyr::glimpse(iris)
as_tibble(iris)
utils::View(iris)
View(iris)



#SUMMARISE wCompute separate summary row for each group.
iris %>%
  group_by(Species) %>%
  summarise(avg = mean(Sepal.Width)) %>%
  arrange(avg)
#
#MUTATE wCompute separate summary row for each group but leave whole data set
iris %>%
  group_by(Species) %>%
  mutate(avg = mean(Sepal.Width)) %>%
  arrange(avg)



iris



#
iris %>%
  group_by(Species) %>%
  summarise_all(funs(mean))

as_tibble(iris)
by_species <- iris %>% group_by(Species)
str(by_species)

# One function
by_species %>% summarise_all(n_distinct)
by_species %>% summarise_all(mean)

# Use the _at and _if variants for conditional mapping.
by_species %>% summarise_if(is.numeric, mean)


# summarise_at() can use select() helpers with the vars() function:
by_species %>% summarise_at(vars(Petal.Width), mean)
by_species %>% summarise_at(vars(matches("Width")), mean)

# You can provide additional arguments. Those are evaluated only once:
by_species %>% summarise_all(mean)
by_species %>% summarise_all(mean, trim = 1)

# You can provide an expression or multiple functions with the funs() helper.
by_species %>% mutate_all(funs(. * 0.4))
by_species %>% summarise_all(funs(min, max))

by_species %>% summarise_all(funs(min,n_distinct))


# Note that output variable name must now include function name, in order to
# keep things distinct.

# Function names will be included if .funs has names or whenever multiple
# functions are used.
by_species %>% mutate_all(funs("in" = . / 2.54))
by_species %>% mutate_all(funs(rg = diff(range(.))))
by_species %>% summarise_all(funs(med = median))
by_species %>% summarise_all(funs(Q3 = quantile), probs = 0.75)
by_species %>% summarise_all(c("min", "max"))
by_species %>% summarise_all(funs(min,max))

# Two functions, continued
by_species %>% summarise_at(vars(Petal.Width, Sepal.Width), funs(min, max))
by_species %>% summarise_at(vars(matches("Width")), funs(min, max))
by_species %>% summarise_at(vars(matches("Width")), min)



#Summarise w/o group_by
iris %>%
  summarise(avg = mean(Sepal.Width)) %>%
  arrange(avg)
#
#MUTATE Compute new variables by group.
iris %>%
  group_by(Species) %>%
  mutate(avg = mean(Sepal.Width)) %>%
  arrange(avg)
# Mutate w/o group_by
iris %>%
  mutate(avg = mean(Sepal.Width)) %>%
  arrange(avg)
#
iris<-as_tibble(iris)
iris
count(iris)

as_tibble(iris) %>%
  group_by(Species) %>%
  dplyr::summarise(n=dplyr::n()) %>%
  #  ungroup %>%
  as.data.frame

#FILTER
iris %>%
  filter( Sepal.Length > 7)




