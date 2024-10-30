# testing
library(knitr)
kable(head(mtcars[, 1:6]))
str(cars)
# if (!require('shiny')) install.packages('shiny')
# demo('notebook', package = 'knitr')

# purl('MyExposome_1527_v2.Rmd')

#Makeing a classificaion lookup table for Steven
AllChemClass<- masterParam %>%
  left_join(class_L,by="ParameterID") %>%
  select(-ParameterID) %>%
  mutate(Result=1) %>%
  spread(key=classification,value=Result,fill=0) %>%
  arrange(-`Polycyclic Aromatic Hydrocarbon (PAH)`)

write.csv(AllChemClass, file = "results_output\\AllChemClass.csv", row.names=FALSE)
