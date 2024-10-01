# Massage testResults.big to give steven more air concentration info
# assume we've run base code
#
# Get rid of zero values

tr <- testResults.big %>%
  filter(Ca_for_Customer > 0)

trStat<- tr %>%
  group_by(ParameterName) %>%
  summarise(Ca_for_Customer_min=min(Ca_for_Customer),
            Ca_for_Customer_mean=mean(Ca_for_Customer),
            Ca_for_Customer_max=max(Ca_for_Customer))
write.csv(trStat,"GiveSteven.csv")
