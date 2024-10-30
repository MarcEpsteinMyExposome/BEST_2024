# USING DARTMOUTH PAH DATA aS SAMPLE

# There are OTHER CHARTS we can do to compare these 2 bands... we could do a simple BAR CHART for each chemical showing amount...
#     not clear that that isn't BETTER than this diff chart

#library(ggplot2)
#theme_set(theme_bw())
#oldBand<-"A180367"
#newBand<-"A180368"
newBand<-"A180371"
oldBand<-"A180372"

x<-testResults.big %>%
  filter(SampleNumber %in% c(oldBand,newBand)) %>%   # PIck only 2 bands
  select(ParameterID,ParameterName,SampleNumber,Result) # Pick only columns we need <this is unnecessar>


x2<- x %>%
  spread(SampleNumber,Result) %>%  # Create 2 new columns, one for EACH of the two bands we're analyzing
  mutate(diff=!!as.name(newBand)-!!as.name(oldBand)) %>%   # Subtrack NEW from OLD
  mutate(aboveBelow = if_else(diff<0,"below","above")) %>%  # This is just to separate by color/label on chart
  arrange(diff) %>%  # Make sure that it is order of DIFF from smallest to largets
  mutate(ParameterName=factor(ParameterName,levels=ParameterName))  # Convert DIFF to factor to make it not resort

# Diverging Barchart
ggplot(x2, aes(x = ParameterName, y = diff)) +
  geom_bar(stat = 'identity', aes(fill = aboveBelow), width = .5)  +
  scale_fill_manual(
    name = "Change",
    labels = c("Above Previous Value", "Below Previous Value"),
    values = c("above" = "#00ba38", "below" = "#f8766d")
  ) +
  labs(subtitle = "How much each compound changed (UP or DOWN)",
       title = "Change from FIrst band to Second Band") +
  xlab("Chemicals") +
  ylab("Change in Value") +
  theme(axis.text.x = element_text(angle = 70,
                                   hjust = 1,
                                   #  colour=columnColors,
                                   #size=6),  #or leave blank?
  ))



## ANOTHER ATTEMPT to show this information:
# Create side-by-side BAR graph showing first vs 2nd band values
# Grouped
ggplot(x, aes(fill=SampleNumber, y=Result, x=ParameterName)) +
  geom_bar(position="dodge", stat="identity") +
  labs(subtitle = "For Each detected Chemical",
     title = paste("First Band(",oldBand,") vs Second Band(",newBand,")",sep="")) +
  xlab("Chemicals") +
  ylab("Change in Value") +
  theme(axis.text.x = element_text(angle = 70,
                                   hjust = 1,
                                   #  colour=columnColors,
                                   #size=6),  #or leave blank?
  ))
