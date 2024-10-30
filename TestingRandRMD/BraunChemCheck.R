library(tidyr)

braun <- read.table(
  "data/BraunChems.csv",
  sep = ",",
  header = TRUE,
  colClasses = "character" # Import all as character
  ,
  comment.char = "" # Don't allow use of "#" as comment in input
  ,
  quote = "\"",
  fileEncoding = "UTF-8-BOM"
)

braun$CASNumber
masterParam$CASNumber


length(intersect(braun$CASNumber,masterParam$CASNumber)) # 311

write.csv(masterParam[masterParam$CASNumber %in% intersect(braun$CASNumber,masterParam$CASNumber),],"braun_overlap_with_DRS.csv")
