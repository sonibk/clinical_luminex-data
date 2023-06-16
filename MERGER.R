#### merging of the data----

### Load the required package

library(openxlsx)
library(dplyr)


### set the working directory
setwd("C:/Users/Yp/Desktop/luminex/FOR USE")



### import the datasets
IMPAC_file <- read.csv("IMPAC_BK.csv", header = TRUE)
MP_data <- read.csv("mp_data_csv.csv", header = TRUE)
Parasit  <- read.xlsx("2023 parsitemia and HRP2.xlsx",rowNames = F )
IMPAC_ONLY <- read.csv("EDITED_IMPAC_ONLY_BK.csv", header = T)


### Merging----



### add infomation from the IMPAC only dataset
merge_data4 <- merge(IMPAC_file, IMPAC_ONLY,by = "IMPAC_ID", all.x = TRUE )



### Add infomation about the MP-IDs
merge_data5 <- merge(merge_data4, MP_data, by = "MP_ID", all.x = TRUE)
View(merge_data5)


#### Add infomation about parasitemia
merge_data6 <- merge(merge_data5, Parasit, by = "MP_ID", all.x = TRUE)


#### Write the csv file
write.csv(merge_data6, "total merged dataset.csv")
  

