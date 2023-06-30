#### merging of the data----
install.packages("knitr")
install.packages("kabbleExtra")
install.packages("rstatix")


### Load the required package
library(openxlsx)
library(dplyr)
library(knitr)
library(kableExtra)
library(tableone)
library(rstatix)


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


#### the merged dataset collapsed the colnames from the two datasets IMPAC and MP dataset
### and acelled the total merged dataset


#### import the refined merged dataset where you have 199 nparticipant
## set working directory
setwd("C:/Users/Yp/Desktop/luminex/FOR USE")

### import the datset
Charalampos_data <- read.csv("Refined_merged_data.csv", header = T)

###import the fever only dataset
Fever_Only <- read.xlsx("IMPAC_FEVER_DrugOnly.xlsx")


####include the fever information 
Charalampos_data_F <- merge(Charalampos_data, Fever_Only, 
                            by = "IMPAC_ID",all.x = T )


### include information about Nets and Cytokines for admission and 24hrs
NETs_and_Luminex <- read.csv("NETS_Luminex_data.csv", header = TRUE)


### include information on gender
fever_subset <- read.csv("Charalampos_Fever_Subset.csv")
sex <- fever_subset$Sex
NETs_and_Luminex$Sex <- sex


###Use Table one for finding difference----

####identify the variable names
dput(names(NETs_and_Luminex))

#### Vector of the variables to summarize

My_Vars <- c( "MIP1.alpha", "Il.1.beta", "IL.4","Ip.10", "Il.6", "IL.8.x", "Il.10", 
              "IL.12p70.x", "Il.13", "Il17.A", 
              "IFN.gamma", "GM.CSF", "TNF.alpha", "MIP1.beta", "IFN.alpha.x", 
              "MCP.1", "CD62P", "Il.1.alpha", "I.CAM.1", "CD62E..E.Selectin.", 
              "VEGFR2.KDR.Flk.1", "vWF.A2", "L.Selectin.CD62L", "S100B", "Angiopoetin.2", 
              "Osteoprotegerin.TNFRSF11B", "Osteopontin.OPN", "Syndecan.1.CD138", 
              "Tau", "CD31.PECAM.1", "CX3CL1", "Thrombomodulin.BDCA.3", "ADAMTS13", 
              "G.CSF", "VCAM.1.CD106", 
              "Angiopoetin.1", "MIP.1.alpha_.24hrs.", "Il.1.beta_.24hrs.", 
              "IL.4_.24hrs.", "Ip.10_.24hrs.", "Il.6_.24hrs.", "IL.8.y", "Il.10_.24hrs.", 
              "IL.12p70.y", "Il.13_.24hrs.", "Il17.A_.24hrs.", "IFN.gamma_.24hrs.", 
              "GM.CSF_.24hrs.", "TNF.alpha_.24hrs.", "MIP1.beta_.24hrs.", "IFN.alpha.y", 
              "MCP.1_24hrs", "CD62P_24hrs", "Il.1.alpha_24hrs", "I.CAM.1_24hrs", 
               "CD62E..E.Selectin._24hrs", "VEGFR2.KDR.Flk.1_24hrs", "vWF.A2_24hrs", 
              "L.Selectin.CD62L_24hrs", "S100B_24hrs", "Angiopoetin.2_24hrs", 
              "Osteoprotegerin.TNFRSF11B_24hrs", "Osteopontin.OPN_24hrs", 
               "Syndecan.1.CD138_24hrs", "Tau_24hrs", "CD31.PECAM.1_24hrs", 
              "CX3CL1_24hrs", "Thrombomodulin.BDCA.3_24hrs", "ADAMTS13_24hrs", 
               "G.CSF_24hrs", "VCAM.1.CD106_24hrs", "Angiopoetin.1_24hrs", 
              "NETs_adm", "NETs_24hrs")


####create a vector of categorical variables that need transformation
catVars <- c("Sex", "Category")
NETs_and_Luminex[catVars] <- lapply(NETs_and_Luminex[catVars], factor)


###create a tableone object

tab2 <- CreateTableOne(vars = My_Vars, data = NETs_and_Luminex, factorVars = catVars)
tab2
str(NETs_and_Luminex$Sex)
View(NETs_and_Luminex)

###Show all levels for categorical variables
print(tab2, showAllLevels = TRUE, formatOptions = list(big.mark=","))


#### Show the summary of the variables including the missing values
summary(tab2)


####finding non-normally distributed parameters
norm_test <- NETs_and_Luminex %>% shapiro_test(MIP1.alpha, Il.1.beta, IL.4,Ip.10, Il.6, IL.8.x, Il.10, 
                                        IL.12p70.x, Il.13, Il17.A, 
                                        IFN.gamma, GM.CSF, TNF.alpha, MIP1.beta, IFN.alpha.x, 
                                        MCP.1, CD62P, Il.1.alpha, I.CAM.1, CD62E..E.Selectin., 
                                        VEGFR2.KDR.Flk.1, vWF.A2, L.Selectin.CD62L, S100B, Angiopoetin.2, 
                                        Osteoprotegerin.TNFRSF11B, Osteopontin.OPN, Syndecan.1.CD138, 
                                        Tau, CD31.PECAM.1, CX3CL1, Thrombomodulin.BDCA.3, ADAMTS13, 
                                        G.CSF, VCAM.1.CD106, 
                                        Angiopoetin.1, MIP.1.alpha_.24hrs., Il.1.beta_.24hrs., 
                                        IL.4_.24hrs., Ip.10_.24hrs., Il.6_.24hrs., IL.8.y, Il.10_.24hrs., 
                                        IL.12p70.y, Il.13_.24hrs., Il17.A_.24hrs., IFN.gamma_.24hrs., 
                                        GM.CSF_.24hrs., TNF.alpha_.24hrs., MIP1.beta_.24hrs., IFN.alpha.y, 
                                        MCP.1_24hrs, CD62P_24hrs, Il.1.alpha_24hrs, I.CAM.1_24hrs, 
                                        CD62E..E.Selectin._24hrs, VEGFR2.KDR.Flk.1_24hrs, vWF.A2_24hrs, 
                                        L.Selectin.CD62L_24hrs, S100B_24hrs, Angiopoetin.2_24hrs, 
                                        Osteoprotegerin.TNFRSF11B_24hrs, Osteopontin.OPN_24hrs, 
                                        Syndecan.1.CD138_24hrs, Tau_24hrs, CD31.PECAM.1_24hrs, 
                                        CX3CL1_24hrs, Thrombomodulin.BDCA.3_24hrs, ADAMTS13_24hrs, 
                                        G.CSF_24hrs, VCAM.1.CD106_24hrs, Angiopoetin.1_24hrs, 
                                        NETs_adm, NETs_24hrs)



### check statistically significant non normal samples
View(norm_test)

write.csv(norm_test, "normal_distribution_check.csv")


### put the non-normally distributed parameters in one variable
Non_normal <- c("MIP1.alpha", "Il.1.beta", "IL.4","Ip.10", "Il.6", "IL.8.x", "Il.10", 
                "IL.12p70.x", "Il.13", "Il17.A", 
                "IFN.gamma", "GM.CSF", "TNF.alpha", "MIP1.beta", "IFN.alpha.x", 
                "MCP.1", "CD62P", "Il.1.alpha", "I.CAM.1", "CD62E..E.Selectin.", 
                "VEGFR2.KDR.Flk.1", "vWF.A2", "L.Selectin.CD62L", "S100B", "Angiopoetin.2", 
                "Osteoprotegerin.TNFRSF11B", "Osteopontin.OPN", "Syndecan.1.CD138", 
                "Tau", "CD31.PECAM.1", "CX3CL1", "Thrombomodulin.BDCA.3", "ADAMTS13", 
                "G.CSF", "VCAM.1.CD106", 
                "Angiopoetin.1", "MIP.1.alpha_.24hrs.", "Il.1.beta_.24hrs.", 
                "IL.4_.24hrs.", "Ip.10_.24hrs.", "Il.6_.24hrs.", "IL.8.y", "Il.10_.24hrs.", 
                "IL.12p70.y", "Il.13_.24hrs.", "Il17.A_.24hrs.", "IFN.gamma_.24hrs.", 
                "GM.CSF_.24hrs.", "TNF.alpha_.24hrs.", "MIP1.beta_.24hrs.", "IFN.alpha.y", 
                "MCP.1_24hrs", "CD62P_24hrs", "Il.1.alpha_24hrs", "I.CAM.1_24hrs", 
                "CD62E..E.Selectin._24hrs", "VEGFR2.KDR.Flk.1_24hrs", "vWF.A2_24hrs", 
                "L.Selectin.CD62L_24hrs", "S100B_24hrs", "Angiopoetin.2_24hrs", 
                "Osteoprotegerin.TNFRSF11B_24hrs", "Osteopontin.OPN_24hrs", 
                "Syndecan.1.CD138_24hrs", "Tau_24hrs", "CD31.PECAM.1_24hrs", 
                "CX3CL1_24hrs", "Thrombomodulin.BDCA.3_24hrs", "ADAMTS13_24hrs", 
                "G.CSF_24hrs", "VCAM.1.CD106_24hrs", "Angiopoetin.1_24hrs", 
                "NETs_adm", "NETs_24hrs")


###select the non normal values
non_norma_values = subset(norm_test, p < 0.05)
write.csv(non_norma_values, "non_normal_values_fever.csv")


### PUT this values in a variable
Biomarkers <- non_norma_values$variable


###summarizing non-normal values
print(tab2,nonnormal = Biomarkers, formatOptions = list(big.mark = ","), 
      showAllLevels = F)

#?print.TableOne


##### Stratify the data according to the treatment
tab3 <- CreateTableOne(vars = My_Vars, 
                       strata = "Category",
                       data = NETs_and_Luminex,
                       factorVars = catVars)
total_data <- print(tab3, nonnormal = Biomarkers,formatOptions = list(big.mark=","))

total_data <- as.data.frame(total_data)

View(total_data)

### Download this
write.csv(total_data,"total_data_comparison.csv")



























