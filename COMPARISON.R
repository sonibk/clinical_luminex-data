###load the packages
library(ggpubr)
library(ggplot2)
library(rstatix)
library(patchwork)


###Set the working Directory
setwd("C:/Users/Yp/Desktop/luminex/FOR USE")


####load the csv
### note this CSV only contains data for samples with matching admmission and 24hrs data 
ADM_24 <- read.csv("Adm_24hrs data.csv", header = T)


####Check the variable names
parameters <- dput(names(ADM_24))


####remove the  ID names , category and Time point
parameters <- parameters[-c(1:3)]


#### look at the normality of the normality of the data.
norm.test <- ADM_24 %>% shapiro_test(parameters) 


####put the non - parametric together and the parametic together
non_parametric_variables <- subset(norm.test , p < 0.05)
parametric_variables <- subset(norm.test, p > 0.05)

#### This is a pairwise comparison as they are different measurements of the same samples
###note for the parametric use paired t-test
###for the non-parametric use a paired wilcoxon test

### carry out the paired T-TEST OR WILCOXON test based on the results from normality testing
ggpaired(ADM_24, x = "Time.point", y = "MIP1.alpha",
         color = "Time.point", line.color = "gray", line.size = 0.4,
         palette = "jco")+
  stat_compare_means(paired = TRUE, label = "p.signif", label.y = 1000)+
  facet_wrap(~ Category, ncol = 2)+
  labs(x = "Time", y = "MIP1 alpha", title = "MIP1.alpha")+
  theme(legend.position = "none",
        strip.text = element_text(face = "bold", size = 15),
        axis.title.x = element_text(size = 16),
        axis.text.x = element_text(size =  12),
        axis.text.y = element_text(size =  12),
        axis.title.y = element_text(size = 16),
        plot.title = element_text(size = 22, face = "bold",hjust = 0.5))

###sit back relax and enjoy looking at your results :)











