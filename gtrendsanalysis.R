library(tidyverse)
library(readxl)
library(tableone)
library(aod)
install.packages("readxl")
library("readxl")
install.packages("ppcor")
library(ppcor)
install.packages("datasets")
library("datasets")
install.packages('latex2exp')
dat <- read_excel(___________.xlsx")
#pearson, both vars need to be normally dist  #spearman, not normally distributed
hist(dat$ESRD_INCIDENCE) #unsure
hist(dat$Kidney_failure) #normal
hist(dat$ESRD search term)  #normal
hist(dat$`Blood in urine`)  #normal
hist(dat$Dialysis)  #normal
hist(dat$Swollen feet)  #normal
hist(dat$Kidney_damage) #normal
hist(dat$protein_in_urine)  #normal
hist(dat$ESRD_Facilities_per_1000)  #normal
pcor.test(dat$ESRD_INCIDENCE, dat$Kidney_failure, dat$ESRD_Facilities_per_1000, method = "spearman") #adjusted, unadjusted
pcor.test(dat$ESRD_INCIDENCE, dat$`ESRD search term`, dat$ESRD_Facilities_per_1000, method = "spearman")
pcor.test(dat$ESRD_INCIDENCE, dat$`Blood in urine`, dat$ESRD_Facilities_per_1000, method = "spearman")
pcor.test(dat$ESRD_INCIDENCE, dat$Dialysis, dat$ESRD_Facilities_per_1000, method = "spearman")
pcor.test(dat$ESRD_INCIDENCE, dat$`Swollen feet`, dat$ESRD_Facilities_per_1000, method = "spearman")
pcor.test(dat$ESRD_INCIDENCE, dat$Kidney_damage, dat$ESRD_Facilities_per_1000, method = "spearman")
pcor.test(dat$ESRD_INCIDENCE, dat$protein_in_urine, dat$ESRD_Facilities_per_1000, method = "spearman")
pcor.test(dat$ESRD_INCIDENCE, dat$`sesame street: (01/01/2014 - 31/12/2016)`, dat$ESRD_Facilities_per_1000, method = "spearman")
cor.test(dat$ESRD_INCIDENCE, dat$`sesame street: (01/01/2014 - 31/12/2016)`, method = "spearman")
pcor.test(dat$ESRD_INCIDENCE, dat$Kidney_failure, dat$healthcarerank, method = "spearman") #adjusted, unadjusted
cor.test(dat$ESRD_INCIDENCE, dat$Kidney_failure, method = "spearman") #adjusted, unadjusted

dat_1 <- read_csv("/Users/sritarunit/Downloads/geoMap.csv", skip = 1)
library(ggplot2)
setNames(state.abb, state.name)[c(dat$Region)]
m <- lm(dat$Kidney_failure ~ dat$ESRDPREVALENCE2016)
a <- signif(sqrt(summary(m)$r.squared), digits = 2)
require(latex2exp)
textlab <- paste(TeX('$\\rho$') + a + sep = "=")
ggplot(dat, aes(x=ESRDPREVALENCE2016, y=Kidney_failure)) + geom_point(alpha = 0.05) + geom_text(label=setNames(state.abb, state.name)[c(dat$Region)], position = position_dodge(width=0.01),  size=3.5) + geom_smooth(method = "lm", se = TRUE) + annotate("text", x = 245, y = 90, label = sprintf("%s",TeX('$\\rho$')), color="blue", size = 5, parse=TRUE) + annotate("text", x = 275, y = 90, label = sprintf(" \" = %s \"",a), color="blue", size = 5, parse=TRUE)

#`sesame street: (01/01/2014 - 31/12/2016)`

#Table
vec <- colnames(dat)[2:8]

adj <- vector(mode="numeric", length=7)
install.packages("psych")
library("psych")


search_terms <- colnames(dat)[-c(1,2)]

dat <- as.data.frame(dat)

mydata <- cbind(dat$ESRD_INCIDENCE, dat$`sesame street: (01/01/2014 - 31/12/2016)`, dat$ESRD_Facilities_per_1000,dat$Kidney_failure,dat$`Blood in urine`,dat$Dialysis
                , dat$`Swollen feet`, dat$Kidney_damage, dat$protein_in_urine)







colnames(mydata) <- c("ESRD_INCIDENCE","ESRD_SEARCH_TERM","ESRD_FAC_PER_THOUSAND","Kidney_failure","Blood_in_urine","Dialysis","Swollen_feet",
                      "Kidney_damage","protein_in_urine")

partial.r(data=mydata, x=c("ESRD_INCIDENCE","ESRD_SEARCH_TERM"), y="ESRD_FAC_PER_THOUSAND")


partial_r_esrd_inc <- function(searchterm,adjustterm,data = mydata){
  partial.r(data, x=c("ESRD_INCIDENCE",searchterm),y=adjustterm,method ="spearman")
}

search_terms2 <- c("ESRD_SEARCH_TERM","Kidney_failure","Blood_in_urine","Dialysis","Swollen_feet",
                  "Kidney_damage","protein_in_urine")
saveRDS(mydata,"/Users/sritarunit/esrd_search_trends/corr_data.RDS")
val_2_adj <- map_dbl(search_terms2, function(term){partial.r(mydata, x=c("ESRD_INCIDENCE",term),y= "ESRD_FAC_PER_THOUSAND",method ="spearman")[1,2]})
val_2_unadj <- as.numeric(unlist(map(search_terms2, function(term){cor.test(mydata[,"ESRD_INCIDENCE"],mydata[,term],method ="spearman")[4]})))
tab_1 <- as.data.frame(cbind(Search_Terms = search_terms2, Adj = as.numeric(val_2_adj), Unadj = as.numeric(val_2_unadj)))
saveRDS(tab_1, "/Users/sritarunit/esrd_search_trends/tab_1.RDS")


#adj[2] <- partial.r(data=mydata, x=c("ESRD_INCIDENCE","`ESRD search term`"), y="ESRD_Facilities_per_1000")
adj[1] <- pcor.test(dat$ESRD_INCIDENCE, dat$Kidney_failure, dat$ESRD_Facilities_per_1000, method = "spearman")[1]
adj[2] <- partial.r(data=mydata, x=c("ESRD_INCIDENCE","ESRD_SEARCH_TERM"), y="ESRD_FAC_PER_THOUSAND",method="spearman")[1,2]
#adj[2] <- pcor.test(dat$ESRD_INCIDENCE, dat$`ESRD search term`, dat$ESRD_Facilities_per_1000, method = "spearman")[1]
adj[3] <- pcor.test(dat$ESRD_INCIDENCE, dat$`Blood in urine`, dat$ESRD_Facilities_per_1000, method = "spearman")[1]
adj[4] <- pcor.test(dat$ESRD_INCIDENCE, dat$Dialysis, dat$ESRD_Facilities_per_1000, method = "spearman")[1]
adj[5] <- pcor.test(dat$ESRD_INCIDENCE, dat$`Swollen feet`, dat$ESRD_Facilities_per_1000, method = "spearman")[1]
adj[6] <- pcor.test(dat$ESRD_INCIDENCE, dat$Kidney_damage, dat$ESRD_Facilities_per_1000, method = "spearman")[1]
adj[7] <- pcor.test(dat$ESRD_INCIDENCE, dat$protein_in_urine, dat$ESRD_Facilities_per_1000, method = "spearman")[1]

un_adj <- vector(mode="numeric", length=7)
un_adj[1] <- cor.test(dat$ESRD_INCIDENCE, dat$Kidney_failure, method = "spearman")[4]
un_adj[2] <- cor.test(dat$ESRD_INCIDENCE, dat$`ESRD search term`, method = "spearman")[4]
un_adj[3] <- cor.test(dat$ESRD_INCIDENCE, dat$`Blood in urine`, method = "spearman")[4]
un_adj[4] <- cor.test(dat$ESRD_INCIDENCE, dat$Dialysis, method = "spearman")[4]
un_adj[5] <- cor.test(dat$ESRD_INCIDENCE, dat$`Swollen feet`, method = "spearman")[4]
un_adj[6] <- cor.test(dat$ESRD_INCIDENCE, dat$Kidney_damage, method = "spearman")[4]
un_adj[7] <- cor.test(dat$ESRD_INCIDENCE, dat$protein_in_urine, method = "spearman")[4]
adj <- unlist(adj)
un_adj <- unlist(un_adj)
val_2 <- unlist(val_2)
corr_table <- cbind(vec,un_adj, val_2)
corr_table <- as.data.frame(corr_table)
write_csv(corr_table,"GT_Keywords_Analysisresults.csv")












dat$ESRD_INCIDENCE = as.numeric(dat$ESRD_INCIDENCE)
adj <-cor(dat[lapply(dat,run_function(val))])
x = 1
run_function <- function(val){
  pcor.test(dat$ESRD_INCIDENCE, sprintf("dat$%s",x), sprintf("dat$ESRD_Facilities_per_1000"), method = "spearman")[1]
}


for(i in vec)
{adj[x] <- pcor.test(sprintf("dat$ESRD_INCIDENCE"), sprintf("dat$%s",i), sprintf("dat$ESRD_Facilities_per_1000"), method = "spearman")[4
  x=x+1}






pcor.test(dat$ESRD_INCIDENCE, dat_1$`sesame street: (01/01/2014 - 31/12/2016)`, dat$ESRD_Facilities_per_1000, method = "spearman")[1]
cor.test(dat$ESRD_INCIDENCE, dat_1$`sesame street: (01/01/2014 - 31/12/2016)`, method = "spearman")[4]




