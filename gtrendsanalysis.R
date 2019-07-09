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
dat <- read_excel("/Users/sritarunit/Documents/ESRD_National trends.xlsx")
#pearson, both vars need to be normally dist  #spearman, not normally distributed
# hist(dat$ESRD_INCIDENCE) #unsure
# hist(dat$Kidney_failure) #normal
# hist(dat$ESRD search term)  #normal
# hist(dat$`Blood in urine`)  #normal
# hist(dat$Dialysis)  #normal
# hist(dat$Swollen feet)  #normal
# hist(dat$Kidney_damage) #normal
# hist(dat$protein_in_urine)  #normal
# hist(dat$ESRD_Facilities_per_1000)  #normal

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
mydata<- readRDS("/Users/sritarunit/esrd_search_trends/corr_data.RDS")
val_2_adj <- map_dbl(search_terms2, function(term){partial.r(mydata, x=c("ESRD_INCIDENCE",term),y= "ESRD_FAC_PER_THOUSAND",method ="spearman")[1,2]})
val_2_unadj <- as.numeric(unlist(map(search_terms2, function(term){cor.test(mydata[,"ESRD_INCIDENCE"],mydata[,term],method ="spearman")[4]})))
tab_1 <- as.data.frame(cbind(Search_Terms = search_terms2, Adj = as.numeric(val_2_adj), Unadj = as.numeric(val_2_unadj)))
saveRDS(tab_1, "/Users/sritarunit/esrd_search_trends/tab_1.RDS")
#ESRD_INCIDENCE ~ ESRD_FAC_PER_THOUSAND
mydata <-as.data.frame(mydata)
cor.test(mydata$ESRD_INCIDENCE, mydata$ESRD_FAC_PER_THOUSAND, method = "spearman")[4]
