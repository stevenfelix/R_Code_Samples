# Sample analyses from an MTurk survey study
# Steven Felix

#
# Description:  These are excerpts from my script used to produce analyses for a manuscript.
#

rm(list = ls())
search()

# packages ----------------------------------------------------------------

library(effects)
library(psych)
library(car)
library(dplyr)
library(magrittr)
library(corrplot)
library(arm)

# Opening data ------------------------------------------------------------

setwd("~/Dropbox/Research/Dissertation Project/Study 1 - PC Correlates/Data/Rdata")
load("finalSurveyData.Rdata")
data <- final
rm(final)

# Functions ---------------------------------------------------------------

cor.table <- function(x){
  tables <- corr.test(x)
  rtable <- tables$r
  #rtable[!tables$p <= .05] <- NA
  nostar <- rtable[tables$p > .05]
  onestar <- rtable[tables$p <= .05 & tables$p > .01]
  twostar <- rtable[tables$p <= .01 & tables$p > .001]
  threestar <- rtable[tables$p <= .001]
  rtable[tables$p <= .05 & tables$p > .01] <- paste(format(onestar, digits = 2),"*",sep="")
  rtable[tables$p <= .01 & tables$p > .001] <- paste(format(twostar, digits = 2),"**",sep="")
  rtable[tables$p <= .001] <- paste(format(threestar, digits = 2),"***",sep="")
  rtable[tables$p > .05] <- format(nostar, digits = 2)
  diag(rtable) <- NA
  ptable <- tables$p
  ptable <- format(ptable, digits = 3)
  diag(ptable) <- NA
  ntable <- tables$n
  row.names(ptable) <- paste(row.names(ptable),"p",sep="_")
  if(is.matrix(ntable)){
    row.names(ntable) <- paste(row.names(ntable),"n",sep="_")
  }
  rptables <-rbind(rtable,NA,ptable,ntable)
  return(rptables)
}


# Sample Descriptives ------------------------------------------------------------

datanames <- names(data)[order(names(data))]

# descriptives (mean, median, range...) for numeric variables
library(psych)
tab = c()
for(name in names(data)){
  x <- is.numeric(data[,name])
  tab = c(tab, x)
}
table1 <- describe(data[,tab])
table1 <- as.data.frame(table1)
table1

library(xlsx) # output to excel
write.xlsx(table1, 
           file = "/Users/samf526/Dropbox/Research/Dissertation Project/Manuscripts/Whats in a PC rating/table1.xlsx", showNA = FALSE)
detach(package:xlsx)

# who did people choose for most important category?
table(data$ImpPerson) # absolute frequencies
as.data.frame(format(table(data$ImpPerson) / sum(table(data$ImpPerson)), digits = 2)) # relative freq.

# Sample Correlations --------------------------------------------------------------

# vector of PC variable names
PC.var.names1 <- c("PC_Partner_1","PC_Mother_1","PC_Father_1","PC_Friend_1","PC_Other_1")

# Correlation table for all data
tab <- corr.test(data[,c(PC.var.names1)]) # holm adjust
tab

# Sample Correlation Plots ------------------------------------------------------------------

## Plots: PC and individual variables
rMatrix <- psych:::corr.test(y = data[,c("PC_Mother_1","PC_Father_1","PC_Partner_1","PC_Friend_1","PC_Other_1")],
                     x = data[,c("DAS.fs","SPS_total", "SPANE_neg", "SPANE_pos")],
                     adjust = "holm")
rMatrix2 <- rMatrix$r
row.names(rMatrix2) <- c("Dysfunctional Attitudes", "Perceived Support","Negative Moods", "Positive Moods")
colnames(rMatrix2) <- c("Mother","Father","Partner", "Friend", "Other")
col3 <- colorRampPalette(c("black","grey","white","grey","black"))

x11()
cogTable <- corrplot:::corrplot(rMatrix2, method = "shade", col = col3(20), 
                                addCoef.col = "black", tl.srt  = 60, tl.offset = .8,
                                p.mat = rMatrix$p, tl.col = "black", insig = 'blank', 
                                addshade = 'all',cl.pos = "n", title = "Figure 1")


# Sample Backwards Regression ----------------------------------------------------------

# biggest Partner PC correlations:
#     -SPANE_neg (.34)
#     -PNRQ-neg (.44)
#     -AC-neg (.43)
#     -CSI - (-.36)
#     -PC_Average_nonpartner (.48)
#     -PC Mother - (.37)
#     -PC Other - (.41)
#     -PC Friend - (.42)

library(MASS)

# data with no NAs
dataComp <- data[complete.cases(data[,c("PC_Partner_1", "PC_Average_1_nonpartner","PNRQ_neg", "ACS_neg","SPANE_neg", "CSI", "SPS_total")]),] 

# backwards regression
mod <- lm(PC_Partner_1 ~ PC_Average_1_nonpartner + PNRQ_neg + ACS_neg + CSI +SPANE_neg + SPS_total, 
          data = dataComp)
step <- stepAIC(mod, direction = "both")
step$anova

# final model
mod <- lm(scale(PC_Partner_1) ~ scale(PC_Average_1_nonpartner) + scale(PNRQ_neg) + scale(ACS_neg), data = data)
summary(mod)

# check residuals
res <- rstandard(mod)
plot(res ~ mod$fitted.values, pch = 20, xlab = "Fitted Values", ylab = "Standardized Residuals")
mline <- lm(res ~ mod$fitted.values)
summary(mline)
abline(a = mline$coefficients[1], b = mline$coefficients[2]) # good!

summary(mod) # strongest predictors

# Standardized regression coefficients
library(lm.beta)
betas <- lm.beta(mod)
as.data.frame(betas$standardized.coefficients) # no standard errors

# re-run regression with standardized variables, for Standard Errors
Stmod <- lm(scale(PC_Partner_1) ~ scale(PC_Average_1_nonpartner) + scale(PNRQ_neg) + scale(ACS_neg), data = data)
summary(Stmod)