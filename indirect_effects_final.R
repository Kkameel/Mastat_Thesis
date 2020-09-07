# Reading in the data
data <- read.csv("meta_data_analysis.csv", sep=";", stringsAsFactors = F)
head(data)

# Libraries
library(lavaan)
library(semPlot)
library(tidyr)
library(dplyr)
library(DataExplorer)
library(corrplot)
library(RColorBrewer)
library(polycor)
library(psych)
library(Hmisc)



# Cleaning up the data
data$P_dist_ord <- factor(data$P_distribution, ordered = TRUE, 
                          levels = c("Nonsignificant", "Significant", 
                                     "Very Significant", "Extremely Significant"),
                          labels = c(0, 1, 2, 3))

data <- data %>% mutate(p_value = case_when(P_distribution == "Nonsignificant" ~ 1, 
                                            P_distribution == "Significant" ~ 2, 
                                            P_distribution == "Very Significant" ~ 3, 
                                            P_distribution == "Extremely Significant" ~ 4))


# Wrong encoding - switching endophoric & evidential
data$Endophoric <- (data$Endophoric / data$Sentences)
data$Evidential <- (data$Evidential / data$Sentences)
data$intext <- (data$In.text.Citations / data$Sentences)
data$Absolutes <- (data$Absolutes / data$Sentences)
data$Achievement <- (data$Achievement / data$Sentences)
data$Reward <- (data$Reward / data$Sentences)
data$Empathics <- (data$Empathy / data$Sentences)
data$Hedges <- (data$Hedges / data$Sentences)
data$Negate <- (data$Negate / data$Sentences)
data$Negative.emotions <- (data$Negative.emotions / data$Sentences)
data$Sensory <- (data$Sensory / data$Sentences)
data$First.Person <- (data$First.Person / data$Sentences)
data$Third.Person <- (data$Third.Person / data$Sentences)


plot_missing(data) ## Are there missing values, and what is the missing data profile?
plot_bar(data) ## How does the categorical frequency for each discrete variable look like?
plot_histogram(data) ## What is the distribution of each continuous variable?

cor(data$citationNumCrossRef, data$CitationNumGoogle) # Going forward with only 1 DV
boxplot(data$citationNumCrossRef)
boxplot(data$CitationNumGoogle)

# Refining the data by removing outliers
Q <- quantile(data$citationNumCrossRef, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(data$citationNumCrossRef)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range
eliminated<- subset(data, data$citationNumCrossRef > (Q[1] - 1.5*iqr) & data$citationNumCrossRef < (Q[2]+1.5*iqr))
boxplot(eliminated$citationNumCrossRef)


boxplot(data$Sample.size)
Q <- quantile(eliminated$Sample.size, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(eliminated$Sample.size)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range
eliminated <- subset(eliminated, eliminated$Sample.size > (Q[1] - 1.5*iqr) & eliminated$Sample.size < (Q[2]+1.5*iqr))
boxplot(eliminated$Sample.size)


require(reshape2)

# melting by "Label". `melt is from the reshape2 package. 
# do ?melt to see what other things it can do (you will surely need it)
data.m <- melt(eliminated)
data.m # pasting some rows of the melted data.frame
require(ggplot2)
ggplot(data = data.m, aes(x=variable, y=value)) + geom_boxplot()



corr_simple <- function(data=df,sig=0.5){
  #convert data to numeric in order to run correlations
  #convert to factor first to keep the integrity of the data - each value will become a number rather than turn into NA
  df_cor <- data %>% mutate_if(is.character, as.factor)
  df_cor <- df_cor %>% mutate_if(is.factor, as.numeric)
  #run a correlation and drop the insignificant ones
  corr <- cor(df_cor)
  #prepare to drop duplicates and correlations of 1     
  corr[lower.tri(corr,diag=TRUE)] <- NA 
  #drop perfect correlations
  corr[corr == 1] <- NA 
  #turn into a 3-column table
  corr <- as.data.frame(as.table(corr))
  #remove the NA values from above 
  corr <- na.omit(corr) 
  #select significant values  
  corr <- subset(corr, abs(Freq) > sig) 
  #sort by highest correlation
  corr <- corr[order(-abs(corr$Freq)),] 
  #print table
  print(corr)
  #turn corr back into matrix in order to plot with corrplot
  mtx_corr <- reshape2::acast(corr, Var1~Var2, value.var="Freq")
  
  #plot correlations visually
  corrplot(mtx_corr, is.corr=FALSE, tl.col="black", na.label=" ")
  
}

# Only create a corrplot for relevant data
corr_simple(data = data, sig = 0.4)
corr_simple(data = eliminated, sig = 0.4)

certain_col <- c("Absolutes","Achievement", "Endophoric","Reward", "Empathics")
uncertain_col <-  c("Evidential","Hedges", "intext")
deception_col <-  c("Negate","Negative.emotions", "Sensory", "First.Person", "Third.Person")

certainty <- subset(data, select = certain_col)
cor(certainty)

mean(certainty)
uncertainty <- subset(data, select = uncertain_col)
cor(uncertainty)

deception <- subset(data, select = deception_col)
cor(deception)

# Supporting material
# Creating the basic dataset for the corrplot visual
test_drop <- c("Title", "Author", "P_distribution", "Year", "P_distribution", 
               "P_dist_ord", "Volume", "Issue", "First.page", "Last.page", "Page.length",
               "Subjectivity", "Polarity", "Empathy", "Sentences", "In.text.Citations", "deception_avg", "cert_uncert")
testfile <- data[, ! names(data) %in% test_drop, drop = FALSE]
corr_simple(testfile, sig = 0.4)

# Creating the summary tables for an exploratory overview -> exported to excel to finalize
write.csv(summary(data),"output.csv")
write.csv(describe(data), "additional_output.csv") # additional statistics


#### Eliminated ####???
certainty_avg <- ((eliminated$Absolutes) + (eliminated$Achievement) + (eliminated$Reward) + (eliminated$Empathics) + (eliminated$Endophoric)) / 5
uncertainty_avg <- ((eliminated$Evidential) + (eliminated$Hedges) + (eliminated$intext)) / 3
deception_avg <- ((eliminated$Negate) + (eliminated$Negative.emotions) + (eliminated$Sensory) + (eliminated$First.Person) + (eliminated$Third.Person)) / 5


eliminated$cert_uncert <- (certainty_avg / (certainty_avg + uncertainty_avg))
eliminated$deception_avg <- deception_avg

# Creating the final dataset
dropping <- c("Title", "Author", "P_distribution", "Year", "CitationNumGoogle", "P_distribution", 
              "P_dist_ord", "Volume", "Issue", "First.page", "Last.page", "Page.length",
              "Subjectivity", "Polarity", "Sentences", "In.text.Citations")
finaldata <- eliminated[, ! names(eliminated) %in% dropping, drop = FALSE]


# Rescaling all but the dependent variable
citationNumCrossRef <- finaldata$citationNumCrossRef
finaldata.scale <- as.data.frame(scale( subset(finaldata, select = -citationNumCrossRef) ))
finaldata.scale$citationNumCrossRef <- citationNumCrossRef


# Adding the interaction term to the dataset set, to cover the moderation
finaldata.scale <- finaldata.scale %>% mutate(pval_x_numbauth = p_value * Number.of.Authors)
head(finaldata)

corr_simple(data = finaldata.scale, sig = 0.4)


# Basic mmodel without mediation
basic_model <- '
    # direct effect
      citationNumCrossRef ~ c*p_value
      citationNumCrossRef ~ d*Rank
      citationNumCrossRef ~ e*Special
      citationNumCrossRef ~ f*Word.count
      citationNumCrossRef ~ g*Flesch.Reading.Ease
      citationNumCrossRef ~ h*Time.Delta..months.
      citationNumCrossRef ~ i*Reference.Diversity
      citationNumCrossRef ~ j*Confidence.Interval
      citationNumCrossRef ~ k*Sample.size
      citationNumCrossRef ~ l*Avg.sentence.length

      
      direct := c + d + e + f + g + h + i + j + k + l
  
    # total effect
      total := c + d + e + f + g + h + i + j + k + l 
'

basic_fit <- sem(model = basic_model, data = finaldata.scale, std.lv = FALSE)
summary(basic_fit, standardized = TRUE, fit.measures=TRUE)

mi_basic <- modindices(basic_fit, sort. = TRUE)
mi_basic



# Including both mediations
moderated_mediation_model <- '
    # direct effect
      citationNumCrossRef ~ c*p_value
      citationNumCrossRef ~ d*Rank
      citationNumCrossRef ~ e*Special
      citationNumCrossRef ~ f*Word.count
      citationNumCrossRef ~ g*Flesch.Reading.Ease
      citationNumCrossRef ~ h*Time.Delta..months.
      citationNumCrossRef ~ i*Reference.Diversity
      citationNumCrossRef ~ j*Confidence.Interval
      citationNumCrossRef ~ k*Sample.size
      citationNumCrossRef ~ l*Avg.sentence.length

      
      direct := c + d + e + f + g + h + i + j + k + l
  
    # regressions
      cert_uncert ~ a*p_value
      citationNumCrossRef ~ b*cert_uncert
      
      deception_avg ~ x*p_value
      citationNumCrossRef ~ y*deception_avg
      
      
    # indirect effect (a*b)
      indirect_certainty := a*b
      indirect_deception := x*y
      indirect_total := (a*b) + (x*y)
  
    # total effect
      total := c + d + e + f + g + h + i + j + k + l + (a*b) + (x*y)
'

fit_modmed <- sem(model = moderated_mediation_model, data = finaldata.scale, std.lv = FALSE)
summary(fit_modmed, standardized = TRUE, fit.measures=TRUE)

mi <- modindices(fit_modmed, sort. = TRUE)
mi

# Update the model based upon MI 
moderated_mediation_model2 <- '
    # direct effect
      citationNumCrossRef ~ c*p_value
      citationNumCrossRef ~ d*Rank
      citationNumCrossRef ~ e*Special
      citationNumCrossRef ~ f*Word.count
      citationNumCrossRef ~ g*Flesch.Reading.Ease
      citationNumCrossRef ~ h*Time.Delta..months.
      citationNumCrossRef ~ i*Reference.Diversity
      citationNumCrossRef ~ j*Confidence.Interval
      citationNumCrossRef ~ k*Sample.size
      citationNumCrossRef ~ l*Avg.sentence.length

      
      direct := c + d + e + f + g + h + i + j + k + l
  
    # regressions
      cert_uncert ~ a*p_value
      citationNumCrossRef ~ b*cert_uncert
      
      deception_avg ~ x*p_value
      citationNumCrossRef ~ y*deception_avg
      
      
    # Update modindices
      deception_avg ~ Avg.sentence.length
      
    # indirect effect (a*b)
      indirect_certainty := a*b
      indirect_deception := x*y
      indirect_total := (a*b) + (x*y)
  
    # total effect
      total := c + d + e + f + g + h + i + j + k + l + (a*b) + (x*y)
'

fit_modmed2 <- sem(model = moderated_mediation_model2, data = finaldata.scale, fixed.x = FALSE)
summary(fit_modmed2, standardized = TRUE, fit.measures=TRUE)

mi2 <- modindices(fit_modmed2, sort. = TRUE)
mi2

# Further optimization of the model (withouth moderation)
moderated_mediation_model3 <- '
    # direct effect
      citationNumCrossRef ~ c*p_value
      citationNumCrossRef ~ d*Rank
      citationNumCrossRef ~ e*Special
    #  citationNumCrossRef ~ f*Word.count
      citationNumCrossRef ~ g*Flesch.Reading.Ease
      citationNumCrossRef ~ h*Time.Delta..months.
      citationNumCrossRef ~ i*Reference.Diversity
      citationNumCrossRef ~ j*Confidence.Interval
      citationNumCrossRef ~ k*Sample.size
      citationNumCrossRef ~ l*Avg.sentence.length

      
      direct := c + d + e + g + h + i + j + k + l
  
    # regressions
      citationNumCrossRef ~ b*cert_uncert
      cert_uncert ~ a*p_value
      
      deception_avg ~ x*p_value
      citationNumCrossRef ~ y*deception_avg
      
      
    # Update modindices
      deception_avg ~~ Avg.sentence.length
      Flesch.Reading.Ease ~~ Avg.sentence.length
      Flesch.Reading.Ease ~~ deception_avg 
      
    # indirect effect (a*b)
      indirect_certainty := a*b
      indirect_deception := x*y
      indirect_total := (a*b) + (x*y)
  
    # total effect
      total := c + d + e  + g + h + i + j + k + l + (a*b) + (x*y)
'

fit_modmed3 <- sem(model = moderated_mediation_model3, data = finaldata.scale, fixed.x = FALSE, std.lv = FALSE)
summary(fit_modmed3, standardized = TRUE, fit.measures=TRUE)

mi3 <- modindices(fit_modmed3, sort. = TRUE)
mi3

# Test visualization
semPaths(fit_modmed3, whatLabels = "name")


## Including the moderation 
moderation_included <- '
    # direct effect
      citationNumCrossRef ~ c*p_value
      citationNumCrossRef ~ d*Rank
      citationNumCrossRef ~ e*Special
     # citationNumCrossRef ~ f*Word.count
      citationNumCrossRef ~ g*Flesch.Reading.Ease
      citationNumCrossRef ~ h*Time.Delta..months.
      citationNumCrossRef ~ i*Reference.Diversity
      citationNumCrossRef ~ j*Confidence.Interval
      citationNumCrossRef ~ k*Sample.size
      citationNumCrossRef ~ l*Avg.sentence.length

      
      direct := c + d + e +  g + h + i + j + k + l
  
    # regressions
      citationNumCrossRef ~ b*cert_uncert
      cert_uncert ~ a*p_value
      
      deception_avg ~ x*p_value
      citationNumCrossRef ~ y*deception_avg
      
    # Moderation
      cert_uncert ~ a1*Number.of.Authors
      cert_uncert ~ a2*pval_x_numbauth
      
    #  imm1 := a2*b
      
    # mean of centered write (for use in simple slopes)
      Number.of.Authors ~ number.mean*1

    # variance of centered write (for use in simple slopes)
      Number.of.Authors ~~ number.var*Number.of.Authors
      
    # indirect effects conditional on moderator 
      indirect.SDbelow := a*b + a2*-sqrt(number.var)*b
      indirect.mean := a*b + a2*number.mean*b
      indirect.SDabove := a*b + a2*sqrt(number.var)*b
      
    # Moderation2
      deception_avg ~ x1*Number.of.Authors
      deception_avg ~ x2*pval_x_numbauth
      
      imm2 := x2*y
      
    # indirect effects conditional on moderator 
      indirect.SDbelow2 := x*y + x2*-sqrt(number.var)*y
      indirect.mean2 := x*y + x2*number.mean*y
      indirect.SDabove2 := x*y + x2*sqrt(number.var)*y  

    # indirect effect (a*b)
      indirect_certainty := a*b
      indirect_deception := x*y
      indirect_total := (a*b) + (x*y)
  
    # total effect
      total := c + d + e + g + h + i + j + k + l + (a*b) + (x*y) + a1 + a2 + x1 + x2
'

mode_inc <- sem(model = moderation_included, data = finaldata.scale, fixed.x = FALSE, std.lv = FALSE)
summary(mode_inc, fit.measures = TRUE, standardized = TRUE)

mi_modinc <- modindices(mode_inc, sort. = TRUE)
mi_modinc

# Testing out visuals - yet, too crowded
semPaths(mode_inc, "par", edge.label.cex = 1, fade = FALSE)  #plot our CFA
semPaths(mode_inc, whatLabels = "name")

# Updated model based upon modification indices
moderation_included2 <- '
    # direct effect
      citationNumCrossRef ~ c*p_value
      citationNumCrossRef ~ d*Rank
      citationNumCrossRef ~ e*Special
      citationNumCrossRef ~ g*Flesch.Reading.Ease
      citationNumCrossRef ~ h*Time.Delta..months.
      citationNumCrossRef ~ i*Reference.Diversity
      citationNumCrossRef ~ j*Confidence.Interval
      citationNumCrossRef ~ k*Sample.size
      citationNumCrossRef ~ l*Avg.sentence.length

      
     direct := c + d + e +  g + h + i + j + k + l
  
    # regressions
      citationNumCrossRef ~ b*cert_uncert
      cert_uncert ~ a*p_value
      
      deception_avg ~ x*p_value
      citationNumCrossRef ~ y*deception_avg
      
    # Moderation
      cert_uncert ~ a1*Number.of.Authors
      cert_uncert ~ a2*pval_x_numbauth
      
      imm1 := a2*b
      
    # mean of centered write (for use in simple slopes)
      Number.of.Authors ~ number.mean*1

    # variance of centered write (for use in simple slopes)
      Number.of.Authors ~~ number.var*Number.of.Authors
      
    # indirect effects conditional on moderator 
      indirect.SDbelow := a*b + a2*-sqrt(number.var)*b
      indirect.mean := a*b + a2*number.mean*b
      indirect.SDabove := a*b + a2*sqrt(number.var)*b
      
    # Moderation2
      deception_avg ~ x1*Number.of.Authors
      deception_avg ~ x2*pval_x_numbauth
      
      imm2 := x2*y
      
    # indirect effects conditional on moderator 
      indirect.SDbelow2 := x*y + x2*-sqrt(number.var)*y
      indirect.mean2 := x*y + x2*number.mean*y
      indirect.SDabove2 := x*y + x2*sqrt(number.var)*y  
      
    # Update modindices
      deception_avg  ~ Avg.sentence.length

      
    # indirect effect (a*b)
      indirect_certainty := a*b
      indirect_deception := x*y
      indirect_total := (a*b) + (x*y)
  
    # total effect
      total := c + d + e + g + h + i + j + k + l + (a*b) + (x*y) + a1 + a2 + x1 + x2
'

mode_inc2 <- sem(model = moderation_included2, data = finaldata.scale, fixed.x = FALSE, std.lv = FALSE)
varTable(mode_inc2)
summary(mode_inc2, fit.measures = TRUE, standardized = TRUE)

# Final model for research question 1
mi_modinc2 <- modindices(mode_inc2, sort. = TRUE)
mi_modinc2



# Nasic model for research question 2 - starting from including both moderation & mediation already
# due to similar structure

moderation_included_refdev <- '
    # direct effect
      Reference.Diversity ~ c*p_value
      Reference.Diversity ~ d*Rank
      Reference.Diversity ~ e*Special
      Reference.Diversity ~ g*Flesch.Reading.Ease
      Reference.Diversity ~ h*Time.Delta..months.
      Reference.Diversity ~ j*Confidence.Interval
      Reference.Diversity ~ k*Sample.size
      Reference.Diversity ~ l*Avg.sentence.length

      
      direct := c + d + e + g + h + j + k + l
  
    # regressions
      Reference.Diversity ~ b*cert_uncert
      cert_uncert ~ a*p_value
      
      deception_avg ~ x*p_value
      Reference.Diversity ~ y*deception_avg
      
      
    # Moderation
      cert_uncert ~ a1*Number.of.Authors
      cert_uncert ~ a2*pval_x_numbauth
      
      imm1 := a2*b
      
    # mean of centered write (for use in simple slopes)
      Number.of.Authors ~ number.mean*1

    # variance of centered write (for use in simple slopes)
      Number.of.Authors ~~ number.var*Number.of.Authors
      
    # indirect effects conditional on moderator 
      indirect.SDbelow := a*b + a2*-sqrt(number.var)*b
      indirect.mean := a*b + a2*number.mean*b
      indirect.SDabove := a*b + a2*sqrt(number.var)*b
      
    # Moderation2
      deception_avg ~ x1*Number.of.Authors
      deception_avg ~ x2*pval_x_numbauth
      
      imm2 := x2*y
      
    # indirect effects conditional on moderator 
      indirect.SDbelow2 := x*y + x2*-sqrt(number.var)*y
      indirect.mean2 := x*y + x2*number.mean*y
      indirect.SDabove2 := x*y + x2*sqrt(number.var)*y  

      
    # indirect effect (a*b)
      indirect_certainty := a*b
      indirect_deception := x*y
      indirect_total := (a*b) + (x*y)
  
    # total effect
      total := c + d + e + g + h + j + k + l + (a*b) + (x*y) + a1 + a2 + x1 + x2
'
# Model evaluation
mode_inc_refdev <- sem(model = moderation_included_refdev, data = finaldata.scale, fixed.x = FALSE, std.lv = FALSE)
summary(mode_inc_refdev, standardized = TRUE, fit.measures=TRUE)

mi_modinc_refdev <- modindices(mode_inc_refdev, sort. = TRUE)
mi_modinc_refdev

# Final model update based upon modification indices
moderation_included_refdev2 <- '
    # direct effect
      Reference.Diversity ~ c*p_value
      Reference.Diversity ~ d*Rank
      Reference.Diversity ~ e*Special
   #   Reference.Diversity ~ f*Word.count
      Reference.Diversity ~ g*Flesch.Reading.Ease
      Reference.Diversity ~ h*Time.Delta..months.
      Reference.Diversity ~ j*Confidence.Interval
      Reference.Diversity ~ k*Sample.size
      Reference.Diversity ~ l*Avg.sentence.length

      
      direct := c + d + e + g + h + j + k + l
  
    # regressions
      Reference.Diversity ~ b*cert_uncert
      cert_uncert ~ a*p_value
      
      deception_avg ~ x*p_value
      Reference.Diversity ~ y*deception_avg
      
      
    # Moderation
      cert_uncert ~ a1*Number.of.Authors
      cert_uncert ~ a2*pval_x_numbauth
      
      imm1 := a2*b
      
    # mean of centered write (for use in simple slopes)
      Number.of.Authors ~ number.mean*1

    # variance of centered write (for use in simple slopes)
      Number.of.Authors ~~ number.var*Number.of.Authors
      
    # indirect effects conditional on moderator 
    #  indirect.SDbelow := a*b + a2*-sqrt(number.var)*b
    #  indirect.mean := a*b + a2*number.mean*b
    #  indirect.SDabove := a*b + a2*sqrt(number.var)*b
      
    # Moderation2
      deception_avg ~ x1*Number.of.Authors
      deception_avg ~ x2*pval_x_numbauth
      
      imm2 := x2*y
      
    # indirect effects conditional on moderator 
    #  indirect.SDbelow2 := x*y + x2*-sqrt(number.var)*y
    #  indirect.mean2 := x*y + x2*number.mean*y
    #  indirect.SDabove2 := x*y + x2*sqrt(number.var)*y  
      
    # Modification
      deception_avg  ~ Avg.sentence.length
      
    # indirect effect (a*b)
      indirect_certainty := a*b
      indirect_deception := x*y
    #  indirect_total := (a*b) + (x*y)
  
    # total effect
    #  total := c + d + e + g + h + j + k + l + (a*b) + (x*y) + a1 + a2 + x1 + x2
'

# Final Model interpretation
mode_inc_refdev2 <- sem(model = moderation_included_refdev2, data = finaldata.scale, fixed.x = FALSE, std.lv = FALSE)
summary(mode_inc_refdev2, standardized = TRUE, fit.measures=TRUE)

# Final check on MI's of the second model
mode_inc_refdev2 <- modindices(mode_inc_refdev2, sort. = TRUE)
mode_inc_refdev2