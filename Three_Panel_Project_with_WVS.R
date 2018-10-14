############################################################################################
# Author: Benjamin S. Knight
# Created: October 13th, 2018
# Data: 
# Notes: Inspired by "Follow the Leader" by Gabriel S. Lenz (2012)
# Code for coefficietn plot borrowed from DJSparks https://gist.github.com/dsparks/4332698
############################################################################################

library(haven)
GSS_2006 <- read_dta("/Users/benjamin/Documents/Ben/GSS_Panel/GSS_panel06w123_R6 - stata.dta")
myvars <- c("year_1", "year_2", "year_3", 
            "id_1", "id_2", "id_3",
            "partyid_1", "partyid_2", "partyid_3", 
            "homosex_1", "homosex_2", "homosex_3")
GSS_2006 <- GSS_2006[myvars]
names(GSS_2006) <- c("Response_Year_2006", 
                    "Response_Year_2008", 
                    "Response_Year_2010",
                    "GSS_Respondent_ID_2006",
                    "GSS_Respondent_ID_2008",
                    "GSS_Respondent_ID_2010",
                    "Political_Party_Affiliation_2006",
                    "Political_Party_Affiliation_2008",
                    "Political_Party_Affiliation_2010",
                    "Attitudes_Towards_Homosexuality_2006",
                    "Attitudes_Towards_Homosexuality_2008",
                    "Attitudes_Towards_Homosexuality_2010")

# PARTYID
# "Generally speaking, do you usually think of yourself as a Republican, 
#  Democrat, Independent, or what?"
# "Strong Democrat": 0
# "Not very strong Democrat": 1
# "Independent, close to Democrat": 2
# "Independent (Neither, No response)": 3
# "Independent, close to Republican": 4
# "Not very strong Republican": 5
# "Strong Republican": 6


# HOMOSEX
# "What about sexual relations between two adults of the same sex--do you think it is always wrong, 
# almost always wrong, wrong only sometimes, or not wrong at all?"
# 
# "Always Wrong": 1
# "Almost Always Wrong": 2
# "Wrong Only Sometimes": 3
# "Not wrong at all": 4

model1 <- lm(formula = "Political_Party_Affiliation_2008~
                        Attitudes_Towards_Homosexuality_2008", data=GSS_2006)
summary(model1)
model2 <- lm(formula = "Political_Party_Affiliation_2010~
                        Attitudes_Towards_Homosexuality_2010", data=GSS_2006)
summary(model2)

model1Frame <- data.frame(Variable = rownames(summary(model1)$coef),
                          Coefficient = summary(model1)$coef[, 1],
                          SE = summary(model1)$coef[, 2],
                          Model = paste(" 2008 Postive Attitudes \n", 
                                            "Towards Homosexuality \n",
                                            "Vs. 2008 Republican \n",
                                            "Party Affiliation"))
model2Frame <- data.frame(Variable = rownames(summary(model2)$coef),
                          Coefficient = summary(model2)$coef[, 1],
                          SE = summary(model2)$coef[, 2],
                          Model = paste(" 2010 Postive Attitudes \n", 
                                            "Towards Homosexuality \n",
                                            "Vs. 2010 Republican \n",
                                            "Party Affiliation"))

# Combine these data.frames
allModelFrame <- data.frame(rbind(model1Frame, model2Frame))  # etc.
allModelFrame <- allModelFrame[-c(1,3), ]

# Specify the width of your confidence intervals
interval1 <- -qnorm((1-0.9)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier

# Plot
library('ggplot2')
zp1 <- ggplot(allModelFrame, aes(colour = Model))
zp1 <- zp1 + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)
zp1 <- zp1 + geom_linerange(aes(x = Variable, ymin = Coefficient - SE*interval1,
                                ymax = Coefficient + SE*interval1),
                            lwd = 1, position = position_dodge(width = 1/2))
zp1 <- zp1 + geom_pointrange(aes(x = Variable, y = Coefficient, ymin = Coefficient - SE*interval2,
                                 ymax = Coefficient + SE*interval2),
                             lwd = 1/2, position = position_dodge(width = 1/2),
                             shape = 21, fill = "WHITE")
zp1 <- zp1 + coord_flip() + theme_bw()
zp1 <- zp1 + ggtitle(paste("Predictive Power of Attitudes Towards Homosexuality \n",
                           "for Party Identification, 2008 and 2010"))
print(zp1)  # The trick to these is position_dodge().


#######################################################################################################

model3 <- lm(formula = "Political_Party_Affiliation_2008~
                        Attitudes_Towards_Homosexuality_2006", data=GSS_2006)
summary(model3)
model4 <- lm(formula = "Political_Party_Affiliation_2010~
             Attitudes_Towards_Homosexuality_2008", data=GSS_2006)
summary(model4)

model3Frame <- data.frame(Variable = rownames(summary(model3)$coef),
                          Coefficient = summary(model3)$coef[, 1],
                          SE = summary(model3)$coef[, 2],
                          Model = paste(" 2006 Postive Attitudes \n", 
                                        "Towards Homosexuality \n",
                                        "Vs. 2008 Republican \n",
                                        "Party Affiliation"))
model4Frame <- data.frame(Variable = rownames(summary(model4)$coef),
                          Coefficient = summary(model4)$coef[, 1],
                          SE = summary(model4)$coef[, 2],
                          Model = paste(" 2008 Postive Attitudes \n", 
                                        "Towards Homosexuality \n",
                                        "Vs. 2010 Republican \n",
                                        "Party Affiliation"))

# Combine these data.frames
allModelFrame2 <- data.frame(rbind(model3Frame, model4Frame))  # etc.
allModelFrame2 <- allModelFrame2[-c(1,3), ]

# Plot
library('ggplot2')
zp2 <- ggplot(allModelFrame2, aes(colour = Model))
zp2 <- zp2 + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)
zp2 <- zp2 + geom_linerange(aes(x = Variable, ymin = Coefficient - SE*interval1,
                                ymax = Coefficient + SE*interval1),
                            lwd = 1, position = position_dodge(width = 1/2))
zp2 <- zp2 + geom_pointrange(aes(x = Variable, y = Coefficient, ymin = Coefficient - SE*interval2,
                                 ymax = Coefficient + SE*interval2),
                             lwd = 1/2, position = position_dodge(width = 1/2),
                             shape = 21, fill = "WHITE")
zp2 <- zp2 + coord_flip() + theme_bw()
zp2 <- zp2 + ggtitle(paste("Predictive Power of PRIOR Attitudes Towards Homosexuality \n",
                           "for Party Identification, 2008 and 2010"))
print(zp2)  # The trick to these is position_dodge().

#######################################################################################################

#SCRAP CODE - DISREGARD
# 
# library(dotwhisker)
# library(broom)
# library(dplyr)
# 
# 
# a1 <- lm(formula = "partyid_1~homosex_1", data=GSS_2006)
# a2 <- lm(formula = "partyid_3~homosex_3", data=GSS_2006)
# # draw a dot-and-whisker plot
# dwplot(a1+1, conf.level = .95)
# 
# source("https://www.r-statistics.com/wp-content/uploads/2010/07/coefplot.r.txt")
# coefplot(a1, par=-1)
# coefplot(a2, par=-1)
# 
# m1 <- lm("partyid_2~homosex_1", data=GSS_2006)
# m2 <- lm("partyid_3~homosex_2", data=GSS_2006)
# # draw a dot-and-whisker plot
# dwplot(m1, vline = geom_vline(xintercept = 0, colour = "grey50", linetype = 2)) +
#   xlab("Coefficient")

