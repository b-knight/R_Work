library(car)
library(dplyr)
library(olsrr)
library(MASS)
load("/home/bknight/Documents/Academics/R_Work/WV6_Data_R_v_2016_01_01.rdata")
# V9: Important in life: Religion	
# For each of the following, indicate how important it is in your life. 
# Would you say it is:  Religion		
                                      # 1##Very important 
                                      # 2##Rather important 
                                      # 3##Not very important 
                                      # 4##Not at all important 
                                      # -5##BH: Missing; 
                                      # RU:Inappropriate response{Inappropriate} 
                                      # -4##Not asked in survey 
                                      # -3##Not applicable 
                                      # -2##No answer 
                                      # -1##Don't know 

#  V79	Schwartz: Tradition is important to this person; to follow the customs handed 
# down by one’s religion or family	Now I will briefly describe some people. Using this card, 
# would you please indicate for each description whether that person is very much like you, 
# like you, somewhat like you, not like you, or not at all like you?:  "Tradition is 
# important to this person; to follow the customs handed down by one’s religion or family"			
                                      # 1##Very much like me 
                                      # 2##Like me 
                                      # 3##Somewhat like me 
                                      # 4##A little like me 
                                      # 5##Not like me 
                                      # 6##Not at all like me 
                                      # 5##DE,SE:Inapplicable ; 
                                      # RU:Inappropriate response; 
                                      # Missing{Inappropriate} 
                                      # -4##Not asked in survey 
                                      # -3##Not applicable 
                                      # -2##No answer 
                                      # -1##Don´t know 


# V106	How much you trust: People of another religion	
# I ‘d like to ask you how much you trust people from various groups. 
# Could you tell me for each whether you trust people from this group completely, 
# somewhat, not very much or not at all? :  People of another religion	
                                            # 1##Trust completely 
                                            # 2##Trust somewhat 
                                            # 3##Do not trust very much 
                                            # 4##Do not trust at all 
                                            # -5##DE,SE:Inapplicable ; 
                                            # RU:Inappropriate response; Missing{Inappropriate} 
                                            # -4##Not asked in survey 
                                            # -3##Not applicable 
                                            # -2##No answer 
                                            # -1##Don´t know 

# V150	Meaning of religion: To follow religious norms and ceremonies 
# vs To do good to other people	With which one of the following statements do you agree most? 
# The basic meaning of religion is: "To follow religious norms  and ceremonies" or 
# "To do good to other people"
                                # 1##Follow religious norms and ceremonies 
                                # 2##Do good to other people 
                                # 3##Neither of them, other (SI,IN) 
                                # 4##Both (SI,IN) 
                                # -5##DE,SE:Inapplicable ; 
                                # RU:Inappropriate response; 
                                # BH: Missing{Inappropriate} 
                                # -4##Not asked in survey 
                                # -3##Not applicable 
                                # -2##No answer 
                                # -1##Don´t know 
# V248	Highest educational level attained	
# What is the highest educational level that you have attained?
                                  # 1##No formal education 
                                  # 2##Incomplete primary school 
                                  # 3##Complete primary school 
                                  # 4##Incomplete secondary school: technical/ vocational type 
                                  # 5##Complete secondary school: technical/ vocational type 
                                  # 6##Incomplete secondary school: university-preparatory type 
                                  # 7##Complete secondary school: university-preparatory type 
                                  # 8##Some university-level education, without degree 
                                  # 9##University - level education, with degree 
                                  # -5##AU: Inapplicable (No-school education) DE,
                                  # SE:Inapplicable ; SG: Refused; ZA:Other; Missing{Inappropriate} 
                                  # -4##Not asked 
                                  # -3##Not applicable 
                                  # -2##No answer 
                                  # -1##Don´t know 

# V156	People who belong to different religions are probably just as moral as those who 
# belong to mine	Please tell us if you strongly agree, agree, disagree, or strongly disagree 
# with the following statements: People who belong to different religions are probably just as 
# moral as those who belong to mine	3	
                                 # 1##Strongly agree 
                                 # 2##Agree 
                                 # 3##Disagree 
                                 # 4##Strongly disagree 
                                 # -5##DE,
                                 # SE:Inapplicable ; 
                                 # BH: Missing{Inappropriate} 
                                 # -4##Not asked in survey 
                                 # -3##Not applicable 
                                 # -2##No answer 
                                 # -1##Don´t know 


#######################################################################
# Data Prep
#######################################################################

df <- select(WV6_Data_R, c(V9, V79, V106, V150, V156, V248)) 
valid_values <- c(1,2,3,4)
df <- subset(df, V9 %in% valid_values)
df <- subset(df, V79 %in% c(1,2,3,4,5,6))
df <- subset(df, V106 %in% valid_values)
df <- subset(df, V150 %in% valid_values)
df <- subset(df, V156 %in% valid_values)
df <- subset(df, V248 %in% c(1,2,3,4,5,6,7,8,9))
names(df) <- c("Y", "X_1", "X_2", "X_3", "X_4", "X_5")

#######################################################################
# Factor Variables
#######################################################################

# Categorical Decomposition
df$X_1_lvl_1 <- ifelse(df$X_1 == 1, 1, 0) 
df$X_1_lvl_2 <- ifelse(df$X_1 == 2, 1, 0) 
df$X_1_lvl_3 <- ifelse(df$X_1 == 3, 1, 0) 
df$X_1_lvl_4 <- ifelse(df$X_1 == 4, 1, 0) 

model1 <- lm("Y ~ X_1_lvl_2 + X_1_lvl_3 + X_1_lvl_4 +X_2 + X_3 + X_4 +  X_5", data=df)
summary(model1)

ols_vif_tol(model1)
vif(model1)

df$X_1_factor <- factor(df$X_1)
model2 <- lm("X_4 ~ Y + X_1 + X_2 + X_3", data=df)
summary(model2)
vif(model2)

#######################################################################
# Mean Centering
#######################################################################
center_scale <- function(x) {scale(x, scale = FALSE)}


model7 <- lm("Y ~ X_1 + X_2 + X_3 + X_4 + X_5", data=df)
summary(model7)
ols_vif_tol(model7)
vif(model7)


model8 <- lm("Y ~ X1_centered + X2_centered + X3_centered + X4_centered + X5_centered", data=df)
summary(model8)
ols_vif_tol(model8)
vif(model8)

#######################################################################
# Ridge Regression
#######################################################################
library(MASS)
coef(lm.ridge("Y ~ X_1_factor + X_2 + X_3", data=df))

#######################################################################
# Polynomial Variables
#######################################################################

df$X_2_squared <- df$X_2^2
df$X_2_cubed <- df$X_2^3
model3 <- lm("Y ~ X_1 + X_2 + I(X_2^2) + I(X_2^3) + X_3", data=df)
model4 <- lm("Y ~ X_1 +X_2 + X_2_squared + X_2_cubed+ X_3", data=df)
summary(model3)
summary(model4)
vif(model3)
vif(model4)


########################################################################
# Interaction Terms and Mean Centering
########################################################################
df$X_4 <- df$X_2*df$X_3
model5 <- lm("Y ~ X_1 + X_2 + X_3 + I(X_2*X_3)", data=df)
model6 <- lm("Y ~ X_1 + X_2 + X_3 + X_4", data=df)
summary(model6)
vif(model5)
ols_vif_tol(model6)


model5 <- lm("Y ~ X_1 + X_2 + X_3 + I(X_2*X_3)", data=df)
summary(model5)
ols_vif_tol(model5)

center_scale <- function(x) {
  scale(x, scale = FALSE)
}

df$X1_centered <- center_scale(df$X_1)
df$X2_centered <- center_scale(df$X_2)
df$X3_centered <- center_scale(df$X_3)
model7 <- lm("Y ~ X1_centered + X2_centered + X3_centered + I(X2_centered*X3_centered)", data=df)
summary(model7)
ols_vif_tol(model7)
########################################################################
# Orthogonal Polynomial 
########################################################################

m2 <- lm(dist ~ poly(speed, 3, raw = FALSE), data = cars)
summary(m2)
ols_vif_tol(m2)

m2 <- lm(dist ~ poly(speed, 3), data = cars)
summary(m2)

m3 <- lm(dist ~ poly(speed, 1, raw = TRUE), data = cars)
summary(m3)
data(mtcars)

meeting_invites <- mtcars$wt
asana_tasks <- -mtcars$mpg +39

model <- lm(asana_tasks~meeting_invites, data = mtcars)
summary(model)

addNoise <- function(mtx) {
  if (!is.matrix(mtx)) mtx <- matrix(mtx, byrow = TRUE, nrow = 1)
  random.stuff <- matrix(runif(prod(dim(mtx)), min = -0.00001, max = 0.0001), nrow = dim(mtx)[1])
  random.stuff + mtx
}


mtcars$emails <- as.numeric(addNoise(meeting_invites))

model2 <- lm(asana_tasks ~ meeting_invites + emails, data = mtcars)
summary(model2)

