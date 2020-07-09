#####Being yourself in adolescence####

##load data
library(haven)
false_self_data <- read_sav("false_self_data.sav")
View(false_self_data)


#load packages

library(psych)
library(car)
library(rockchalk)
library(apaTables)
library(lm.beta)

#descriptives

data_describe <- subset (false_self_data, select = c(age, false_self, depression, 
                                         anxiety, self_esteem))
describe (data_describe)

table(false_self_data$gender) # 0 = males; 1 = females

#transforme gender for ploting slopes

false_self_data$gender <- as.numeric(as.character(false_self_data$gender))

##generate descriptive table

data_describe_cor <- data_describe[,2:5]

apa.cor.table(data_describe_cor, filename="Table1.doc", table.number=1)

#assumptions for regression

##target regression: depression ~ self_esteem*false_self + gender + age

##linearity

###depression
summary(lm(depression ~ self_esteem, data = false_self_data))
summary(lm(depression ~ self_esteem, data = false_self_data))$r.squared
summary(lm(depression ~ false_self, data = false_self_data))
summary(lm(depression ~ false_self, data = false_self_data))$r.squared

##target regression: anxiety ~ self_esteem*false_self + gender + age

##linearity

###anxiety
summary(lm(anxiety ~ self_esteem, data = false_self_data))
summary(lm(anxiety ~ self_esteem, data = false_self_data))$r.squared
summary(lm(anxiety ~ false_self, data = false_self_data))
summary(lm(anxiety ~ false_self, data = false_self_data))$r.squared

##colinearity for Block 1 for all hierarchical models

cor(false_self_data$self_esteem, false_self_data$false_self)
cor(false_self_data$depression, false_self_data$false_self)
cor(false_self_data$anxiety, false_self_data$false_self)

##target regressions: false_self ~ depression*self_esteem + anxiety*self_esteem + gender; depression ~ false_self*anxiety + false_self*self_esteem + gender; anxiety ~ flase_self*sel_esteem + false_self*depression

###false self
summary(lm(false_self ~ self_esteem, data = false_self_data))
summary(lm(false_self ~ self_esteem, data = false_self_data))$r.squared
summary(lm(false_self ~ depression, data = false_self_data))
summary(lm(false_self ~ depression, data = false_self_data))$r.squared
summary(lm(false_self ~ anxiety, data = false_self_data))
summary(lm(false_self ~ anxiety, data = false_self_data))$r.squared

###depression
summary(lm(depression ~ self_esteem, data = false_self_data))
summary(lm(depression ~ self_esteem, data = false_self_data))$r.squared
summary(lm(depression ~ false_self, data = false_self_data))
summary(lm(depression ~ false_self, data = false_self_data))$r.squared
summary(lm(depression ~ anxiety, data = false_self_data))
summary(lm(depression ~ anxiety, data = false_self_data))$r.squared

###depression
summary(lm(anxiety ~ self_esteem, data = false_self_data))
summary(lm(anxiety ~ self_esteem, data = false_self_data))$r.squared
summary(lm(anxiety ~ false_self, data = false_self_data))
summary(lm(anxiety ~ false_self, data = false_self_data))$r.squared
summary(lm(anxiety ~ depression, data = false_self_data))
summary(lm(anxiety ~ depression, data = false_self_data))$r.squared

##multicolinearity

car::vif(lm(false_self ~ self_esteem + depression + anxiety, data = false_self_data))
car::vif(lm(depression ~ false_self + self_esteem + anxiety, data = false_self_data))
car::vif(lm(anxiety ~ false_self + depression + self_esteem, data = false_self_data))
car::vif(lm(self_esteem ~ false_self + depression + anxiety, data = false_self_data))

###################################################
### hierarchical linear regression - depression ###
###################################################

block1d <- lm(depression ~ false_self, data = false_self_data)
summary(block1d)
summary(block1d_beta <- lm.beta(block1d))

block2d <- update(block1d, . ~ . + gender + self_esteem)
summary(block2d)
summary(block2d_beta <- lm.beta(block2d))

block3d <- update(block2d, . ~ . + false_self:self_esteem)
                   
summary(block3d)

summary(block3d_beta <- lm.beta(block3d))

anova(block1d,block2d,block3d)

###################################################
### hierarchical linear regression - anxiety ###
###################################################

block1a <- lm(anxiety ~ false_self, data = false_self_data)
summary(block1a)
summary(block1a_beta <- lm.beta(block1a))

block2a <- update(block1a, . ~ . + gender + self_esteem)
summary(block2a)
summary(block2a_beta <- lm.beta(block2a))

block3a <- update(block2a, . ~ . + false_self:self_esteem)
summary(block3a)
summary(block3a_beta <- lm.beta(block3a))

anova(block1a,block2a,block3a)

###################################################
### hierarchical linear regression - false_self ###
###################################################

block1f <- lm(false_self ~ self_esteem, data = false_self_data)
summary(block1f)
summary(block1f_beta <- lm.beta(block1f))

block2f <- update(block1f, . ~ . + gender + depression + anxiety)
summary(block2f)
summary(block2f_beta <- lm.beta(block2f))

block3f <- update(block2f, . ~ . + depression:self_esteem + anxiety:self_esteem)
summary(block3f)
summary(block3f_beta <- lm.beta(block3f))

anova(block1f,block2f,block3f)

#moderation: anxiety*self_esteem

plotCurves(block3f, plotx="self_esteem", modx="anxiety", 
           modxVals="std.dev.",
           col = c("blue", "black", "orange"),
           interval="confidence", 
           main = "Moderation effect of anxiety on the relationship of false self and self-esteem")


