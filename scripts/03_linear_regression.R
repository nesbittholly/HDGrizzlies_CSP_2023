library(tidyverse)
library(ggcorrplot) #ggcorrplot function
library(survey) #svydesign and svyglm functions
library(MASS) #stepAIC function

# reading in data and preparing dataframe for model
dat<-read_csv("data/processed/gbdat_CompositeDummyVars4.csv")
dat2<-dat[(dat$resident==1),]%>% #removes part time residents
    dplyr::select(belief_mt, satis,emotions, hunting_norm_beliefs, trust, proc_justice,
                  imposed, natural, knowl_gb, edu, age, tenure_new, attitude_likert,tenure_new3,
                  exp1_afar, exp3_closehome, exp4_damage, exp5_knowppldamage, exp6_fearsafety,
                  ag_profit, outdoor_ind, hunter, male, urban,
                  years_current_address, acres,
                  weight) #only include variables of interest
dat3<-na.omit(dat2) #removing non-response rows
dat3$weight_norm<-dat3$weight/(mean(dat3$weight)) #calculates the normalized weight, mean of weight_norm should be 1 and sum should be number of cases
dat3<-dat3%>%dplyr::select(-weight) #removing the non-normalized weight
dat3$log_years_current_address<-log(dat3$years_current_address) #transforming years at current address -- order of magnitude difference causing heteroscedasticity
dat3$log_acres<-log(dat3$acres+1) #transforming acres -- order of magnitude difference causing heteroscedasticity

# standardizing data
dat_z<-dat3%>%
    dplyr::select(belief_mt,
                  emotions, hunting_norm_beliefs, trust, proc_justice, 
                  imposed, natural,
                  log_years_current_address, log_acres, 
                  knowl_gb, edu, age, tenure_new, attitude_likert,
                  tenure_new3,
                  satis) #creating a dataframe of non-binary variables to be standardized 
dat_z<-as_tibble(corpcor::wt.scale(dat_z, dat3$weight_norm)) #standardizing weighted variables
binary_dat_weight<-dat3%>%dplyr::select(-!!names(dat_z)) #creating dataframe with all remaining variables
dat_z2<-as_tibble(cbind(dat_z, binary_dat_weight)) #combining standardized data with binary data and weights
summary(dat_z2)

# dataframe with candidate covariates
dat_z3<-dat_z2%>%dplyr::select(belief_mt, satis,emotions, hunting_norm_beliefs, trust, proc_justice, 
                               imposed, natural,
                               log_years_current_address, log_acres, 
                               knowl_gb, edu, age, tenure_new, attitude_likert,
                               tenure_new3, exp1_afar, exp3_closehome, exp4_damage, exp5_knowppldamage, exp6_fearsafety, ag_profit, outdoor_ind, hunter, male,
                               urban)

# correlation plots to see how correlated some of the covariates are
cor_matrix <- cor(dat_z3)#%>%dplyr::select(-c(proc_justice, log_acres, imposed)))
ggcorrplot(cor_matrix, hc.order = T, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("red", "white", "darkgreen"), 
           title="Correlation between covariates", 
           ggtheme=theme_bw)
#a few covariates are way too correlated to include both in the model
#I compared models with one of each variable to choose which variable of each pair to include (not shown here)
#proc_justice and trust r = 0.63 --> sign flips with proc_justice instead of trust in model, indicating model instability. drop proc_justice from consideration
#log_acres and ag_profit r = 0.61 --> model with log_acres has more error, so drop log_acres
#imposed and attitude_likert r = -0.69 --> attitude_likert is a stronger predictor of belief than imposed, so drop imposed
#outdoor industry has no variation, so remove from model

# specifying saturated model
## linear models using survey package to specify weighting from stratified sample
design_z<-svydesign(id=~1, weights=dat_z2$weight_norm, data=dat_z3) #specifies the weighting
regsvy_sat<-svyglm(belief_mt ~ 
                       attitude_likert+
                       emotions+
                       hunting_norm_beliefs+
                       natural+
                       knowl_gb+
                       edu+
                       age+
                       tenure_new+I(tenure_new^2)+
                       exp3_closehome+
                       exp4_damage+
                       exp5_knowppldamage+
                       ag_profit+
                       hunter+
                       male+
                       urban+
                       trust,
                   design=design_z)
AIC(regsvy_sat) #AIC = 385.9 --> since publication, this code doesn't appear to be working anymore, because of an R update?
jtools::summ(regsvy_sat, vifs=T)

# model selection
## backward selection
fit_reg_backward<-MASS::stepAIC(regsvy_sat, direction="backward")
jtools::summ(fit_reg_backward, digits=4, vifs=T)
AIC(fit_reg_backward)#377.4 --> since publication, this code doesn't appear to be working anymore, because of an R update?

## forward and iterative selection --> since publication, this code doesn't appear to be working anymore, because of an R update?
### null model required for forward and iterative selection 
regsvy0<-svyglm(belief_mt ~ 1, data=dat_z3, design=design_z)
AIC(regsvy0) #930.7

### forward selection, upper is saturated model, lower is null model
fit_reg_forward<-MASS::stepAIC(regsvy0, direction="forward", scope=list(upper=regsvy_sat, lower=regsvy0))
summ(fit_reg_forward)
AIC(fit_reg_forward) #377.2

### iterative selection, upper is saturated model, lower is null model
fit_reg_both<-MASS::stepAIC(regsvy0, direction="both", scope=list(upper=regsvy_sat, lower=regsvy0))
jtools::summ(fit_reg_both)
AIC(fit_reg_both) #377.2

## how do models compare?
### forward to null and saturated
anova(fit_reg_forward, regsvy0)#sig diff than null
anova(fit_reg_forward, regsvy_sat)#not sig diff from saturated

## backward to null and saturated
anova(fit_reg_backward, regsvy0)#sig diff than null
anova(fit_reg_backward, regsvy_sat)#not sig diff from saturated

# final model from forward and iterative model selection
design_z<-svydesign(id=~1, weights=dat_z2$weight_norm, data=dat_z2)
glm_svy<-svyglm(belief_mt ~ 
                    attitude_likert+
                    hunter+
                    hunting_norm_beliefs+
                    exp5_knowppldamage+
                    age+
                    natural+
                    emotions+
                    trust,
                design=design_z)
jtools::summ(glm_svy, digits=4, vifs=T)
AIC(glm_svy) #377.2 --> since publication, this code doesn't appear to be working anymore, because of an R update?

# model diagnostics
par(mfrow=c(2,3))
plot(glm_svy)#residuals vs fitted looks okay (patterns due to ordinal data), q-q plot is windy but normality is less important, leverage looks fine
svystdres(glm_svy, doplot=TRUE) #standardized residal plot looks good.
dev.off()

## normality
sf.test(glm_svy$resid) #significantly different from normal

svy_resid<-svystdres(glm_svy)
par(mfrow=c(2,4))
plot(dat_z2$attitude_likert,svy_resid$stdresids, col=rgb(red=.2, green=.2, blue=.2, alpha=0.5), pch=16) #some evidence of heteroscedasticity
plot(dat_z2$emotions,svy_resid$stdresids, col=rgb(red=.2, green=.2, blue=.2, alpha=0.5), pch=16)
plot(dat_z2$hunting_norm_beliefs,svy_resid$stdresids, col=rgb(red=.2, green=.2, blue=.2, alpha=0.5), pch=16)
plot(dat_z2$natural,svy_resid$stdresids, col=rgb(red=.2, green=.2, blue=.2, alpha=0.5), pch=16)
plot(dat_z2$age,svy_resid$stdresids, col=rgb(red=.2, green=.2, blue=.2, alpha=0.5), pch=16)
plot(dat_z2$exp5_knowppldamage,svy_resid$stdresids, col=rgb(red=.2, green=.2, blue=.2, alpha=0.5), pch=16)
plot(dat_z2$hunter,svy_resid$stdresids, col=rgb(red=.2, green=.2, blue=.2, alpha=0.5), pch=16)
plot(dat_z2$trust,svy_resid$stdresids, col=rgb(red=.2, green=.2, blue=.2, alpha=0.5), pch=16)

## Hosmer-Lemeshow GOF test --> sensitive to group number, not good for binary predictors
#https://stats.stackexchange.com/questions/186219/how-many-groups-to-use-in-hosmer-and-lemeshow-test
for (i in 4:15) {
    print(hoslem.test(dat_z2$belief_mt, fitted(glm_svy), g=i) $p.value)
} #0 sig values --> probably safe to say that there is minimal evidence of poor model fit

for (i in 4:15) {
    print(hoslem.test(dat_z2$belief_mt, fitted(fit_reg_backward), g=i) $p.value)
} #0 sig values --> probably safe to say that there is minimal evidence of poor model fit

for (i in 4:15) {
    print(hoslem.test(dat_z2$belief_mt, fitted(regsvy_sat), g=i) $p.value)
} #

# k-fold cross validation
cv.glm(dat_z2, glm_svy)#delta is the MSE which is 0.37
cv.glm(dat_z2, fit_reg_backward)#delta is the MSE which is 0.37
cv.glm(dat_z2, regsvy0) #MSE is 0.99
cv.glm(dat_z2, regsvy_sat) #MSE is 0.37

