library(GWmodel)
library(robustbase)
library(RColorBrewer)

load("/home/sahil/Desktop/DubVoter.rda")

names(Dub.voter)
pairs(Dub.voter)

data(Georgia)
# data(GeorgiaCounties)

m1 = lm('GenEl2004 ~ DiffAdd', data=Dub.voter)
m2 = lm('GenEl2004 ~ Unempl', data=Dub.voter) # lowest AIC of 2110
m3 = lm('GenEl2004 ~ LARent', data=Dub.voter)
m4 = lm('GenEl2004 ~ SC1', data=Dub.voter)
m5 = lm('GenEl2004 ~ LowEduc', data=Dub.voter)
m6 = lm('GenEl2004 ~ Age18_24', data=Dub.voter)
m7 = lm('GenEl2004 ~ Age25_44', data=Dub.voter)
m8 = lm('GenEl2004 ~ Age45_64', data=Dub.voter)
m9 = lm('GenEl2004 ~ log(DiffAdd) + I(log(DiffAdd)^2)', data=Dub.voter)
# model with top 2 variables
m10 = lm('GenEl2004 ~ Unempl + LARent', data=Dub.voter) # lowest AIC of 2052
AIC(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10) # find lowest AIC
# m3 and m2 are essentiall the same model, since AIC only differ by 2

# Global model
dubvoter_ols = lm('GenEl2004 ~ DiffAdd + Unempl + LARent + SC1 + LowEduc + Age18_24 + Age25_44 + Age45_64',
                  data=Dub.voter)
# The model with all variables indicated that Unemployment, LARent, Age18_24, Age25_44 
# were significant variables.
dubvoter_ols1 = lm('GenEl2004 ~ Unempl + LARent + Age25_44 + Age18_24',
                  data=Dub.voter)
vif(dubvoter_ols)
AIC(dubvoter_ols)# AIC is lower (1999)
AIC(dubvoter_ols1)# AIC is lower (1999)
summary(dubvoter_ols)
summary(dubvoter_ols1)
par(mfrow=c(2,1)) # Change the panel layout to 2 x 2
par(mfrow=c(1,1))
plot(dubvoter_ols)
# Unemp, LARent, Age18_24, Age25_44 are important predictores
# R^2 is 62%

anova(m10, dubvoter_ols) # anova confirms that we need the model with all variables

anova(dubvoter_ols, dubvoter_ols1) # it confirms that we 


# Geographicall weighted model

# Find optimal bandwitth with specific kernel
dubvoter_bw1= bw.gwr('GenEl2004 ~ DiffAdd + Unempl + LARent + SC1 + LowEduc + Age18_24 + Age25_44 + Age45_64',
                  data=Dub.voter, approach = "AIC", kernel="bisquare", adaptive = T)
# 109 is the adaptive kernel, AIC = 1921, so we choose this approach

dubvoter_bw2= bw.gwr('GenEl2004 ~ DiffAdd + Unempl + LARent + SC1 + LowEduc + Age18_24 + Age25_44 + Age45_64',
                     data=Dub.voter, approach = "AIC", kernel="gaussian", adaptive = T)
# 25 is the adaptive kernel, AIC = 1938

dubvoter_bw3= bw.gwr('GenEl2004 ~ DiffAdd + Unempl + LARent + SC1 + LowEduc + Age18_24 + Age25_44 + Age45_64',
                     data=Dub.voter, approach = "AIC", kernel="tricube", adaptive = T)
# 109 is the adaptive kernel, AIC = 1920

dubvoter_bw4= bw.gwr('GenEl2004 ~ DiffAdd + Unempl + LARent + SC1 + LowEduc + Age18_24 + Age25_44 + Age45_64',
                     data=Dub.voter, approach = "AIC", kernel="boxcar", adaptive = T)
# 56 is the adaptive kernel, AIC = 1920

dubvoter_bw5= bw.gwr('GenEl2004 ~ DiffAdd + Unempl + LARent + SC1 + LowEduc + Age18_24 + Age25_44 + Age45_64',
                     data=Dub.voter, approach = "AIC", kernel="exponential", adaptive = T)
# 24 is the adaptive kernel, AIC = 1943

# since these AIC of these 2 models differ by more than 2, we can choose the model with lowest AIC.

# Do geographically weighted regression, for boxcar and tricube
gwr1 = gwr.basic(formula = GenEl2004 ~ DiffAdd + Unempl + LARent + SC1 + LowEduc + Age18_24 + Age25_44 + Age45_64,
                     data=Dub.voter, bw = dubvoter_bw3, kernel="tricube", adaptive = T)
print(gwr1)
# Unemp, LARent, Age18_24, Age25_44 are important predictores
# AIC is 1999 which is lower than before
# AICC is 1999, so 
# all predictors have a sign change, atleast some of these coeff are effectively zero
# The model has increased the number of paramters from 8 to 79.90

gwr2 = gwr.basic(formula = GenEl2004 ~ DiffAdd + Unempl + LARent + SC1 + LowEduc + Age18_24 + Age25_44 + Age45_64,
                 data=Dub.voter, bw = dubvoter_bw4, kernel="boxcar", adaptive = T)
print(gwr2)

names(gwr1$SDF)
spplot(gwr1$SDF, "DiffAdd", main="GW DiffAdd Coeff Estimates") 
# As we can see, people in the center of Dublin have negative relationship with GenEl
# while people who live on the outskirts of dublin have positive relationship with GenEL
# Maybe it's because centre of city is expensive and people cannot afford the houses, so they move outside

spplot(gwr1$SDF, "LARent", main="GW LARent Coeff Estimates") 
# As we can see, people in Dublin have negative relationship with renting, so as the renting percentage
# in each ED increases, there is reduced voter turnout rate!

spplot(gwr1$SDF, "Age25_44", main="GW Age25_44 Coeff Estimates") 
# There is distinct contrast in south and north dublin. Southern part has positive relationship while
# northern part has negative relationship

spplot(gwr1$SDF, "Age18_24", main="GW LARent Coeff Estimates") 
# herer we find there is an overall -ve relationship with this age group of people, except in a few
# patches

spplot(gwr1$SDF, "Unempl", main="GW Unempl Coeff Estimates") 
# herer we find there is an overall -ve relationship with this age group of people, except in a few
# patches

spplot(gwr1$SDF, "LowEduc", main="GW LowEduc Coeff Estimates") 

print(gwr1$GW.diagnostic)
# AIC is 1826.147, but adjusted R^2 has increased to 75%

# now we deal with collinearity
# Find optimal bandwitth with specific kernel
collinear_bw1= bw.gwr.lcr('GenEl2004 ~ DiffAdd + Unempl + LARent + SC1 + LowEduc + Age18_24 + Age25_44 + Age45_64',
                     data=Dub.voter, kernel="tricube", adaptive = T, lambda.adjust = TRUE, cn.thresh = 30)


collinear_gwr1= gwr.lcr('GenEl2004 ~ DiffAdd + Unempl + LARent + SC1 + LowEduc + Age18_24 + Age25_44 + Age45_64',
                          data=Dub.voter, kernel="tricube", adaptive = T, lambda.adjust = TRUE, cn.thresh = 30,bw = collinear_bw1)

collinear_gwr1$GW.diagnostic
spplot(collinear_gwr1$SDF, "Local_CN", main=" Locally adjusted lambda CN thresh = 30")
plot(collinear_gwr1$SDF$Local_CN, collinear_gwr1$SDF$Local_Lambda)
spplot(collinear_gwr1$SDF, "Local_Lambda", main="Local Lambda number with adjustment")




## Qithout adjusting the lambda
collinear_bw3= bw.gwr.lcr('GenEl2004 ~ DiffAdd + Unempl + LARent + SC1 + LowEduc + Age18_24 + Age25_44 + Age45_64',
                          data=Dub.voter, kernel="tricube", adaptive = T, lambda.adjust = FALSE, cn.thresh = 30)


collinear_gwr3= gwr.lcr('GenEl2004 ~ DiffAdd + Unempl + LARent + SC1 + LowEduc + Age18_24 + Age25_44 + Age45_64',
                        data=Dub.voter, kernel="tricube", adaptive = T, lambda.adjust = FALSE, cn.thresh = 30,bw = collinear_bw2)
collinear_gwr3$GW.diagnostic
spplot(collinear_gwr3$SDF, "Local_CN", main=" Not Locally adjusted lambda CN thresh = 30")
plot(collinear_gwr3$SDF$Local_CN, collinear_gwr3$SDF$Local_Lambda)
spplot(collinear_gwr3$SDF, "Local_Lambda", main="Local Lambda number")

