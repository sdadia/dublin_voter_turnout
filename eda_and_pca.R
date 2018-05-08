data = read.csv("/home/sahil/Documents/maynooth_studies/Voting_data_project/votingdata.csv")
head(data)

data$log_DiffAdd = log(data$DiffAdd)

fit = lm('GenEl2004~DiffAdd', data=data)
summary(fit)
plot(fit)


fit2 = lm('GenEl2004~log_DiffAdd + DiffAdd', data=data)
summary(fit2)
plot(fit2)

anova(fit, fit2) # here we conclude we need the log of diffAddress by anova - but res v/s fit plots look problematic

fit3 = lm('GenEl2004~ log_DiffAdd', data=data)
summary(fit3)
plot(fit3)

fit4 = lm('GenEl2004 ~ log_DiffAdd + I(log_DiffAdd^2)', data=data)
summary(fit4)
plot(fit4)

fit5 = lm('GenEl2004~ log_DiffAdd + I(log_DiflifAdd^2) + I(log_DiffAdd^3)', data=data)
summary(fit5)
plot(fit5)

anova(fit4, fit5) # anova confirms we do not need the 3rd power term, so we check the diagnostics for model 4

# examining collinearity
fit = lm('GenEl2004 ~ Age25_44 + Age45_64', data=data)
summary(fit)

library(car)
vif(fit) # VIF score over 10 is a problem


# df <- subset(data, select = -c(X, Y, GenEl2004, DED_ID))
df <- subset(data, select = c(Age18_24, Age25_44, LARent, Age45_64, DiffAdd, Unempl, SC1))
df = scale(df)
pca1 = prcomp(df)
pca1
plot(pca1)
pca1$sdev^2 / sum(pca1$sdev^2)
biplot(pca1, scale=1)
biplot(pca1, scale=1, xlabs=rep(".", nrow(data)), cex=c(3,1))
summary(pca1)
pca1$lo
# 90% of variance is explained by first 4 pc
# PCA1 contrasts Age45_64 and SC1 with other variables, indicates, Age45_64 has a +ve relationship
# PCA2 contrasts LArent and Unempl with other variables
# PCA2 contrasts LArent and Unempl with other variables

screeplot <- function(p) {
  e <- p$sdev ^ 2
  e <- e / sum(e)
  plot(
    1:length(e),
    e,
    xlab = "Component number",
    pch = 20,
    ylab = "Variance proportion",
    main = "Explained Variance Proportion",
    axes = F,
    ylim = c(0, max(e)*1.04)
  )
  lines(1:length(e), e)
  axis(1, at = 1:length(e))
  axis(2)
}

screeplot(pca1)


require(GWmodel)
pc_temp = princomp(df, cor=F, scores = T)
pc_temp$loadings
names(pc_temp)

require(MASS)

lm_ridge = lm.ridge('GenEl2004 ~ DiffAdd + Unempl + LARent + SC1 + LowEduc + Age18_24 + Age25_44 + Age45_64', data=data)
names(lm_ridge)
summary(lm_ridge)
