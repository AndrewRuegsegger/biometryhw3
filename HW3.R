library(dplyr)
library(ggthemes)
library(AICcmodavg)

file.choose()
fish <- read.csv("C:\\Users\\Andrew\\OneDrive\\Documents\\BIOL 57774\\Homework\\seasons rich all den hab 02 biometry 22.csv")
View(fish)
str(fish)

# Scale covariate means around 0 w/ scale().
fish <- fish %>%
  mutate(scTemp = c(scale(Temp)),
         scDO = c(scale(DO)),
         scSPC = c(scale(SPC)),
         scCover = c(scale(Cover)),
         scCanopy = c(scale(Canopy)),
         scDepth = c(scale(Depth)),
         scVelocity = c(scale(Velocity)),
         scSubstrate = c(scale(Substrate)))

#########################
### MODEL ASSUMPTIONS ###
#########################

model <- lm(Richness ~ scTemp + 
              scDO +
              scSPC + 
              scCover + 
              scCanopy + 
              scDepth + 
              scVelocity +
              scSubstrate,
            data=fish,
            na.action = "na.fail")

autoplot(model, which=1:6, ncol=3, label.size=3)

##########################
### Checking linearity ###
##########################
ggplot(data=fish, aes(y=Richness, x=scTemp))+
  geom_point()+
  geom_smooth(method="lm", se = TRUE)+
  theme_few() # Maybe slight positive linear 
ggplot(data=fish, aes(y=Richness, x=scDO))+
  geom_point()+
  geom_smooth(method="lm", se = TRUE)+
  theme_few() # Maybe slight negative linear
ggplot(data=fish, aes(y=Richness, x=scSPC))+
  geom_point()+
  geom_smooth(method="lm", se = TRUE)+
  theme_few() # Maybe positively linear
ggplot(data=fish, aes(y=Richness, x=scCover))+
  geom_point()+
  geom_smooth(method="lm", se = TRUE)+
  theme_few() # Definitely not linear
ggplot(data=fish, aes(y=Richness, x=scCanopy))+
  geom_point()+
  geom_smooth(method="lm", se = TRUE)+
  theme_few() # Maybe slight negative linear
ggplot(data=fish, aes(y=Richness, x=scDepth))+
  geom_point()+
  geom_smooth(method="lm", se = TRUE)+
  theme_few() # Maybe slight negative linear 
ggplot(data=fish, aes(y=Richness, x=scVelocity))+
  geom_point()+
  geom_smooth(method="lm", se = TRUE)+
  theme_few() # Maybe slight negative linear
ggplot(data=fish, aes(y=Richness, x=scSubstrate))+
  geom_point()+
  geom_smooth(method="lm", se = TRUE)+
  theme_few() # Definitely not linear

# No scatterplots show strong signs of linearity, and all show no or dubious signs.

#############################
### Checking independence ###
#############################

durbinWatsonTest(model)
# Durbin-Watson statistic = 1.874 = little to no autocorrelation = residuals are independent.

#################################
### Checking homoscedasticity ###
###     and normality         ###
#################################

autoplot(model, which=1:6, ncol=3, label.size=3)
# Residual plot shows some fanning and uneven number of observations toward extreme values
# = not super homoscedastic.
# Observations do not follow qqplot line super well = not super normal. 

#############################
### Checking collinearity ###
#############################

cor(fish[,c("scTemp","scDO","scSPC","scCover","scCanopy","scDepth","scVelocity","scSubstrate")])
# Several predictors are correlated.

vif <- vif(model)
vif
tol <- 1/(vif)
tol
# Though tolerances are quite high for predictors, suggesting that collinearity
# is probably not a huge issue. 

#######################
### MODEL SELECTION ###
#######################

### While I don't really know anything about fish or these fish in particular,
### I'll define some candidate models with vague hypotheses in mind so that I
### can do some model averaging and hopefully increase our predictive power.

model1a <- lm(Richness ~ scTemp + scDO + scSPC + scCover + scCanopy + scDepth + scVelocity + scSubstrate, data=fish) # global model
model2a <- lm(Richness ~ scTemp + scDO + scSPC, data=fish) # predictors that directly affect the water
model3a <- lm(Richness ~ scCanopy + scDO + scTemp, data=fish) # light availability affects temperature and algae abundance, which affects DO, which affects fish survival
model4a <- lm(Richness ~ scDepth + scCover + scSubstrate + scVelocity, data=fish) # habitat properties that I imagine would affect the kind of fish you would find living in an area
model5a <- lm(Richness ~ scDepth + scCover + scSubstrate, data=fish) # like model3a but maybe velocity doesn't matter much
model6a <- lm(Richness ~ scCover + scCanopy + scDepth, data=fish) # maybe the ability to hide equals more fish species
model7a <- lm(Richness ~ scSubstrate + scVelocity, data=fish) # high velocity + small particle size = lots of suspended sediment = bad for fish.
summary(model2a)
summary(model1a)
summary(model3a)
summary(model6a)
summary(model4a)
summary(model7a)
summary(model5a)

AIC(model2a)
AIC(model1a)
AIC(model3a)
AIC(model6a)
AIC(model4a)
AIC(model7a)
AIC(model5a)
# Comparing AICc values for candidate models
# threw in model6 from stepwise selection post-hoc to make
# things simple
cand.mods <- list(model1a, model2a, model3a, model4a, model5a, model6a, model7a, model6) 

modnames <- c("model1a",
              "model2a",
              "model3a",
              "model4a",
              "model5a",
              "model6a",
              "model7a",
              "model6")

aic.table <- aictab(cand.set = cand.mods, modnames=modnames) 
aic.table
# model2a is the top model. The next top model (model1a) has an AIC value 7.64 higher than model2a, which is too high to warrant model averaging. My cut off is a deltaAIC of 2.

# Because I did not get two models that I feel comfortable averaging from my [candidate] model selection,
# I will perform stepwise selection to see if there is another comparable model within a
# deltaAIC of 2 of the top model from my candidate models so that I may average these models
# and increase our predictive power.

### Automatic selection ###

both <- stepAIC(model, direction="both")
back <- stepAIC(model, direction="backward")
forward <- stepAIC(model, direction="forward")
# Both and backwards methods = scTemp + scSPC + scDepth is the 'best' model.

?stepAIC

### Manual selection just to double check work ###
model <- lm(Richness ~ scTemp + 
              scDO +
              scSPC + 
              scCover + 
              scCanopy + 
              scDepth + 
              scVelocity +
              scSubstrate,
            data=fish)
summary(model) # scCover has the highest p-value, so am removing that.
AIC(model)

model2 <- lm(Richness ~ scTemp + 
              scDO +
              scSPC +
              scCanopy + 
              scDepth + 
              scVelocity +
              scSubstrate,
            data=fish)
summary(model2)
AIC(model2) # AIC went slightly down and scDO has the highest p-value, so removing that now.

model3 <- lm(Richness ~ scTemp +
              scSPC + 
              scCanopy + 
              scDepth + 
              scVelocity +
              scSubstrate,
            data=fish)
summary(model3)
AIC(model3) # AIC went slightly down and scVelocity has the highest p-value, so removing that now.

model4 <- lm(Richness ~ scTemp +
               scSPC + 
               scCanopy + 
               scDepth + 
               scSubstrate,
             data=fish)
summary(model4)
AIC(model4) # AIC went slightly down and scCanopy has the highest p-value, so removing that now.

model5 <- lm(Richness ~ scTemp +
               scSPC + 
               scDepth + 
               scSubstrate,
             data=fish)
summary(model5)
AIC(model5) # AIC went slightly down and scSubstrate has the highest p-value, so removing that now.

model6 <- lm(Richness ~ scTemp +
               scSPC + 
               scDepth,
             data=fish)
summary(model6)
AIC(model6) # All p-values < 0.15, so I will stop removing predictors. 
# This selection confirms what was found with automatic selection.

summary(model2a)
summary(model6)
AIC(model2a)
AIC(model6)

# Model fit is still not amazing, but it's what we've got.
autoplot(model2a, which = 1:2)
autoplot(model6, which = 1:2)

# Both of these models have pretty comparable AIC's (351.38 and 349.81)
# and adj  r-squared's (0.25 and 0.27), making them pretty comparable.
# I'm going to perform model averaging taking the top model from the stepwise approach
# and the top model from my candidate models, since the deltaAIC b/t these models is 1.57 (<2.00).
# I don't think this is the traditional way to go about things, but I don't see any major issues
# with doing this if it means we can increase our predictive power while keeping our model
# at least somewhat informed by what I can infer about the ecology.

####################################
### MODEL AVERAGING AND GRAPHING ###
####################################
# The model averaging stuff seemed overly complex in the lab, so I used model.avg from MuMIn
# and ggpredict from ggeffects instead, which produces marginal means and adjusted predicted values
# for the response. 
?model.avg
?ggpredict

library(MuMIn)

models <- list(model2a, model6)
avg_model <- model.avg(models)
summary(avg_model)

library(ggeffects)

temp_mean <- mean(fish$Temp)
temp_sd <- sd(fish$Temp)
plot_temp <- ggpredict(avg_model, terms = "scTemp [all]")
plot_temp$x <- (plot_temp$x * temp_sd) + temp_mean

SPC_mean <- mean(fish$SPC)
SPC_sd <- sd(fish$SPC)
plot_SPC <- ggpredict(avg_model, terms = "scSPC [all]")
plot_SPC$x <- (plot_SPC$x * SPC_sd) + SPC_mean

depth_mean <- mean(fish$Depth)
depth_sd   <- sd(fish$Depth)
plot_depth <- ggpredict(avg_model, terms = "scDepth [all]")
plot_depth$x <- (plot_depth$x * depth_sd) + depth_mean

DO_mean <- mean(fish$DO)
DO_sd <- sd(fish$DO)
plot_DO <- ggpredict(avg_model, terms = "scDO [all]")
plot_DO$x <- (plot_DO$x * DO_sd) + DO_mean

######################
### VISUALIZATION ####
######################

p1 <- ggplot(plot_temp, aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "grey80", alpha = 0.5) +
  geom_line(color = "red", size = 1) +
  labs(
    x="Temperature",
    y=""
  )+
  theme_few()
p2 <- ggplot(plot_SPC, aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "grey80", alpha = 0.5) +
  geom_line(color = "red", size = 1) +
  labs(
    x="SPC",
    y=""
  )+
  theme_few()
p3 <- ggplot(plot_depth, aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "grey80", alpha = 0.5) +
  geom_line(color = "red", size = 1) +
  labs(
    x="Depth",
    y=""
  )+
  theme_few()
p4 <- ggplot(plot_DO, aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "grey80", alpha = 0.5) +
  geom_line(color = "red", size = 1) +
  labs(
    x="DO",
    y=""
  )+
  theme_few()

library(cowplot)
plot_grid(p1, p2, p3, p4, 
          labels = c("A", "B", "C", "D"),
          ncol = 2)
library(grid)
plot_grid_no_y <- plot_grid(p1, p2, p3, p4, 
                            labels = c("A", "B", "C", "D"), 
                            ncol = 2)
y_label <- textGrob("Predicted fish species richness", 
                    rot = 90, 
                    gp = gpar(fontface = "bold", fontsize = 14))
final_plot <- plot_grid(y_label, plot_grid_no_y, 
                        ncol = 2, 
                        rel_widths = c(0.09, 1))
final_plot

