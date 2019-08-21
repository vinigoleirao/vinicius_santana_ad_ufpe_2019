#############################################################################################
#############################################################################################
############################ DATA ANALYSIS ##################################################
############################ FINAL ACTIVITY #################################################
#############################################################################################
########################  VINICIUS SILVA SANTANA ############################################
#############################################################################################
#############################################################################################



# COMMENT: REMEMBER TO ADD HEADS AND CLEAR NAMES TO VARIABLES IN PLOTS

############################################################################################
############################ LOADING PACKAGES AND DATAFRAME ################################
############################################################################################

getwd()
setwd("./Curso R/QOG Data")

require(tidyverse)
require(ggplot2)

pov.aid <- read.csv("./vinicius-santana-ad-ufpe-2019.csv")

view(pov.aid)
names(pov.aid)
summary(pov.aid)

#########################################################################################
################################ EXPLORATORY DATA ANALYSIS ##############################
#########################################################################################

# how many countries for each of the relevant areas of study
ggplot(data = pov.aid) +
  geom_bar(mapping = aes(x = cregion))

# checking the distribution of aid_capta within cregion 
ggplot(data = pov.aid, mapping = aes(x = cregion, y = aid_capta)) +
  geom_boxplot() + coord_flip()

# checking the distribution of aid_total within regions
ggplot(data = pov.aid, mapping = aes(x = cregion, y = aid_total)) +
  geom_boxplot() + coord_flip()

# distribution of pov_rate within regions
ggplot(data = pov.aid, mapping = aes(x = cregion, y = pov_rate)) +
  geom_boxplot() + coord_flip()

# HIGHEST POVERTY RATES ARE ENCOUNTERED IN ALL THREE AFRICAN REGIONS

ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = median_inc, y = aid_crsio))

ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = median_inc, y = aid_crsc))

ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = median_inc, y = aid_total))
# IT DOESNT SEEM THAT MEDIAN INCOME MATTERS MUCH FOR THE SIZE OF AID

ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = median_inc, y = aid_capta))

#########

ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = gdp_size, y = aid_total))
# BUT IT DOES SEEM THAT THE HIGHER THE GDP THE HIGHER THE AID TOTAL
# checking it by gdp_capta
ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = gpd_capta, y = aid_total)) # not very conclusive

#who gives more based on gdp size? IO or other donors?
ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = gdp_size, y = aid_crsio)) #international organizations

ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = gdp_size, y = aid_crsc))

# does the same apply to gdp capta?
ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = gpd_capta, y = aid_crsio))

ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = gpd_capta, y = aid_crsc)) # inconclusive

# what about poverty rate?
ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = pov_rate, y = aid_total))
# data suggests that countries that receive low aid_total tend to have their aid slightly
# increased as poverty rates are higher. High aid total was only given to countries with less
# than 40% pov rate

ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = pov_rate, y = aid_crsio)) 
# it seems the higher the poverty rate, the lower the aid donated by intl org

ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = gpd_capta, y = aid_crsc)) # same applies for other donors


# what about gdp growth?
ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = gdp_growth, y = aid_total))

ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = gdp_growth, y = aid_crsio))

ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = gdp_growth, y = aid_poor))

ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = gdp_growth, y = aid_crsc))
# IT SEEMS AS GDP GROWTH IS HIGHER, THE AID ENDS TO INCREASE

# what about regions?
ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = cregion, y = aid_total)) + coord_flip()

ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = cregion, y = aid_crsio)) + coord_flip()

ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = cregion, y = aid_crsc)) + coord_flip()

ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = cregion, y = aid_capta)) + coord_flip()

ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = cregion, y = aid_poor)) + coord_flip() # not use this




ggplot(data = pov.aid) +
  geom_hex(mapping = aes(x = gpd_capta, y = aid_total))

ggplot(data = pov.aid) +
  geom_hex(mapping = aes(x = gdp_size, y = aid_total))

ggplot(data = pov.aid) +
  geom_hex(mapping = aes(x = pov_rate, y = aid_total))

ggplot(data = pov.aid) +
  geom_hex(mapping = aes(x = pov_rate, y = aid_capta))

# MODELLING
library(modelr)

mod <- lm(log(aid_total) ~ log(gdp_size), data = pov.aid)

pov.aid2 <- pov.aid %>% 
  add_residuals(mod) %>% 
  mutate(resid = exp(resid))

ggplot(data = pov.aid2) + 
  geom_point(mapping = aes(x = gdp_size, y = resid)) # I didnt quite understand this

ggplot(data = pov.aid2) + 
  geom_boxplot(mapping = aes(x = cregion, y = resid)) + coord_flip()

## DONT KNOW HOW TO USE THIS JUST YET

###########################################################################################
##################################### OTHER PLOT CHECKS ###################################
###########################################################################################


plot(pov.aid$aid_total, pov.aid$pov_rate)
# FROM THIS PLOT WE SEE THAT COUNTRIES THAT RECEIVED MORE TOTAL AID ARE COUNTRIES THAT
# HAVE LOWER POVERTY RATES.

plot(pov.aid$aid_crsc, pov.aid$pov_rate)
# THE SAME APPLIES IF ONLY DONORS, WITHOUT INTERNATIONAL ORGANIZATIONS ARE TAKEN INTO ACCOUNT

plot(pov.aid$aid_crsio, pov.aid$pov_rate)
# AND IF ONLY THE AID FROM INTERNATIONAL ORGANIZATIONS ARE TAKEN INTO ACCOUNT

plot(pov.aid$aid_total, pov.aid$gdp_size)
# AS ECONOMIES ARE HIGHER, HOWEVER THE TENDENCY SEEMS TO BE HIGHER AID

plot(pov.aid$aid_total, pov.aid$gdp_growth)
# IT SEEMS NOT TO BE ABOUT GDP GROWTH, BUT ON GDP SIZE ONLY

plot(pov.aid$gdp_size, pov.aid$aid_capta)
# ONLY COUNTRIES WITH LOW GDP SIZE HAVE RECEIVED HIGH PER CAPTA AID

plot(pov.aid$pov_rate, pov.aid$aid_capta)
# IT SEEMS TO HAVE PRETTY MUCH EQUAL DISTRIBUTION HERE

plot(pov.aid$gpd_capta, pov.aid$aid_capta)


##########################################################################################
################################ REGRESSION MODELS #######################################
##########################################################################################

# APPLYING A SIMPLE LINEAR REGRESSION USING POVERTY RATE AND TOTAL AID, TO SEE HOW THEY BEHAVE
povaidreg1 <- lm(data = pov.aid, aid_total ~ pov_rate)
summary (povaidreg1)

# AS IT SEEMS, THE HIGHER THE POVERTY RATE, THE LOWER THE AID RECEIVED BY THE COUNTRIES, AS
# WITH A NEGATIVE SIGNAL IN THE ESTIMATE WITH A SIGNIFICANT P VALUE OF 0.04791.

plot(povaidreg1)

# WHAT CAN I INFER ABOUT THE FITTED AND RESIDUALS?

# APPLYING A LINEAR REGRESSION USING GDP SIZE TO ESTIMATE THE VALUE OF TOTAL AID
povaidreg2 <- lm(data = pov.aid, aid_total ~ gdp_size)
summary(povaidreg2)

# WITH A HIGH SIGNFICANT VALUE, AND A POSITIVE SIGNAL, IT SEEMS HIGHER GDP ALLOWS FOR MORE
# AID RECEIVED IN TOTAL

plot(povaidreg2)

# IF GDP ALLOWS MORE AID, HOW DOES IT WORK WHEN TAKING POVERTY RATE INTO ACCOUNT CONTROLLING
# THE OUTCOMES OF INTERNATIONAL COOPERATION?

povaidmulti <- lm(data = pov.aid, aid_total ~ gdp_size + pov_rate)
summary(povaidmulti)

# WITH THIS REGRESSION, POVERTY RATE BECOMES STATISTICALLY NON SIGNIFICANT HOWEVER IT STILL SHOWS
# A NEGATIVE INFLUENCE OVER THE TOTAL AMOUNT OF AID RECEIVED BY THE COUNTRIES

plot(povaidmulti)

# CHECKING IF THE REGION PLAYS A ROLL IN THE OUTCOMES OF TOTAL AID
plot(pov.aid$cregion, pov.aid$aid_total)
aid.regionreg1 <- lm(data = pov.aid, aid_total ~ cregion)
summary(aid.regionreg1)

# IN RELATION TO THE AMAZON BASIN, ONLY  THE BAY OF BENGAL HAS A SLIGHT STATISTICAL SIGNIFICANCE
# SHOWING IT PLAYS A ROLE IN INCREASING THE VALUE OF AID.

# ADDING TO THE REGRESSION THE GDP GROWTH, THE MEDIAN INCOME AND THE REGION OF COUNTRIES
full.pov.aid.reg <- lm(data = pov.aid, aid_total ~ pov_rate + gdp_size + gdp_growth + cregion)
summary(full.pov.aid.reg)

# CHECKING AN INVERTED REGRESSION WHERE POVERTY RATES CONTROLL THE INFLUENCE OF GDP IN AID RECEIVING
inverse.pov.aid.reg <- lm(data = pov.aid, aid_total ~ gdp_size + pov_rate + gdp_growth + median_inc + cregion)
summary(inverse.pov.aid.reg)

# AGAIN GDP SHOWS TO BE MORE SIGNIFICANT THAN POVERTY RATE

