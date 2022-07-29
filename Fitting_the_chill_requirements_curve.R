#To fit the curve

library(ggplot2) #for plotting
library(reshape2)
library(dplyr)
library(emmeans) #for confidence interval
library(dplyr) #for pipes
library(MASS) #for prediction of infliction point with standard error


##################
#using raw data
##################

#read data

Data <- read.csv('data/Budbreak_datafinal20212022.csv', sep = ",")

###############################################################

#bring date in Date format 
Data$Collection <- as.Date(Data$Collection, format  = '%d/%m/%Y')

#share of buds in bloom
Data$share_budbreak <- Data$Buds_51_stage_BBCH / Data$Total_Buds

#plot without fitted curve
ggplot(Data, aes(x = Chill_Portions, y = share_budbreak)) + geom_point()+
  facet_grid(~Variety)


#eduardo transferred the average bloom data back to the original 1 / 0 format
#--> need function to convert Total Buds and share_budbreak to 0 and 1
convert_relative_frequency <- function(n, occ){
  #get number of occurences
  n_bloom <- occ
  n_no_bloom <- (n - occ)
  
  #transform to vector with 0 and 1 instead
  return(c(rep(1, n_bloom), rep(0, n_no_bloom)))
  
}

convert_relative_frequency(n = Data$Total_Buds[1],
                           occ = Data$Buds_51_stage_BBCH[1])



budbreak_abs <- data.frame(NULL)
for(i in 1:nrow(Data)){
  abs_freq <- convert_relative_frequency(n = Data$Total_Buds[i],
                             occ = Data$Buds_51_stage_BBCH[i])
  budbreak_abs <- rbind(budbreak_abs,
                        data.frame(variety = Data$Variety[i],
                                   twig = Data$Twig[i],
                                   collection_date = Data$Collection[i],
                                   measurement_date = Data$Measurement_day[i],
                                   chill_portions = Data$Chill_Portions[i],
                                   buddbreak = abs_freq))
}

ggplot(budbreak_abs, aes(x = chill_portions, y = buddbreak)) + geom_point()+
  facet_grid(~variety)

#split variety into season and variety

# n_last <- 4                                # Specify number of characters to extract
# budbreak_abs$season <- substr(budbreak_abs$variety, nchar(budbreak_abs$variety) - n_last + 1, nchar(budbreak_abs$variety)) # Extract last three characters
# budbreak_abs$season <- as.numeric(budbreak_abs$season)
# 
# budbreak_abs$variety <- substring(budbreak_abs$variety, 1, nchar(budbreak_abs$variety)-(n_last+1))


ggplot(budbreak_abs, aes(x = chill_portions, y = buddbreak)) + geom_point()+
  facet_grid(season~variety)


#split the data by variety and year
budbreak_list <- split(budbreak_abs, budbreak_abs$variety)

model_list <- purrr::map(budbreak_list, function(x) glm(formula = buddbreak ~ chill_portions, 
                                                        family = binomial, data = x, weights = NULL ))

yhat.df2 <-  model_list %>%
  purrr::map(function(x) emmeans(x, ~ chill_portions, at=list(chill_portions=seq(0,100,by=.1)), type = 'response')) %>%
  purrr::map(as.data.frame) %>%
  bind_rows(.id = 'variety')
  

#fit binomial model for each variety, but not for each 
#m2 <- glm(formula = buddbreak ~ chill_portions + variety, family = binomial, data = budbreak_abs, weights=NULL )

#get confidence interval
#yhat.df2 <- emmeans(m2, ~chill_portions, at=list(chill_portions=seq(0,92,by=.1)), type='response', by= 'variety') %>%
#  as.data.frame()



#plot fitted curve with confidence interval
ggplot(budbreak_abs, aes(x = chill_portions)) +
  geom_ribbon(data = yhat.df2, aes(ymin = asymp.LCL, ymax = asymp.UCL), 
              fill = 'blue', alpha = .4) +
  geom_line( data = yhat.df2, aes( y = prob), color = 'blue') +
  geom_point(aes(y = buddbreak)) +
  labs(y='Share of broken buds', x='Chill Accumulation (CP)')+
  facet_wrap(~variety, nrow = 3, ncol = 2)

###########################################
##I want to add b50 as "CR" (to each one)
###########################################


#get chillportions at 50% budbreak
budbreak_50 <- purrr::map(model_list, MASS::dose.p, p = c(.5))

  
  
#b50 <- MASS::dose.p(m2, p=c(.5))
#b50
#--> much smaller std