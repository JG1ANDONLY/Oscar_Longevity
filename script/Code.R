

library(dplyr)
library(readxl)
library(survival)
library(ggplot2)
library(table1)
library(patchwork)
library(ggforce)
library(ggdist)
library(gghalves)
library(gridExtra)

################################################################################
#week1: data cleaning

{

# Load the data
data <- read_excel("../data/V1.academy_award_winners.xls")

# Rename the variables
new_column_names <- c("ID","sex","birth_country","race","name_change","genre","birth_year","final_year","status",
                      "films_total","films_star","num_win","num_nom","year_film","year_win","year_nom")
colnames(data) <- new_column_names

# Generate time in years
data$time <- data$final_year - data$birth_year

# Recode the event as 1=event (death/uncensored observation) and 0=censored
data$status <- 1 - data$status

# Recode variables containing "Yes" or "No"
yes_no_variables <- names(data)[sapply(data, function(x) any(grepl("Yes|No", x)))]
for (variable in yes_no_variables) {
  data[[variable]] <- ifelse(data[[variable]] == "Yes", 1, 0)
}

# Factor and label variables
# Generate new variables for win status
# win_ever: a binary variable if the person wins
# winstatus: a three level variable: 0 no nominations, 1: nominations but no win, 2: winners
data <- data %>%
  mutate(
    sex = factor(sex, levels = c(1, 0), labels = c('Male', 'Female')),
    genre = factor(genre, levels = c(0, 1), labels = c('Not Drama', 'Drama')),
    race = factor(race, levels = c(0, 1), labels = c('Non-White', 'White')),
    status = factor(status, levels = c(0, 1), labels = c('Alive', 'Dead')),
    birth_country = factor(birth_country, levels = c(0, 1), labels = c('No', 'Yes')),
    name_change = factor(name_change, levels = c(0, 1), labels = c('No', 'Yes')),
    win_ever = if_else(num_win >= 1, 1, 0),
    winstatus_n = ifelse(is.na(year_win) & is.na(year_nom),0,
                         ifelse(is.na(year_win) & !is.na(year_nom),1,2)),
    winstatus = factor(winstatus_n, levels=c(2,1,0), labels=c("Winners","Nominees","Controls"))
  )

#Set variable labels
label(data$birth_country)<-"Birthplace, US"
label(data$sex)<- "Sex"
label(data$race)<- "Race"
label(data$status)<- "Censorship Condition"
label(data$win_ever)<- "Ever Win Academy Award"
label(data$films_total)<- "Number of Films"
label(data$films_star)<- "Number of Four Star Films"
label(data$num_win)<- "Number of Wins"
label(data$num_nom)<- "Number of Nominations"
label(data$time)<- "Age (Years)"
label(data$genre)<- "Drama Genre"
label(data$name_change)<- "Name Change"
label(data$winstatus)<- "Win Status"
label(data$time)<- "Age at End"

}

################################################################################
#week 2: identify abnormal observations

{

#find possible inconsistencies in the data, 
#such as: a person who was first nominated for an Oscar after they first won an Oscar.

# Filter for inconsistencies
# flag1 <- num_win > num_nom → not exist
# flag2: year_win < year_nom: winning year is earlier than year of nomination
# flag3: year_film > year_nom: year of the first film is later than year of nomination
# flag4: films_star > films_total: number of 4 star movie is larger than total film number
# flag5: final_year < year_nom: final year in study is earlier than year of nomination
# Add flags to inconsistent winner(s)

data <- data %>%
  mutate(flag1 = ifelse(num_win > num_nom, 1, 0),
         flag2 = ifelse(year_win < year_nom, 1, 0),
         flag3 = ifelse(year_film > year_nom, 1, 0),
         flag4 = ifelse(films_star > films_total, 1, 0),
         flag5 = ifelse(final_year < year_nom, 1, 0)) %>%
  mutate(flag_general = ifelse(flag1 == 1 | flag2 == 1 | flag3 == 1 |flag4 == 1, 1, 0))
#we did not drop the flag5 because there is one person is nominated after he died, that is valid

# Print out IDs of inconsistent winner(s)
inconsistent_id <- data |>
  filter(flag_general == 1) |>
  select(ID)

# Remove inconsistent winner(s)
data <- data %>%
  filter(is.na(flag_general) | flag_general != 1)

}

################################################################################
#week 3: figures and table 1

#Figures
{
# R1: sex & num_win
  ggplot(data, aes(x = sex, fill = factor(num_win))) +
  geom_bar(position = "fill",alpha=0.8, width = 0.5) + theme_minimal() +
  ylab('Percentage') + scale_y_continuous(labels = scales::percent_format()) +
  ggtitle('Stacked Percentage Bar Plot for Number of Wins by Sex') +
  labs(fill = "Number of Wins") 
  
  ggsave('../visualization/fig1_sex_num_win.png')

# R2: sex & num_nom
# Overlapped histogram
ggplot(data, aes(x = num_nom, fill = sex)) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 15) + 
  theme_minimal() + 
  labs(title = "Histogram for Number of Nominations by Sex",
       x = "Number of Nominations",
       y = "Count") + 
  scale_x_continuous(breaks = seq(min(data$num_nom), max(data$num_nom), by = 1))

ggsave('../visualization/fig2_sex_num_nom.png')

# Rain cloud plot
# ggplot(data, aes(x = sex, y = num_nom, fill = sex)) +
#   ggdist::stat_halfeye(adjust = .5, width = .3, .width = 0, justification = -.3, point_colour = NA, alpha = 0.5) +
#   geom_boxplot(width = .05, outlier.shape = NA, alpha = 0.5)+
#   gghalves::geom_half_point(aes(color = sex), side = "l", range_scale = .4, alpha = .5) +
#   labs(title = "Raincloud Plot of Nomination Times by Sex", x = "Sex", y = "Number of Nominations", fill = "Sex") +
#   scale_y_continuous(breaks = seq(min(data$num_nom), max(data$num_nom), by = 1)) +
#   theme(plot.title = element_text(face = "bold"), axis.title.x = element_text(face = "bold"), axis.title.y = element_text(face = "bold")) +
#   theme_classic() + 
#   scale_fill_manual(values = c("Male" = "#00A1D5FF", "Female" = "#B24745FF")) +
#   scale_color_manual(values = c("Male" = "#00A1D5FF", "Female" = "#B24745FF")) +
#   coord_flip() 

# R3: films_total & num_nom
# Scatter
ggplot(data, aes(x = films_total, y = num_nom)) +
  geom_point(size = 1) + 
  xlab("Total Number of Films") + 
  ylab("Number of Nominations") +
  scale_x_continuous(breaks = seq(min(data$films_total), max(data$films_total), by = 20)) +
  scale_y_continuous(breaks = seq(min(data$num_nom), max(data$num_nom), by = 1)) +
  ggtitle("Number of Nominations and Total Number of Films") +
  theme_minimal() +
  geom_smooth(method = "lm", se = TRUE)

ggsave('../visualization/fig3_films_total_num_nom.png')

# R4: genre & num_nom

# Overlapped histogram
ggplot(data, aes(x = num_nom, fill = genre)) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 15) +
  theme_minimal() + 
  labs(title = "Histogram for Number of of Nominations by Film Type",x = "Number of Nominations", y = "Count",fill = "Film Type") + 
  scale_x_continuous(breaks = seq(min(data$num_nom), max(data$num_nom), by = 1))

ggsave('../visualization/fig4_genre_num_nom.png')

# Rain cloud plot
# ggplot(data, aes(x = genre, y = num_nom, fill = genre)) +
#   ggdist::stat_halfeye(adjust = .5, width = .3, .width = 0, justification = -.3, point_colour = NA, alpha = 0.5) +
#   geom_boxplot(width = .05, outlier.shape = NA, alpha = 0.5)+
#   gghalves::geom_half_point(aes(color = genre), side = "l", range_scale = .4, alpha = .5) +
#   labs(title = "Raincloud Plot of Nomination Times by Film Type", x = "Film Type", y = "Number of Nominations", fill = "Film Type") +
#   scale_y_continuous(breaks = seq(min(data$num_nom), max(data$num_nom), by = 1)) +
#   theme(plot.title = element_text(face = "bold"), axis.title.x = element_text(face = "bold"), axis.title.y = element_text(face = "bold")) +
#   theme_classic() + 
#   scale_fill_manual(values = c("Drama" = "#00A1D5FF", "Non-Drama" = "#B24745FF")) +
#   scale_color_manual(values = c("Drama" = "#00A1D5FF", "Non-Drama" = "#B24745FF")) +
#   coord_flip() 

# R5: num_4star_films & sex

# Rain cloud plot
ggplot(data, aes(x = sex, y = films_star, fill = sex)) +
  ggdist::stat_halfeye(adjust = .5, width = .3, .width = 0, justification = -.3, point_colour = NA, alpha = 0.5) +
  geom_boxplot(width = .05, outlier.shape = NA, alpha = 0.5)+
  gghalves::geom_half_point(aes(color = sex), side = "l", range_scale = .4, alpha = .5) +
  labs(title = "Raincloud Plot of Four-star Films by Sex", x = "Sex", y = "Number of Four-star Films", fill = "Sex") +
  scale_y_continuous(breaks = seq(min(data$films_star), max(data$films_star), by = 5)) +
  theme(plot.title = element_text(face = "bold"), axis.title.x = element_text(face = "bold"), axis.title.y = element_text(face = "bold")) +
  theme_classic() + 
  scale_fill_manual(values = c("Male" = "#00A1D5FF", "Female" = "#B24745FF")) +
  scale_color_manual(values = c("Male" = "#00A1D5FF", "Female" = "#B24745FF")) +
  coord_flip() 

ggsave('../visualization/fig5_num_4star_films_sex.png')

# R6: race & num_win

# Bar plot
summary_data <- data %>%
  group_by(race) %>%
  summarise(mean_num_win = mean(num_win),
            se_num_win = sd(num_win) / sqrt(n()))

ggplot(summary_data, aes(x = race, y = mean_num_win)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_errorbar(aes(ymin = mean_num_win - se_num_win, ymax = mean_num_win + se_num_win),
                width = 0.2) +
  labs(x = "Race", y = "Mean Number of Wins",
       title = "Mean Number of Wins by Race with SD") +
  theme_classic()

ggsave('../visualization/fig6_race_num_win.png')
# R7: race & num_nom
# ggplot(data, aes(x = race, y = num_nom, fill = race)) +
#   ggdist::stat_halfeye(adjust = .5, width = .3, .width = 0, justification = -.3, point_colour = NA, alpha = 0.5) +
#   geom_boxplot(width = .05, outlier.shape = NA, alpha = 0.5)+
#   gghalves::geom_half_point(aes(color = race), side = "l", range_scale = .4, alpha = .5) +
#   labs(title = "Raincloud Plot of Number of Nominations by Race", x = "Race", y = "Number of Nominations", fill = "Race") +
#   scale_y_continuous(breaks = seq(min(data$num_nom), max(data$num_nom), by = 1)) +
#   theme(plot.title = element_text(face = "bold"), axis.title.x = element_text(face = "bold"), axis.title.y = element_text(face = "bold")) +
#   theme_classic() + 
#   scale_fill_manual(values = c("Non-White" = "#00A1D5FF", "White" = "#B24745FF")) +
#   scale_color_manual(values = c("Non-White" = "#00A1D5FF", "White" = "#B24745FF")) +
#   coord_flip() 
}

# R7: bar plot or histogram for all variables
discreet_var <- data %>%
  select(sex, birth_country, race, name_change, genre, status, num_win, 
         num_nom, win_ever, winstatus)

continuous_var <- data %>%
  select(birth_year, final_year, films_total, films_star, year_film, year_win, 
         year_nom, time)

# Create bar plots for discrete variables
discreet_plots <- lapply(names(discreet_var), function(var) {
  ggplot(discreet_var, aes(x = var, fill = var)) +
    geom_bar() +
    labs(title = paste("Bar plot of", var),
         x = var, y = "Count") +
    scale_fill_discrete(name = var) + theme_minimal()
    
})

# Create histograms for continuous variables
continuous_plots <- lapply(names(continuous_var), function(var) {
  ggplot(continuous_var, aes_string(x = var)) +
    geom_histogram(binwidth = 1, fill = "blue", color = "black") +
    labs(title = paste("Histogram of", var),
         x = var, y = "Frequency") + theme_minimal()
})


# Arrange plots into a grid
grid_arrangement <- grid.arrange(grobs = c(discreet_plots, continuous_plots),
                                 nrow = 5, ncol = 4)

ggsave("../visualization/fig7_bar_histogram.png", grid_arrangement, width = 12, height = 12, units = "in")


#Table 1
{
#Function to calculate p-values
pvalue <- function(x, ...) {
  #Remove overall column if present
  if ("overall" %in% names(x)) {
    x["overall"] <- NULL
  }
  # Construct vectors of data y, and groups (strata) g
  y <- unlist(x)
  g <- factor(rep(1:length(x), times=sapply(x, length)))
  
  #For numeric variables across 2 strata, perform a 2-sample t-test
  if (is.numeric(y) & length(unique(g)) == 2) {
    p <- t.test(y ~ g)$p.value
  }
  
  # For numeric variables across >2 strata, perform ANOVA
  if (is.numeric(y) & length(unique(g)) > 2) {
    anova = aov(y ~ g)
    p <- summary(anova)[[1]][[5]][1]
  } 
  else {
    # For categorical variables, perform a chi-squared test
    p <- chisq.test(table(y, g))$p.value
  }
  # Format the p-value, using an HTML entity for the less-than sign (Remove if knitting to PDF)
  c(sub("<", "&lt;", format.pval(p, digits=2, nsmall=2, eps=0.001)))
}

#Set caption
caption <- "Table 1. Characteristics of Academy Awards Winners, Nominees, and Controls"

#Set units for continuous variables
units(data$time)  <- "years"

#Create table- remove overall column, render continuous as mean (SD), median (Q1, Q3), min/max
t2 <- table1(~ time + sex + race + birth_country + name_change + genre + films_total + 
               films_star + num_win + num_nom + status | winstatus, data=data,
             overall=F, 
             extra.col=list(`P-value`=pvalue), 
             caption=caption,
             render.continuous = c(.="Mean (SD)", .="Median (Q1, Q3)", "Range"="Min - Max"))
t2
}

################################################################################
#week 4 and week 5(mid-term)

#h1: winners better than nominees
#h2: winners better than controls
#h3: nominees better than controls

# generate new variables
# new variables: 
# nom_years: the age when the person was first nominated
# win_years: the age when the person first win
# followup: the follow up time: start time is the birth age
# death_time: people's age when died

data = mutate(data, nom_years = year_nom - birth_year,
              win_years = year_win - birth_year,
              followup = ifelse(is.na(year_nom), final_year - birth_year, 
                                # if year_nom (first nomination year) is NA (the person never got nomination), the follow up is final year minus first year
                                pmax(final_year, year_win, na.rm=T) - birth_year), 
                                # account for the winners who get the award after they died
                                # year_nom is not missing (the person got nomination), the follow up is the later year between (final_year and first win year) - birth_year
              death_time = ifelse(status=="Dead",final_year - birth_year,NA))

# set variable labels
label(data$nom_years)<-"Age when first nominated"
label(data$win_years)<-"Age when first winned"
label(data$followup)<-"Follow-up time"
label(data$death_time)<-"Death age"

# generate time varying variables: separate the age period by nomination and win status, examine the death status for each age period 
data_timevary <- tmerge(data,data,id=ID,tstop=followup,
                        death_tv = event(death_time),
                        nom_tv = tdc(nom_years),
                        win_tv = tdc(win_years))

#####################################
# Hypothesis 1: winners VS nominees #
#####################################

# Drop those controls who are never nominated (include those who are nominated)
data_timevary_h1 <- data_timevary[data_timevary$nom_tv==1,]

# Fit the naive model for hypothesis 1 (not adjusting for confounding)
cox.fit_h1 = coxph(Surv(tstart, tstop, death_tv==1)~win_tv, data=data_timevary_h1)
summary(cox.fit_h1)

##########################################################
# Test assumption 1: the proportional hazards assumption #
##########################################################

# Method 1: log log plot
cox.fit_h1 = survfit(Surv(followup, death_tv == 1) ~ win_tv, data = data_timevary_h1)
plot(cox.fit_h1, fun = "cloglog")
# Looks parallel

# Method 2: Schoenfeld residual plot
cox.fit_h1 = coxph(Surv(followup, death_tv == 1) ~ win_tv, data = data_timevary_h1)
x = data_timevary_h1[data_timevary_h1$death_tv==1,]$followup
y = residuals(cox.fit_h1, type ="schoenfeld")
plot(x,y)
lines(smooth.spline(x,y,spar=0.8))
# Looks no pattern

# Method 3: regression Schoenfeld residual on time
summary(lm(y~x))
# The slope is not significant

# Method 4: Interaction between win status (winners VS nominees) and time
# use "winstatus_n" because tt function does not take factor
cox.fit_h1 = coxph(Surv(followup, death_tv == 1) ~ winstatus_n + tt(winstatus_n), data = data_timevary_h1,
                tt = function (x,t,...)x*t)
summary(cox.fit_h1)
# The interaction term not significant

###########################################################
# Test assumption 2: linear relationship with log hazards #
###########################################################

# Birth year: violated
# The model
cox.fit_h1 = coxph(Surv(tstart, tstop, death_tv==1)~win_tv, data=data_timevary_h1)
# Plot the martingale residual
x = data_timevary_h1$birth_year
y = residuals(cox.fit_h1,type ="martingale")
plot(x,y)
lines(smooth.spline(x,y,spar=0.8))
# There is a pattern of the martingale residual so the linear assumption is violated

# Films total: violated
# The model
cox.fit_h1 = coxph(Surv(tstart, tstop, death_tv==1)~win_tv, data=data_timevary_h1)
# Plot the martingale residual
x = data_timevary_h1$films_total
y = residuals(cox.fit_h1,type ="martingale")
plot(x,y)
lines(smooth.spline(x,y,spar=0.8))
# There is a pattern of the martingale residual so the linear assumption is violated

# Films star: met
# The model
cox.fit_h1 = coxph(Surv(tstart, tstop, death_tv==1)~win_tv, data=data_timevary_h1)
# Plot the martingale residual
x = data_timevary_h1$films_star
y = residuals(cox.fit_h1,type ="martingale")
plot(x,y)
lines(smooth.spline(x,y,spar=0.8))
# There is no pattern of the martingale residual so the linear assumption is met

# The final model for hypothesis 1: adjusting for sex, birth_country, race, name_change, genre, films_star, 

cox.fit_h1 = coxph(Surv(tstart, tstop, death_tv==1) ~ win_tv + sex + birth_country + race + name_change + genre + films_star, data=data_timevary_h1)
summary(cox.fit_h1)

####################################
# Hypothesis 2: winners VS control #
####################################

# Drop those who are nominated but never wins
# data_timevary_h2 <- data_timevary[!((data_timevary$nom_tv==1) & (data_timevary$win_tv==0)),]

data_timevary_h2 <- data_timevary %>% filter(winstatus %in% c("Winners", "Controls"))

# Fit the naive model for hypothesis 2 (not adjusting for confounding)
cox.fit_h2 = coxph(Surv(tstart, tstop, death_tv==1)~win_tv, data=data_timevary_h2)
summary(cox.fit_h2)

##########################################################
# Test assumption 1: the proportional hazards assumption #
##########################################################

# Method 1: log log plot
cox.fit_h2 = survfit(Surv(followup, death_tv == 1) ~ win_tv, data = data_timevary_h2)
plot(cox.fit_h2, fun = "cloglog")
# violated

# Method 2: Schoenfeld residual plot
cox.fit_h2 = coxph(Surv(followup, death_tv == 1) ~ win_tv, data = data_timevary_h2)
x = data_timevary_h2[data_timevary_h2$death_tv==1,]$followup
y = residuals(cox.fit_h2, type ="schoenfeld")
plot(x,y)
lines(smooth.spline(x,y,spar=0.8))
# 

# Method 3: regression Schoenfeld residual on time
summary(lm(y~x))
# slope not significant

# Method 4: Interaction between win status (winners VS nominees) and time
# use "winstatus_n" because tt function does not take factor
cox.fit_h2 = coxph(Surv(followup, death_tv == 1) ~ winstatus_n + tt(winstatus_n), data = data_timevary_h2,
                   tt = function (x,t,...)x*t)
summary(cox.fit_h2)
# intereaction not significant

###########################################################
# Test assumption 2: linear relationship with log hazards #
###########################################################

# Birth year: violated
# The model
cox.fit_h2 = coxph(Surv(tstart, tstop, death_tv==1)~win_tv, data=data_timevary_h2)
# Plot the martingale residual
x = data_timevary_h2$birth_year
y = residuals(cox.fit_h2,type ="martingale")
plot(x,y)
lines(smooth.spline(x,y,spar=0.8))

# Films total: violated
# The model
cox.fit_h2 = coxph(Surv(tstart, tstop, death_tv==1)~win_tv, data=data_timevary_h2)
# Plot the martingale residual
x = data_timevary_h2$films_total
y = residuals(cox.fit_h2,type ="martingale")
plot(x,y)
lines(smooth.spline(x,y,spar=0.8))

# Films star: met
# The model
cox.fit_h2 = coxph(Surv(tstart, tstop, death_tv==1)~win_tv, data=data_timevary_h2)
# Plot the martingale residual
x = data_timevary_h2$films_star
y = residuals(cox.fit_h2,type ="martingale")
plot(x,y)
lines(smooth.spline(x,y,spar=0.8))

# The final model for hypothesis 2: adjusting for win_tv + sex + birth_country + race + name_change + genre + films_star

cox.fit_h2 = coxph(Surv(tstart, tstop, death_tv==1) ~ win_tv + sex + birth_country + race + name_change + genre + films_star, data=data_timevary_h2)
summary(cox.fit_h2)

####################################
# Hypothesis 3: Nominee VS control #
####################################

# Drop those who wins
# data_timevary_h3 <- data_timevary[!(data_timevary$win_tv==1),]

data_timevary_h3 <- data_timevary %>% filter(winstatus %in% c("Nominees", "Controls"))

# Fit the naive model for hypothesis 1 (not adjusting for confounding)
cox.fit_h3 = coxph(Surv(tstart, tstop, death_tv==1)~nom_tv, data=data_timevary_h3)
summary(cox.fit_h3)

##########################################################
# Test assumption 1: the proportional hazards assumption #
##########################################################

# Method 1: log log plot
cox.fit_h3 = survfit(Surv(followup, death_tv == 1) ~ nom_tv, data = data_timevary_h3)
plot(cox.fit_h3, fun = "cloglog")
# looks parallel

# Method 2: Schoenfeld residual plot
cox.fit_h3 = coxph(Surv(followup, death_tv == 1) ~ nom_tv, data = data_timevary_h3)
x = data_timevary_h3[data_timevary_h3$death_tv==1,]$followup
y = residuals(cox.fit_h3, type ="schoenfeld")
plot(x,y)
lines(smooth.spline(x,y,spar=0.8))
# no specific pattern

# Method 3: regression Schoenfeld residual on time
summary(lm(y~x))
# slope not significant

# Method 4: Interaction between win status (winners VS nominees) and time
# use "winstatus_n" because tt function does not take factor
cox.fit_h3 = coxph(Surv(followup, death_tv == 1) ~ winstatus_n + tt(winstatus_n), data = data_timevary_h3,
                   tt = function (x,t,...)x*t)
summary(cox.fit_h3)
# intereaction not significant

###########################################################
# Test assumption 2: linear relationship with log hazards #
###########################################################

# Birth year: violated
# The model
cox.fit_h3 = coxph(Surv(tstart, tstop, death_tv==1)~nom_tv, data=data_timevary_h3)
# Plot the martingale residual
x = data_timevary_h3$birth_year
y = residuals(cox.fit_h3,type ="martingale")
plot(x,y)
lines(smooth.spline(x,y,spar=0.8))

# Films total: violated
# The model
cox.fit_h3 = coxph(Surv(tstart, tstop, death_tv==1)~nom_tv, data=data_timevary_h3)
# Plot the martingale residual
x = data_timevary_h3$films_total
y = residuals(cox.fit_h3,type ="martingale")
plot(x,y)
lines(smooth.spline(x,y,spar=0.8))

# Films star: met
# The model
cox.fit_h3 = coxph(Surv(tstart, tstop, death_tv==1)~nom_tv, data=data_timevary_h3)
# Plot the martingale residual
x = data_timevary_h3$films_star
y = residuals(cox.fit_h3,type ="martingale")
plot(x,y)
lines(smooth.spline(x,y,spar=0.8))

# The final model for hypothesis 2: adjusting for nom_tv + sex + birth_country + race + name_change + genre + films_star

cox.fit_h3 = coxph(Surv(tstart, tstop, death_tv==1) ~ nom_tv + sex + birth_country + race + name_change + genre + films_star, data=data_timevary_h3)
summary(cox.fit_h3)


























