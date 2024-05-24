###In Class Week 3###
library(dplyr)
library(readxl)
library(ggplot2)
library(table1)
library(writexl)
library(survival)

data <- read_excel('../data/cleaned_data.xlsx')

data <- data |>
  mutate(sex = factor(sex, levels = c(1, 0), labels = c('Male', 'Female')),
         genre = factor(genre, levels = c(0, 1), labels = c('Non-Drama', 'Drama')),
         race = factor(race, levels = c(0, 1), labels = c('Non-White', 'White')))

# R1: sex & num_win
num_win_by_sex_percent_bar_plot <- 
  ggplot(data, aes(x = sex, fill = factor(num_win))) +
  geom_bar(position = "fill") + theme_classic() +
  ylab('Percentage') + scale_y_continuous(labels = scales::percent_format()) +
  ggtitle('Stacked Percentage Bar Plot for Number of Wins by Sex') +
  labs(fill = "Number of Wins")

ggsave('../visualization/num_win_by_sex_percent_bar_plot.png')

# R2: sex & num_nom
num_nom_hist_by_sex <- ggplot(data, aes(x = num_nom, fill = sex)) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 15) +
  theme_classic() +
  facet_wrap(~ sex, ncol = 1, scales = "free_y") +
  ggtitle('Histogram for Number of Nomination by Sex') +
  ylab("Number of Nomination")

ggsave('../visualization/num_nom_hist_by_sex.png')

# R3: films_total & num_nom
num_nom_and_films_total <- ggplot(data, aes(x = num_nom, y = films_total)) +
  geom_point(size = 1) + xlab("Number of Nominations") + 
  ylab("Total Number of Files") +
  ggtitle("Number of Nominations and Total Number of Films") +
  theme_classic() +
  geom_smooth(method = "lm", se = TRUE)

ggsave('../visualization/num_nom_and_films_total.png')

# R4: genre & num_nom
genre_and_num_nom <- ggplot(data, aes(x = num_nom, fill = genre)) +
  geom_histogram(alpha = 0.8, bins = 10) + 
  xlab("Number of Nominations") + ylab("Density") +
  ggtitle("Density of Number of Nominations for Drama vs Non-Drama Films") +
  theme_classic()
genre_and_num_nom
ggsave('../visualization/genre_and_num_nom.png')

# R5: num_4star_films & sex

num_4star_films_and_sex <- ggplot(data, aes(x = num_4star_films, fill = sex)) +
  geom_histogram(position = "dodge", binwidth = 2, alpha = 0.5) +
  xlab("Number of Films Starred") + ylab("Count") + 
  ggtitle("Histogram of Number of Films Starred by Sex") +
  theme_classic()
num_4star_films_and_sex
ggsave('../visualization/num_4star_films_and_sex.png')

# R6: race & num_win #
summary_data <- data %>%
  group_by(race) %>%
  summarise(mean_num_win = mean(num_win),
            se_num_win = sd(num_win) / sqrt(n()))

race_and_num_win <- ggplot(summary_data, aes(x = race, y = mean_num_win)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_errorbar(aes(ymin = mean_num_win - se_num_win, ymax = mean_num_win + se_num_win),
                width = 0.2) +
  labs(x = "Race", y = "Mean Number of Wins",
       title = "Mean Number of Wins by Race with SD") +
  theme_classic()

ggsave('../visualization/race_and_num_win.png')

# table 1 creation
caption <- "Table 1. Characteristics of Academy Awards Winners, Nominees, and Controls"
data <- data |>
  mutate(winstatus = ifelse(num_win >= 1, 'winner', 
                            ifelse(num_win == 0 & num_nom == 0, 'control', 'nominee')))
table1 <- table1(~ lifespan + sex + race + birth_country + name_change + genre + films_total + 
         num_4star_films + num_win + num_nom + status | winstatus, data=data,
       overall=F, 
       caption=caption,
       render.continuous = c(.="Mean (SD)", .="Median (Q1, Q3)", "Range"="Min - Max"))

##############Time-varying covariates

data = mutate(data, nom_years = first_nom_year - birth_year,
            win_years = first_win_year - birth_year,
            followup = ifelse(is.na(first_nom_year), final_year - birth_year,
                              pmax(final_year, first_win_year, na.rm=T)-birth_year),
            death_time = ifelse(status==1,final_year - birth_year,NA))

data_timevary <- tmerge(data1=data, data2=data, id=ID, tstop=followup,
                      death_tv = event(death_time),
                      nom_tv = tdc(nom_years),
                      win_tv = tdc(win_years))

summary(data$nom_years)
summary(data$win_years)
summary(data$followup)
summary(data$death_time)

# Hypothesis 1
data_timevary_h1 <- data_timevary[data_timevary$nom_tv==1,]
cox.fit_h1 = coxph(Surv(tstart, tstop, death_tv==1)~win_tv, data=data_timevary_h1)
summary(cox.fit_h1)

# Hypothesis 2 
data_timevary_h2 <- data_timevary[!((data_timevary$nom_tv==1) & (data_timevary$win_tv==0)),]
cox.fit_h2 = coxph(Surv(tstart, tstop, death_tv==1)~win_tv, data=data_timevary_h2)
summary(cox.fit_h2)

# Hypothesis 3
data_timevary_h3 <- data_timevary[!(data_timevary$win_tv==1),]
cox.fit_h3 = coxph(Surv(tstart, tstop, death_tv==1)~nom_tv, data=data_timevary_h3)
summary(cox.fit_h3)





