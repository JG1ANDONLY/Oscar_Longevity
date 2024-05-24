# Does Oscar Winning/Nomination Affect Longevity? A Survival Analysis.
Authors: Tanqing Feng, Zhongyi (Jame) Guo, Junjie Lu, Yuewei Ling, Sean Tsung <br>


*The authors are listed in alphabetical order by their last names and have contributed equally to this work.*

## Summary

We developed three Cox regression models to analyze the longevity differences among three groups: actors and actresses who have won Academy Awards (winners), those who have only been nominated (nominees), and those who have never been nominated (controls). These models considered the dynamic nature of group membership and included adjustments for the following confounding effects: birth year, name changes, involvement in the drama genre, total number of films, and the number of films rated four stars. Our finding indicates that there is no significant difference in longevity among the three groups.

## Methods

Demographic characteristics are expressed as mean ± standard deviation, median (interquartile range), and range for continuous variables, and are expressed as counts (proportions) for categorical variables. Actors and actresses were divided into three groups–winners (won Academy Awards), nominees (only ever nominated), and controls (never nominated). 

We identified actors or actresses with potentially erroneous records by checking for the following cases: (i) the number of Academy Awards won exceeds the number of nominations; (ii) the year of their first win is earlier than the year they were first nominated; (iii) the year their first film was released is later than the year they were first nominated; (iv) the number of four-star films exceeds the total number of films; (v) the year of death is later than the year of their first win. Actors and actresses in the first four scenarios were removed from our analysis, whereas those in the final scenario underwent additional validation to determine whether they warranted exclusion.

Correlation analysis was conducted to explore the potential relationships between variables: sex and number of wins, sex and number of nominations, total number of films and number of nominations, drama genre and number of nominations, sex number of four-star films, and sex and number of wins.

Cox regression models were established to analyze the longevity among winners versus controls, winners versus nominees, and nominees versus controls. Baseline characteristics showing significant intergroup differences were identified as potential confounders. Categorical variables and continuous variables that we found did not violate the linearity properties (based on martingale residual plots) were included in the final model. Proportional hazard assumptions were tested through diagnostic plots (log-log and Schoenfeld residuals) and interaction term testing. Our analysis was stratified by award status to address immortal time bias, recognizing their status only after they had won or been nominated. 

All analyses were conducted using R software (version 4.1.2), employing two-tailed tests with a significance threshold set at α = 0.05.

## Results

*Data preprocessing*

No inconsistencies were detected in the number of wins, nominations, four-star films, and total number of films. However, one individual (ID 1430) had the anomaly of achieving her first win before her initial nomination, which was abnormal as one cannot win an Oscar prior to being nominated. Additionally, three individuals (ID 908, ID 1192, ID 1521) had the unusual circumstance where their first films were released after their first year of nominations, which was also abnormal because one cannot be nominated for the Academy Award without having released a film. Consequently, these four individuals were excluded from the dataset, leaving 1666 individuals for our analysis. Notably, ID 1075 should be ***Massimo Troisi***, who earned posthumous Oscars nominations, a detail confirmed by examining real-life historical records.

*Correlation Analysis*

The distribution of Academy Award wins among actors and actresses displays a similar pattern (Figure 1). For both groups, the majority have not won any awards, followed by smaller percentages who have won once or twice. A very small number of actors or actresses have won three times. Only one actress won four Academy Awards.

![alt text](/visualization/fig1_sex_num_win.png) 
<strong>Figure 1.</strong> Stacked Percentage Bar Plot for Number of Wins by Sex

Most actors and actresses had no nominations for an Academy Award (Figure 2). However, among the actress group, although less frequent across nomination categories, there were instances of higher nomination times (11 and 12), implying these individuals might be more established or recognized. Both distributions aligned closely, displaying right-skewness, indicating rare instances of individuals with exceptionally high nomination counts. This emphasized the rarity and challenge of earning Academy Award nominations multiple times.

![alt text](/visualization/fig2_sex_num_nom.png)
<strong>Figure 2.</strong> Histogram for Number of Nominations by Sex

The positive slope of the linear regression between the number of Academy Award nominations and the total number of films suggests that as actors or actresses produced more films, their likelihood of being nominated for an Academy Award increased (Figure 3). 

![alt text](/visualization/fig3_films_total_num_nom.png)
<strong>Figure 3.</strong> Number of Nominations and Total Number of Films

The histogram depicting drama films occupied a larger area compared to that of non-drama films among nominated films (Figure 4), with both distributions exhibiting right skewness. One potential explanation for this trend was that narrative stories had a greater ability to engage and resonate with audiences, thus increasing their likelihood of receiving Academy Award nominations.

![alt text](/visualization/fig4_genre_num_nom.png)
<strong>Figure 4.</strong> Histogram for Number of Nominations by Film Type

The density plot section in the rain cloud plot section depicting the number of four-star films exhibited heavy right-skewness for both actors and actresses (Figure 5), aligning with the expectation that creating such films should be challenging. Additionally, actors had a higher median number of four-star films compared to actresses, while the interquartile range of actors surpassed that of actresses, indicating a potential sex bias in terms of film rating. Moreover, there were more outliers in the distribution of the number of four-star films among actors, suggesting that some actors might contribute many more four-star films than the other actors.

![alt text](/visualization/fig5_num_4star_films_sex.png)
<strong>Figure 5.</strong> Rain Cloud Plot of Four-star Films by Sex

The White actors and actresses had a higher mean and lower standard deviation of the number of times winning an Academy Award than all other ethnicities combined (Figure 6). This could imply that a race and ethnicity disparity might exist between the White ethnicity and other ethnicities in winning an Academy Award.

![alt text](/visualization/fig6_race_num_win.png)
<strong>Figure 6.</strong> Mean Number of Wins by Race with Standard Deviation (SD)

Among all continuous variables, only year of birth, year of the person’s first film, and age were normally distributed (Figure 7).

![alt text](/visualization/fig7_bar_histogram.png)
<strong>Figure 7.</strong> Bar Plots and Histograms of all Variables

Compared to nominees and controls, winners were significantly older (mean age 67.4 vs. 63.1 and 66.0 years, respectively; *p<0.001*) and had higher median numbers of total films (52 vs. 40 and 33; *p<0.001*), four-star films (9 vs. 6 and 4; *p<0.001*), and nominations (median 2 vs. 1; *p<0.001*). A higher proportion of winners had changed their name (28.6% vs. 22.4% and 9.2%; *p<0.001*) and worked in the drama genre (81.9% vs. 78.5% and 72.1%; *p=0.001*) relative to the nominee and control groups (Table 1).

<strong>Table 1.</strong> Baseline characteristics of actors and actresses who won Academy Awards (winners), were only ever nominated (nominees), or were never nominated (controls)



