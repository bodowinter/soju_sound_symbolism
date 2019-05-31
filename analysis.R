## Bodo Winter
## June 7, 2017
## Analysis of experiments

##------------------------------------------------------------------
## Load stuff:
##------------------------------------------------------------------

## Load in libraries:

library(tidyverse)
library(stringr)
library(lme4)
library(afex)
library(MuMIn)

## Set working directory:

mainPath <- '/Users/winterb/Research/harkness_soju/analysis'
setwd(file.path(mainPath, 'processed_data/'))

## Load in files:

E1 <- read_csv('E1_hardness.csv')
E2 <- read_csv('E2_roughness.csv')
E3 <- read_csv('E3_gender.csv')
E4 <- read_csv('E4_soju_content.csv')
E5 <- read_csv('E5_taste.csv')
E6 <- read_csv('E6_pleasant.csv')

ger <- read_csv('german_processed.csv')
spa <- read_csv('spanish_processed.csv')
chi <- read_csv('chinese_processed.csv')


##------------------------------------------------------------------
## Data carpentry:
##------------------------------------------------------------------

## List of object names:

mydfs <- c('E1', 'E2', 'E3', 'E4', 'E5', 'E6')

## Loop through and exclude non-native speakers:

for (i in 1:length(mydfs)) {
	this_df <- get(mydfs[i])
	
	## Check whether they are English:

	natives <- str_to_lower(this_df$Native)
	natives <- str_detect(natives, 'english')
	
	## Exclude:
	
	this_df <- this_df[which(natives), ]
	
	assign(mydfs[i], this_df)
	}

## Exclude 'other' so that gender can be sum-coded:

E5 <- filter(E5,
	Gender != 'other')

## Check N:

nrow(E1) / 2
nrow(E2) / 2
nrow(E3) / 2
nrow(E4) / 2
nrow(E5) / 2
nrow(E6) / 2

nrow(spa) / 2
nrow(ger) / 2
nrow(chi) / 2

## Gender composition:

table(E1$Gender) / 2
table(E2$Gender) / 2
table(E3$Gender) / 2
table(E4$Gender) / 2
table(E5$Gender) / 2
table(E6$Gender) / 2

## Total sample size:

# First four:
(nrow(E1) + nrow(E2) + nrow(E3) + nrow(E4)) / 2

# First four:
N <- nrow(E1) + nrow(E2) + nrow(E3) + nrow(E4) + nrow(E5) + nrow(E6)
N <- N + nrow(ger) + nrow(chi) + nrow(spa)
N / 2

## Age composition:

mean(E1$Age, na.rm = T); range(E1$Age, na.rm = T)
mean(E2$Age, na.rm = T); range(E2$Age, na.rm = T)
mean(E3$Age, na.rm = T); range(E3$Age, na.rm = T)
mean(E4$Age, na.rm = T); range(E4$Age, na.rm = T)
mean(E5$Age, na.rm = T); range(E5$Age, na.rm = T)
mean(E6$Age, na.rm = T); range(E6$Age, na.rm = T)

mean(ger$Age, na.rm = T); range(ger$Age, na.rm = T)
mean(chi$Age, na.rm = T); range(chi$Age, na.rm = T)

spa <- mutate(spa,
	Age = ifelse(Age == "33 años", 33, Age),
	Age = as.numeric(Age))
spa <- filter(spa, Age != 3, Age != 16)
mean(spa$Age, na.rm = T); range(spa$Age, na.rm = T)

## Exclude "other":

chi <- filter(chi, Gender != '其他')
spa <- filter(spa, Gender != 'otro')

## Gender composition:

table(ger$Gender) / 2
table(chi$Gender) / 2
table(spa$Gender) / 2

## Loop through to sum and deviation code:

for (i in 1:length(mydfs)) {
	this_df <- get(mydfs[i])
	
	# Make categorical variables into factors:
	
	this_df <- mutate(this_df,
		RespType = as.factor(RespType),
		StimGender = as.factor(StimGender),
		Gender = as.factor(Gender),
		Question = as.factor(Question))
	
	# Sum code predictors:
	
	contrasts(this_df$StimGender) <- contr.sum(2)
	contrasts(this_df$Gender) <- contr.sum(2)
	contrasts(this_df$Question) <- contr.sum(2)
	
	assign(mydfs[i], this_df)
	}

## This is where we can work with all data together:

mydfs <- c(mydfs, c('ger', 'spa', 'chi'))

## Check how many per sample know Korean stuff:

for (i in 1:length(mydfs)) {
	cat(paste0('Analyzing ', paste0(mydfs[i], '\n')))
	
	this_df <- get(mydfs[i])
	
	## Number of participants:
	
	cat(paste0(nrow(this_df) / 2))
	
	# Korean stuff:
	
	print(table(this_df$KoreanVisit) / 2)
	print(table(this_df$KoreanLanguage) / 2)
	print(table(this_df$SojuQuestion) / 2)
	print(table(this_df$KoreanRestaurant) / 2)
	print(table(this_df$KoreanFriends) / 2)
	print(table(this_df$KoreanDrama) / 2)
	
	cat('\n\n------------------------\n')
	}

## Make into factor:

ger <- mutate(ger, RespType = factor(RespType))
spa <- mutate(spa, RespType = factor(RespType))
chi <- mutate(chi, RespType = factor(RespType))

## Exclude those participants that know Korean or have been there:

for (i in 1:length(mydfs)) {
	this_df <- get(mydfs[i])
	
	# Make categorical variables into factors:
	
	this_df <- filter(this_df,
		KoreanLanguage %in% c('None', 'Nein', 'Ninguno', '无'),
		KoreanVisit %in% c('Never', 'Niemals', 'Nunca', '从未去过'))
	
	assign(paste0(mydfs[i], '_red'), this_df)
	}

## How many had to be excluded?

(nrow(E1) - nrow(E1_red)) / 2
1 - (nrow(E1_red) / nrow(E1))

(nrow(E2) - nrow(E2_red)) / 2
1 - (nrow(E2_red) / nrow(E2))

(nrow(E3) - nrow(E3_red)) / 2
1 - (nrow(E3_red) / nrow(E3))

(nrow(E4) - nrow(E4_red)) / 2
1 - (nrow(E4_red) / nrow(E4))

(nrow(E5) - nrow(E5_red)) / 2
1 - (nrow(E5_red) / nrow(E5))

(nrow(E6) - nrow(E6_red)) / 2
1 - (nrow(E6_red) / nrow(E6))

(nrow(ger) - nrow(ger_red)) / 2
1 - (nrow(ger_red) / nrow(ger))	# 20%

(nrow(spa) - nrow(spa_red)) / 2
1 - (nrow(spa_red) / nrow(spa))	# 7%

(nrow(chi) - nrow(chi_red)) / 2
1 - (nrow(chi_red) / nrow(chi))	# 32%

## Check gender and age of cross-linguistic ones:




##------------------------------------------------------------------
## Inferential statistics:
##------------------------------------------------------------------

## Formula for all models:

myFormula <- as.formula('RespType ~ StimGender + Gender + Question +
	Question:Gender + Question:StimGender + (1|ID)')
myFormula_red <- as.formula('RespType ~ StimGender + Gender + (1|ID)')

## Create models:

summary(E1.mdl <- glmer(myFormula, data = E1_red, family = 'binomial'))
summary(E2.mdl <- glmer(myFormula, data = E2_red, family = 'binomial'))
summary(E3.mdl <- glmer(myFormula, data = E3_red, family = 'binomial'))
summary(E4.mdl <- glmer(myFormula, data = E4_red, family = 'binomial'))
summary(E5.mdl <- glmer(myFormula, data = E5_red, family = 'binomial'))
summary(E6.mdl <- glmer(myFormula, data = E6_red, family = 'binomial'))

## Cross-cultural full versions (here: no exclusions due to Korean familiarity):

summary(ger.mdl <- glmer(myFormula, data = ger, family = 'binomial', control = glmerControl(optimizer = 'bobyqa')))
summary(spa.mdl <- glmer(myFormula, data = spa, family = 'binomial', control = glmerControl(optimizer = 'bobyqa')))
summary(chi.mdl <- glmer(myFormula, data = chi, family = 'binomial', control = glmerControl(optimizer = 'bobyqa')))

summary(ger.mdl_null <- glmer(myFormula_red, data = ger, family = 'binomial', control = glmerControl(optimizer = 'bobyqa')))
summary(spa.mdl_null <- glmer(myFormula_red, data = spa, family = 'binomial', control = glmerControl(optimizer = 'bobyqa')))
summary(chi.mdl_null <- glmer(myFormula_red, data = chi, family = 'binomial', control = glmerControl(optimizer = 'bobyqa')))

## Cross-cultural reduced versions (exclusions due to Korean familiarity):

summary(ger_red.mdl <- glmer(myFormula, data = ger_red, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))
summary(spa_red.mdl <- glmer(myFormula, data = spa_red, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))
summary(chi_red.mdl <- glmer(myFormula, data = chi_red, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))

summary(ger.mdl_red_null <- glmer(myFormula_red, data = ger_red, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))
summary(spa.mdl_red_null <- glmer(myFormula_red, data = spa_red, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))
summary(chi.mdl_red_null <- glmer(myFormula_red, data = chi_red, family = 'binomial',
	control = glmerControl(optimizer = 'bobyqa')))

## Create models for R-squared comparison:

summary(E1.mdl_red <- glmer(myFormula_red, data = E1_red, family = 'binomial'))
summary(E2.mdl_red <- glmer(myFormula_red, data = E2_red, family = 'binomial'))
summary(E3.mdl_red <- glmer(myFormula_red, data = E3_red, family = 'binomial'))
summary(E4.mdl_red <- glmer(myFormula_red, data = E4_red, family = 'binomial'))
summary(E5.mdl_red <- glmer(myFormula_red, data = E5_red, family = 'binomial'))
summary(E6.mdl_red <- glmer(myFormula_red, data = E6_red, family = 'binomial'))

## R-squared:

r.squaredGLMM(E1.mdl)	# convergence issue
r.squaredGLMM(E2.mdl)
r.squaredGLMM(E3.mdl)
r.squaredGLMM(E4.mdl)
r.squaredGLMM(E5.mdl)
r.squaredGLMM(E6.mdl)

## Overall R-squared cross-linguistic:

r.squaredGLMM(ger_red.mdl)
r.squaredGLMM(spa_red.mdl)
r.squaredGLMM(chi_red.mdl)

r.squaredGLMM(ger.mdl)
r.squaredGLMM(spa.mdl)
r.squaredGLMM(chi.mdl)

## R-squared for models without question effects:

r.squaredGLMM(E1.mdl)[1] - r.squaredGLMM(E1.mdl_red)[1]
r.squaredGLMM(E2.mdl)[1] - r.squaredGLMM(E2.mdl_red)[1]
r.squaredGLMM(E3.mdl)[1] - r.squaredGLMM(E3.mdl_red)[1]
r.squaredGLMM(E4.mdl)[1] - r.squaredGLMM(E4.mdl_red)[1]
r.squaredGLMM(E5.mdl)[1] - r.squaredGLMM(E5.mdl_red)[1]
r.squaredGLMM(E6.mdl)[1] - r.squaredGLMM(E6.mdl_red)[1]

## Same for cross-cultural:

r.squaredGLMM(ger_red.mdl)[1] - r.squaredGLMM(ger.mdl_red_null)[1]
r.squaredGLMM(spa_red.mdl)[1] - r.squaredGLMM(spa.mdl_red_null)[1]
r.squaredGLMM(chi_red.mdl)[1] - r.squaredGLMM(chi.mdl_red_null)[1]

## R-squared for cross-cultural without exclusions:

summary(ger.mdl_null <- glmer(myFormula_red, data = ger, family = 'binomial'))
summary(spa.mdl_null <- glmer(myFormula_red, data = spa, family = 'binomial'))
summary(chi.mdl_null <- glmer(myFormula_red, data = chi, family = 'binomial'))

r.squaredGLMM(ger.mdl)[1] - r.squaredGLMM(ger.mdl_null)[1]
r.squaredGLMM(spa.mdl)[1] - r.squaredGLMM(spa.mdl_null)[1]
r.squaredGLMM(chi.mdl)[1] - r.squaredGLMM(chi.mdl_null)[1]

## Create likelihood ratio tests:

E1.afex <- mixed(myFormula, E1_red, family = 'binomial', method = 'LRT')
E2.afex <- mixed(myFormula, E2_red, family = 'binomial', method = 'LRT')
E3.afex <- mixed(myFormula, E3_red, family = 'binomial', method = 'LRT')
E4.afex <- mixed(myFormula, E4_red, family = 'binomial', method = 'LRT')
E5.afex <- mixed(myFormula, E5_red, family = 'binomial', method = 'LRT')
E6.afex <- mixed(myFormula, E6_red, family = 'binomial', method = 'LRT')

ger.afex <- mixed(myFormula, ger, family = 'binomial', method = 'LRT')
spa.afex <- mixed(myFormula, spa, family = 'binomial', method = 'LRT')
chi.afex <- mixed(myFormula, chi, family = 'binomial', method = 'LRT')

ger.afex_red <- mixed(myFormula, ger_red, family = 'binomial', method = 'LRT')
spa.afex_red <- mixed(myFormula, spa_red, family = 'binomial', method = 'LRT')
chi.afex_red <- mixed(myFormula, chi_red, family = 'binomial', method = 'LRT')

## Look at this:

E1.afex$anova_table
E2.afex$anova_table
E3.afex$anova_table
E4.afex$anova_table
E5.afex$anova_table
E6.afex$anova_table

ger.afex$anova_table
spa.afex$anova_table
chi.afex$anova_table

ger.afex_red$anova_table
spa.afex_red$anova_table
chi.afex_red$anova_table

## Do separate Chi-Square tests:

mydfs_red <- paste0(mydfs, '_red')
mydfs_red[mydfs_red == 'ger_red'] <- 'ger'
mydfs_red[mydfs_red == 'spa_red'] <- 'spa'
mydfs_red[mydfs_red == 'chi_red'] <- 'chi'

for (i in 1:length(mydfs_red)) {
	cat(paste0('Analyzing ', paste0(mydfs_red[i], '\n')))
	
	this_df <- get(mydfs_red[i])
	
	male <- filter(this_df, StimGender == 'Male')
	female <- filter(this_df, StimGender == 'Female')
	
	male.tab <- table(male$Question, male$RespType)
	female.tab <- table(female$Question, female$RespType)
	
	print(male.tab)
	print(female.tab)
	
	print(chisq.test(male.tab))
	print(chisq.test(female.tab))
	
	cat('\n\n------------------------\n')
	
	}

## Print descriptive stats:

print(E1.tab <- table(E1_red$Question, E1_red$RespType))
print(E1.round <- round(prop.table(E1.tab, 1), 2))

print(E2.tab <- table(E2_red$Question, E2_red$RespType))
print(E2.round <- round(prop.table(E2.tab, 1), 2))

print(E3.tab <- table(E3_red$Question, E3_red$RespType))
E3.tab <- E3.tab[c(2, 1), ]
print(E3.round <- round(prop.table(E3.tab, 1), 2))

print(E4.tab <- table(E4_red$Question, E4_red$RespType))
print(E4.round <- round(prop.table(E4.tab, 1), 2))

print(E5.tab <- table(E5_red$Question, E5_red$RespType))
print(E5.round <- round(prop.table(E5.tab, 1), 2))

print(E6.tab <- table(E6_red$Question, E6_red$RespType))
print(E6.round <- round(prop.table(E6.tab, 1), 2))

print(ger.tab <- table(ger$Question, ger$RespType))
print(ger.round <- round(prop.table(ger.tab, 1), 2))

print(spa.tab <- table(spa$Question, spa$RespType))
print(spa.round <- round(prop.table(spa.tab, 1), 2))

print(chi.tab <- table(chi$Question, chi$RespType))
print(chi.round <- round(prop.table(chi.tab, 1), 2))




##------------------------------------------------------------------
## Main summary plot, all 6 experiments:
##------------------------------------------------------------------

## Settings for plot:

kheu_col <- 'steelblue'
kha_col <- 'goldenrod3'

# kheu_col <- 'wheat3'
# kha_col <- 'seagreen3'

xfac <- 0.2
btm_cex <- 1.45
btm_ypos <- 0.07
kheu_cex <- 1.5

## Make a plot of this:

quartz('', 19, 6)	# height = 6 for paper, height = 9 for presentation
par(mai = c(1.5, 2, 0.5, 0.25))	# for paper
plot(1, 1, type = 'n', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '',
	xlim = c(1, 23), ylim = c(0, 1), bty = 'n')
axis(side = 2, at = seq(0, 1, 0.25),
	labels = paste0(seq(0, 1, 0.25) * 100, '%'),
	lwd = 3, lwd.ticks = 3,
	cex.axis = 1.5, font = 2, las = 2)
mtext(side = 2, text = 'Percentage of response',
	font = 2, cex = 2.2, line = 5.25)	# paper
## Experiment 1: --------------
## E1, first bar, 'hard':
rect(xleft = 1 - xfac, xright = 2 - xfac,
	ybottom = 0, ytop = E1.round[1, 1], col = kheu_col, lwd = 2)
text(expression(italic('khu')), font = 2, las = 2, cex = kheu_cex,
	x = mean(c(1 - xfac, 2 - xfac)), y = E1.round[1, 1] / 2,
	srt = 90)
rect(xleft = 1 - xfac, xright = 2 - xfac,
	ybottom = E1.round[1, 1], ytop = 1.0, col = kha_col, lwd = 2)
text(expression(italic('khya')), font = 2, las = 2, cex = kheu_cex,
	x = mean(c(1 - xfac, 2 - xfac)), y = E1.round[1, 1] + (E1.round[1, 2] / 2),
	srt = 90)
text(x = mean(c(1 - xfac, 2 - xfac)),
	y = -btm_ypos, xpd = NA,
	labels = 'hard', font = 2, cex = btm_cex)
text('Softness', x = 2, y = -0.19, font = 2, cex = 2,
	xpd = NA)
text(paste0('N = ', sum(E1.tab) / 2), x = 2, y = -0.28, font = 2, cex = 1.62,
	xpd = NA)
## E1, second bar, 'smooth':
rect(xleft = 2 + xfac, xright = 3 + xfac,
	ybottom = 0, ytop = E1.round[2, 1], col = kheu_col, lwd = 2)
text(expression(italic('khu')), font = 2, las = 2, cex = kheu_cex,
	x = mean(c(2 + xfac, 3 + xfac)), y = E1.round[2, 1] / 2,
	srt = 90)
rect(xleft = 2 + xfac, xright = 3 + xfac,
	ybottom = E1.round[2, 1], ytop = 1.0, col = kha_col, lwd = 2)
text(expression(italic('khya')), font = 2, las = 2, cex = kheu_cex,
	x = mean(c(2 + xfac, 3 + xfac)), y = E1.round[2, 1] + (E1.round[2, 2] / 2),
	srt = 90)
text(x = mean(c(2 + xfac, 3 + xfac)),
	y = -btm_ypos, xpd = NA,
	labels = 'soft', font = 2, cex = btm_cex)
## Experiment 2: --------------
## E2, first bar, 'rough':
rect(xleft = 5 - xfac, xright = 6 - xfac,
	ybottom = 0, ytop = E2.round[1, 1], col = kheu_col, lwd = 2)
text(expression(italic('khu')), font = 2, las = 2, cex = kheu_cex,
	x = mean(c(5 - xfac, 6 - xfac)), y = E2.round[1, 1] / 2,
	srt = 90)
rect(xleft = 5 - xfac, xright = 6 - xfac,
	ybottom = E2.round[1, 1], ytop = 1.0, col = kha_col, lwd = 2)
text(expression(italic('khya')), font = 2, las = 2, cex = kheu_cex,
	x = mean(c(5 - xfac, 6 - xfac)), y = E2.round[1, 1] + (E2.round[1, 2] / 2),
	srt = 90)
text(x = mean(c(5 - xfac, 6 - xfac)),
	y = -btm_ypos, xpd = NA,
	labels = 'rough', font = 2, cex = btm_cex)
## E2, second bar, 'smooth':
rect(xleft = 6 + xfac, xright = 7 + xfac,
	ybottom = 0, ytop = E2.round[2, 1], col = kheu_col, lwd = 2)
text(expression(italic('khu')), font = 2, las = 2, cex = kheu_cex,
	x = mean(c(6 + xfac, 7 + xfac)), y = E2.round[2, 1] / 2,
	srt = 90)
rect(xleft = 6 + xfac, xright = 7 + xfac,
	ybottom = E2.round[2, 1], ytop = 1.0, col = kha_col, lwd = 2)
text(expression(italic('khya')), font = 2, las = 2, cex = kheu_cex,
	x = mean(c(6 + xfac, 7 + xfac)), y = E2.round[2, 1] + (E2.round[2, 2] / 2),
	srt = 90)
text(x = mean(c(6 + xfac, 7 + xfac)),
	y = -btm_ypos, xpd = NA,
	labels = 'smooth', font = 2, cex = btm_cex)
text('Smoothness', x = 6, y = -0.19, font = 2, cex = 2,
	xpd = NA)
text(paste0('N = ', sum(E2.tab) / 2), x = 6, y = -0.28, font = 2, cex = 1.62,
	xpd = NA)
## Experiment 3: --------------
## E3, first bar, 'male':
rect(xleft = 9 - xfac, xright = 10 - xfac,
	ybottom = 0, ytop = E3.round[1, 1], col = kheu_col, lwd = 2)
text(expression(italic('khu')), font = 2, las = 2, cex = kheu_cex,
	x = mean(c(9 - xfac, 10 - xfac)), y = E3.round[1, 1] / 2,
	srt = 90)
rect(xleft = 9 - xfac, xright = 10 - xfac,
	ybottom = E3.round[1, 1], ytop = 1.0, col = kha_col, lwd = 2)
text(expression(italic('khya')), font = 2, las = 2, cex = kheu_cex,
	x = mean(c(9 - xfac, 10 - xfac)), y = E3.round[1, 1] + (E3.round[1, 2] / 2),
	srt = 90)
text(x = mean(c(9 - xfac, 10 - xfac)),
	y = -btm_ypos, xpd = NA,
	labels = 'male', font = 2, cex = btm_cex)
## E3, second bar, 'female':
rect(xleft = 10 + xfac, xright = 11 + xfac,
	ybottom = 0, ytop = E3.round[2, 1], col = kheu_col, lwd = 2)
text(expression(italic('khu')), font = 2, las = 2, cex = kheu_cex,
	x = mean(c(10 + xfac, 11 + xfac)), y = E3.round[2, 1] / 2,
	srt = 90)
rect(xleft = 10 + xfac, xright = 11 + xfac,
	ybottom = E3.round[2, 1], ytop = 1.0, col = kha_col, lwd = 2)
text(expression(italic('khya')), font = 2, las = 2, cex = kheu_cex,
	x = mean(c(10 + xfac, 11 + xfac)), y = E3.round[2, 1] + (E3.round[2, 2] / 2),
	srt = 90)
text(x = mean(c(10 + xfac, 11 + xfac)),
	y = -btm_ypos, xpd = NA,
	labels = 'female', font = 2, cex = btm_cex)
text('Gender', x = 10, y = -0.19, font = 2, cex = 2,
	xpd = NA)
text(paste0('N = ', sum(E3.tab) / 2), x = 10, y = -0.28, font = 2, cex = 1.62,
	xpd = NA)
## Experiment 4: --------------
## E4, first bar, '30%':
rect(xleft = 13 - xfac, xright = 14 - xfac,
	ybottom = 0, ytop = E4.round[2, 1], col = kheu_col, lwd = 2)
text(expression(italic('khu')), font = 2, las = 2, cex = kheu_cex,
	x = mean(c(13 - xfac, 14 - xfac)), y = E4.round[2, 1] / 2,
	srt = 90)
rect(xleft = 13 - xfac, xright = 14 - xfac,
	ybottom = E4.round[2, 1], ytop = 1.0, col = kha_col, lwd = 2)
text(expression(italic('khya')), font = 2, las = 2, cex = kheu_cex,
	x = mean(c(13 - xfac, 14 - xfac)), y = E4.round[2, 1] + (E4.round[2, 2] / 2),
	srt = 90)
text(x = mean(c(13 - xfac, 14 - xfac)),
	y = -btm_ypos, xpd = NA,
	labels = '30%', font = 2, cex = btm_cex)
text('Alcohol content', x = 14, y = -0.19, font = 2, cex = 2,
	xpd = NA)
text(paste0('N = ', sum(E4.tab) / 2), x = 14, y = -0.28, font = 2, cex = 1.62,
	xpd = NA)
## E4, second bar, '15%':
rect(xleft = 14 + xfac, xright = 15 + xfac,
	ybottom = 0, ytop = E4.round[1, 1], col = kheu_col, lwd = 2)
text(expression(italic('khu')), font = 2, las = 2, cex = kheu_cex,
	x = mean(c(14 + xfac, 15 + xfac)), y = E4.round[1, 1] / 2,
	srt = 90)
rect(xleft = 14 + xfac, xright = 15 + xfac,
	ybottom = E4.round[1, 1], ytop = 1.0, col = kha_col, lwd = 2)
text(expression(italic('khya')), font = 2, las = 2, cex = kheu_cex,
	x = mean(c(14 + xfac, 15 + xfac)), y = E4.round[1, 1] + (E4.round[1, 2] / 2),
	srt = 90)
text(x = mean(c(14 + xfac, 15 + xfac)),
	y = -btm_ypos, xpd = NA,
	labels = '15%', font = 2, cex = btm_cex)
## Experiment 5: --------------
## E5, first bar, 'bitter':
rect(xleft = 17 - xfac, xright = 18 - xfac,
	ybottom = 0, ytop = E5.round[1, 1], col = kheu_col, lwd = 2)
text(expression(italic('khu')), font = 2, las = 2, cex = kheu_cex,
	x = mean(c(17 - xfac, 18 - xfac)), y = E5.round[1, 1] / 2,
	srt = 90)
rect(xleft = 17 - xfac, xright = 18 - xfac,
	ybottom = E5.round[1, 1], ytop = 1.0, col = kha_col, lwd = 2)
text(expression(italic('khya')), font = 2, las = 2, cex = kheu_cex,
	x = mean(c(17 - xfac, 18 - xfac)), y = E5.round[1, 1] + (E5.round[1, 2] / 2),
	srt = 90)
text(x = mean(c(17 - xfac, 18 - xfac)),
	y = -btm_ypos, xpd = NA,
	labels = 'bitter', font = 2, cex = btm_cex)
text('Taste', x = 18, y = -0.19, font = 2, cex = 2,
	xpd = NA)
text(paste0('N = ', sum(E5.tab) / 2), x = 18, y = -0.28, font = 2, cex = 1.62,
	xpd = NA)
## E5, second bar, 'sweet':
rect(xleft = 18 + xfac, xright = 19 + xfac,
	ybottom = 0, ytop = E5.round[2, 1], col = kheu_col, lwd = 2)
text(expression(italic('khu')), font = 2, las = 2, cex = kheu_cex,
	x = mean(c(18 + xfac, 19 + xfac)), y = E5.round[2, 1] / 2,
	srt = 90)
rect(xleft = 18 + xfac, xright = 19 + xfac,
	ybottom = E5.round[2, 1], ytop = 1.0, col = kha_col, lwd = 2)
text(expression(italic('khya')), font = 2, las = 2, cex = kheu_cex,
	x = mean(c(18 + xfac, 19 + xfac)), y = E5.round[2, 1] + (E5.round[2, 2] / 2),
	srt = 90)
text(x = mean(c(18 + xfac, 19 + xfac)),
	y = -btm_ypos, xpd = NA,
	labels = 'sweet', font = 2, cex = btm_cex)
## Experiment 6: --------------
## E6, first bar, 'unpleasant':
rect(xleft = 21 - xfac, xright = 22 - xfac,
	ybottom = 0, ytop = E6.round[2, 1], col = kheu_col, lwd = 2)
text(expression(italic('khu')), font = 2, las = 2, cex = kheu_cex,
	x = mean(c(21 - xfac, 22 - xfac)), y = E6.round[2, 1] / 2,
	srt = 90)
rect(xleft = 21 - xfac, xright = 22 - xfac,
	ybottom = E6.round[2, 1], ytop = 1.0, col = kha_col, lwd = 2)
text(expression(italic('khya')), font = 2, las = 2, cex = kheu_cex,
	x = mean(c(21 - xfac, 22 - xfac)), y = E6.round[2, 1] + (E6.round[2, 2] / 2),
	srt = 90)
text(x = mean(c(21 - xfac, 22 - xfac)),
	y = -btm_ypos, xpd = NA,
	labels = '(-)', font = 2, cex = btm_cex)
text('Pleasantness', x = 22, y = -0.19, font = 2, cex = 2,
	xpd = NA)
text(paste0('N = ', sum(E6.tab) / 2), x = 22, y = -0.28, font = 2, cex = 1.62,
	xpd = NA)
## E6, second bar, 'pleasant':
rect(xleft = 22 + xfac, xright = 23 + xfac,
	ybottom = 0, ytop = E6.round[1, 1], col = kheu_col, lwd = 2)
text(expression(italic('khu')), font = 2, las = 2, cex = kheu_cex,
	x = mean(c(22 + xfac, 23 + xfac)), y = E6.round[1, 1] / 2,
	srt = 90)
rect(xleft = 22 + xfac, xright = 23 + xfac,
	ybottom = E6.round[1, 1], ytop = 1.0, col = kha_col, lwd = 2)
text(expression(italic('khya')), font = 2, las = 2, cex = kheu_cex,
	x = mean(c(22 + xfac, 23 + xfac)), y = E6.round[1, 1] + (E6.round[1, 2] / 2),
	srt = 90)
text(x = mean(c(22 + xfac, 23 + xfac)),
	y = -btm_ypos, xpd = NA,
	labels = '(+)', font = 2, cex = btm_cex)
quartz.save('/Users/winterb/Research/harkness_soju/analysis/figure1.tiff', type = 'tiff', dpi = 300)



##------------------------------------------------------------------
## Main summary plot, cross-cultural data:
##------------------------------------------------------------------

## Settings for plot:

kheu_col <- 'steelblue'
kha_col <- 'goldenrod3'

# kheu_col <- 'wheat3'
# kha_col <- 'seagreen3'

xfac <- 0.2
btm_cex <- 1.45
btm_ypos <- 0.07
kheu_cex <- 1.5

## Make a plot of this:

quartz('', 10, 6)	# height = 6 for paper, height = 9 for presentation
par(mai = c(1.5, 2, 0.5, 0.25))	# for paper
plot(1, 1, type = 'n', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '',
	xlim = c(1, 13), ylim = c(0, 1), bty = 'n')
axis(side = 2, at = seq(0, 1, 0.25),
	labels = paste0(seq(0, 1, 0.25) * 100, '%'),
	lwd = 3, lwd.ticks = 3,
	cex.axis = 1.5, font = 2, las = 2)
mtext(side = 2, text = 'Percentage of response',
	font = 2, cex = 2.2, line = 5.25)	# paper
## Experiment 1: --------------
## E1, first bar, 'hard':
rect(xleft = 1 - xfac, xright = 2 - xfac,
	ybottom = 0, ytop = spa.round[1, 1], col = kheu_col, lwd = 2)
text(expression(italic('khu')), font = 2, las = 2, cex = kheu_cex,
	x = mean(c(1 - xfac, 2 - xfac)), y = spa.round[1, 1] / 2,
	srt = 90)
rect(xleft = 1 - xfac, xright = 2 - xfac,
	ybottom = spa.round[1, 1], ytop = 1.0, col = kha_col, lwd = 2)
text(expression(italic('khya')), font = 2, las = 2, cex = kheu_cex,
	x = mean(c(1 - xfac, 2 - xfac)), y = spa.round[1, 1] + (spa.round[1, 2] / 2),
	srt = 90)
text(x = mean(c(1 - xfac, 2 - xfac)),
	y = -btm_ypos, xpd = NA,
	labels = 'hard', font = 2, cex = btm_cex)
text('Spanish', x = 2, y = -0.19, font = 2, cex = 2,
	xpd = NA)
text(paste0('N = ', sum(spa.tab) / 2), x = 2, y = -0.28, font = 2, cex = 1.62,
	xpd = NA)
## E1, second bar, 'smooth':
rect(xleft = 2 + xfac, xright = 3 + xfac,
	ybottom = 0, ytop = spa.round[2, 1], col = kheu_col, lwd = 2)
text(expression(italic('khu')), font = 2, las = 2, cex = kheu_cex,
	x = mean(c(2 + xfac, 3 + xfac)), y = spa.round[2, 1] / 2,
	srt = 90)
rect(xleft = 2 + xfac, xright = 3 + xfac,
	ybottom = spa.round[2, 1], ytop = 1.0, col = kha_col, lwd = 2)
text(expression(italic('khya')), font = 2, las = 2, cex = kheu_cex,
	x = mean(c(2 + xfac, 3 + xfac)), y = spa.round[2, 1] + (spa.round[2, 2] / 2),
	srt = 90)
text(x = mean(c(2 + xfac, 3 + xfac)),
	y = -btm_ypos, xpd = NA,
	labels = 'soft', font = 2, cex = btm_cex)
## Experiment 2: --------------
## E2, first bar, 'rough':
rect(xleft = 5 - xfac, xright = 6 - xfac,
	ybottom = 0, ytop = ger.round[1, 1], col = kheu_col, lwd = 2)
text(expression(italic('khu')), font = 2, las = 2, cex = kheu_cex,
	x = mean(c(5 - xfac, 6 - xfac)), y = ger.round[1, 1] / 2,
	srt = 90)
rect(xleft = 5 - xfac, xright = 6 - xfac,
	ybottom = ger.round[1, 1], ytop = 1.0, col = kha_col, lwd = 2)
text(expression(italic('khya')), font = 2, las = 2, cex = kheu_cex,
	x = mean(c(5 - xfac, 6 - xfac)), y = ger.round[1, 1] + (ger.round[1, 2] / 2),
	srt = 90)
text(x = mean(c(5 - xfac, 6 - xfac)),
	y = -btm_ypos, xpd = NA,
	labels = 'hard', font = 2, cex = btm_cex)
## E2, second bar, 'smooth':
rect(xleft = 6 + xfac, xright = 7 + xfac,
	ybottom = 0, ytop = ger.round[2, 1], col = kheu_col, lwd = 2)
text(expression(italic('khu')), font = 2, las = 2, cex = kheu_cex,
	x = mean(c(6 + xfac, 7 + xfac)), y = ger.round[2, 1] / 2,
	srt = 90)
rect(xleft = 6 + xfac, xright = 7 + xfac,
	ybottom = ger.round[2, 1], ytop = 1.0, col = kha_col, lwd = 2)
text(expression(italic('khya')), font = 2, las = 2, cex = kheu_cex,
	x = mean(c(6 + xfac, 7 + xfac)), y = ger.round[2, 1] + (ger.round[2, 2] / 2),
	srt = 90)
text(x = mean(c(6 + xfac, 7 + xfac)),
	y = -btm_ypos, xpd = NA,
	labels = 'soft', font = 2, cex = btm_cex)
text('German', x = 6, y = -0.19, font = 2, cex = 2,
	xpd = NA)
text(paste0('N = ', sum(ger.tab) / 2), x = 6, y = -0.28, font = 2, cex = 1.62,
	xpd = NA)
## Experiment 3: --------------
## E3, first bar, 'male':
rect(xleft = 9 - xfac, xright = 10 - xfac,
	ybottom = 0, ytop = chi.round[1, 1], col = kheu_col, lwd = 2)
text(expression(italic('khu')), font = 2, las = 2, cex = kheu_cex,
	x = mean(c(9 - xfac, 10 - xfac)), y = chi.round[1, 1] / 2,
	srt = 90)
rect(xleft = 9 - xfac, xright = 10 - xfac,
	ybottom = chi.round[1, 1], ytop = 1.0, col = kha_col, lwd = 2)
text(expression(italic('khya')), font = 2, las = 2, cex = kheu_cex,
	x = mean(c(9 - xfac, 10 - xfac)), y = chi.round[1, 1] + (chi.round[1, 2] / 2),
	srt = 90)
text(x = mean(c(9 - xfac, 10 - xfac)),
	y = -btm_ypos, xpd = NA,
	labels = 'hard', font = 2, cex = btm_cex)
## E3, second bar, 'female':
rect(xleft = 10 + xfac, xright = 11 + xfac,
	ybottom = 0, ytop = chi.round[2, 1], col = kheu_col, lwd = 2)
text(expression(italic('khu')), font = 2, las = 2, cex = kheu_cex,
	x = mean(c(10 + xfac, 11 + xfac)), y = chi.round[2, 1] / 2,
	srt = 90)
rect(xleft = 10 + xfac, xright = 11 + xfac,
	ybottom = chi.round[2, 1], ytop = 1.0, col = kha_col, lwd = 2)
text(expression(italic('khya')), font = 2, las = 2, cex = kheu_cex,
	x = mean(c(10 + xfac, 11 + xfac)), y = chi.round[2, 1] + (chi.round[2, 2] / 2),
	srt = 90)
text(x = mean(c(10 + xfac, 11 + xfac)),
	y = -btm_ypos, xpd = NA,
	labels = 'soft', font = 2, cex = btm_cex)
text('Chinese', x = 10, y = -0.19, font = 2, cex = 2,
	xpd = NA)
text(paste0('N = ', sum(chi.tab) / 2), x = 10, y = -0.28, font = 2, cex = 1.62,
	xpd = NA)
quartz.save('/Users/winterb/Research/harkness_soju/analysis/figure2.tiff', type = 'tiff', dpi = 300)










##------------------------------------------------------------------
## Inferential statistics of reduced dataframe (even less Korean exposure):
##------------------------------------------------------------------

## Exclude those participants that have Korean friends etc.

mydfs <- c('E1', 'E2', 'E3', 'E4', 'E5', 'E6')
for (i in 1:length(mydfs)) {
	this_df <- get(paste0(mydfs[i], '_red'))
	
	# Make categorical variables into factors:
	
	this_df <- filter(this_df,
		SojuQuestion %in% c('No', 'Maybe'),
		KoreanFriends %in% c('No', 'Maybe'))
	
	assign(paste0(mydfs[i], '_less'), this_df)
	}

## How many had to be excluded?

(nrow(E1_red) - nrow(E1_less)) / 2
1 - (nrow(E1_less) / nrow(E1_red))

(nrow(E2_red) - nrow(E2_less)) / 2
1 - (nrow(E2_less) / nrow(E2_red))

(nrow(E3_red) - nrow(E3_less)) / 2
1 - (nrow(E3_less) / nrow(E3_red))

(nrow(E4_red) - nrow(E4_less)) / 2
1 - (nrow(E4_less) / nrow(E4_red))

(nrow(E5_red) - nrow(E5_less)) / 2
1 - (nrow(E5_less) / nrow(E5_red))

(nrow(E6_red) - nrow(E6_less)) / 2
1 - (nrow(E6_less) / nrow(E6_red))

## Create models:

summary(E1.mdl <- glmer(myFormula, data = E1_less, family = 'binomial'))
summary(E2.mdl <- glmer(myFormula, data = E2_less, family = 'binomial'))
summary(E3.mdl <- glmer(myFormula, data = E3_less, family = 'binomial'))
summary(E4.mdl <- glmer(myFormula, data = E4_less, family = 'binomial'))
summary(E5.mdl <- glmer(myFormula, data = E5_less, family = 'binomial'))
summary(E6.mdl <- glmer(myFormula, data = E6_less, family = 'binomial'))

## Create models for R-squared comparison:

summary(E1.mdl_red <- glmer(myFormula_red, data = E1_less, family = 'binomial'))
summary(E2.mdl_red <- glmer(myFormula_red, data = E2_less, family = 'binomial'))
summary(E3.mdl_red <- glmer(myFormula_red, data = E3_less, family = 'binomial'))
summary(E4.mdl_red <- glmer(myFormula_red, data = E4_less, family = 'binomial'))
summary(E5.mdl_red <- glmer(myFormula_red, data = E5_less, family = 'binomial'))
summary(E6.mdl_red <- glmer(myFormula_red, data = E6_less, family = 'binomial'))

## R-squared:

r.squaredGLMM(E1.mdl)	# convergence issue
r.squaredGLMM(E2.mdl)
r.squaredGLMM(E3.mdl)
r.squaredGLMM(E4.mdl)
r.squaredGLMM(E5.mdl)
r.squaredGLMM(E6.mdl)

## R-squared for models without question effects:

r.squaredGLMM(E1.mdl)[1] - r.squaredGLMM(E1.mdl_red)[1]
r.squaredGLMM(E2.mdl)[1] - r.squaredGLMM(E2.mdl_red)[1]
r.squaredGLMM(E3.mdl)[1] - r.squaredGLMM(E3.mdl_red)[1]
r.squaredGLMM(E4.mdl)[1] - r.squaredGLMM(E4.mdl_red)[1]
r.squaredGLMM(E5.mdl)[1] - r.squaredGLMM(E5.mdl_red)[1]
r.squaredGLMM(E6.mdl)[1] - r.squaredGLMM(E6.mdl_red)[1]

## Create likelihood ratio tests:

E1.afex <- mixed(myFormula, E1_less, family = 'binomial', method = 'LRT')
E2.afex <- mixed(myFormula, E2_less, family = 'binomial', method = 'LRT')
E3.afex <- mixed(myFormula, E3_less, family = 'binomial', method = 'LRT')
E4.afex <- mixed(myFormula, E4_less, family = 'binomial', method = 'LRT')
E5.afex <- mixed(myFormula, E5_less, family = 'binomial', method = 'LRT')
E6.afex <- mixed(myFormula, E6_less, family = 'binomial', method = 'LRT')

## Look at this:

E1.afex$anova_table
E2.afex$anova_table
E3.afex$anova_table
E4.afex$anova_table
E5.afex$anova_table
E6.afex$anova_table

## Results stay the same



##------------------------------------------------------------------
## Meta-analysis:
##------------------------------------------------------------------

## Combining everything:

E1_all <- select(E1, ID, Question, RespType) %>%
	mutate(Question = as.character(Question))
E2_all <- select(E2, ID, Question, RespType) %>%
	mutate(Question = as.character(Question))
E3_all <- select(E3, ID, Question, RespType) %>%
	mutate(Question = as.character(Question))
E4_all <- select(E4, ID, Question, RespType) %>%
	mutate(Question = as.character(Question))
E_all <- bind_rows(E1_all,
	E2_all, E3_all, E4_all)
E_all$Experiment <- c(rep('softness', nrow(E1_all)),
	rep('roughness', nrow(E2_all)),
	rep('gender', nrow(E3_all)),
	rep('alcohol', nrow(E3_all)))
E_all$Condition <- 'soft'
E_all[E_all$Question %in% c('hard', 'rough', 'male', '30%'), ]$Condition <- 'hard'

## Perform meta-analysis:

summary(E_all.mdl <- glmer(RespType ~ Condition * Experiment + (1|ID),
	data = E_all, family = 'binomial'))
E_all.afex <- mixed(RespType ~ Condition * Experiment + (1|ID),
	data = E_all, family = 'binomial', method = 'LRT')




