## Bodo Winter
## June 7, 2017
## Preprocessing data

##------------------------------------------------------------------
## Load stuff:
##------------------------------------------------------------------

## Load in libraries:

library(tidyverse)
library(stringr)
library(lme4)

## Set working directory:

mainPath <- '/Users/winterb/Research/harkness_soju/analysis'
setwd(file.path(mainPath, 'raw_data/'))

## Load in column names:

mycols_long <- colnames(read.csv('qualtrics_long_column_names.csv', header = TRUE))
mycols_short <- colnames(read.csv('qualtrics_short_column_names.csv', header = TRUE))
mycols_soju_question <- colnames(read.csv('qualtrics_soju_content_question.csv', header = TRUE))
mycols_new <- colnames(read.csv('qualtrics_soju_new_column_names.csv', header = TRUE))

## Load in experiment files:

hard <- read.csv('qualtrics_E1_hard.csv', header = FALSE,
	stringsAsFactors = FALSE)
soft <- read.csv('qualtrics_E1_soft.csv', header = FALSE,
	stringsAsFactors = FALSE)
rough <- read.csv('qualtrics_E2_rough.csv', header = FALSE,
	stringsAsFactors = FALSE)
smooth <- read.csv('qualtrics_E2_smooth.csv', header = FALSE,
	stringsAsFactors = FALSE)
male <- read.csv('qualtrics_E3_male.csv', header = FALSE,
	stringsAsFactors = FALSE)
female <- read.csv('qualtrics_E3_female.csv', header = F,
	stringsAsFactors = FALSE)
percent15 <- read.csv('qualtrics_E4_15percent.csv', header = FALSE,
	stringsAsFactors = FALSE)
percent30 <- read.csv('qualtrics_E4_30percent.csv', header = FALSE,
	stringsAsFactors = FALSE)
sweet <- read.csv('Harkness_SWEET.csv', header = FALSE,
	stringsAsFactors = FALSE)
bitter <- read.csv('Harkness_BITTER.csv', header = FALSE,
	stringsAsFactors = FALSE)
pleasant <- read.csv('Harkness_PLEASANT.csv', header = FALSE,
	stringsAsFactors = FALSE)
unpleasant <- read.csv('Harkness_UNPLEASANT.csv', header = FALSE,
	stringsAsFactors = FALSE)

## Name columns:

colnames(hard) <- mycols_long
colnames(soft) <- mycols_long
colnames(rough) <- mycols_short		# earlier version of experiment
colnames(smooth) <- mycols_long
colnames(male) <- mycols_long
colnames(female) <- mycols_long
colnames(percent15) <- mycols_soju_question
colnames(percent30) <- mycols_soju_question
colnames(sweet) <- mycols_new
colnames(bitter) <- mycols_new
colnames(pleasant) <- mycols_new
colnames(unpleasant) <- mycols_new



##------------------------------------------------------------------
## Data carpentry:
##------------------------------------------------------------------

## Create function for extracting responses:

extract_resp <- function(xdata) {
	## Subset with relevant columns that contain 'first' and 'second':
	
	xsubset <- select(xdata, Male_KheuKhya:Male_KhyaKheu.3)
	
	# Results vectors:
	
	xres <- c()
	xcond <- c()
	
	# Loop through rows to extract 'first' and 'second' responses:
	
	for (i in 1:nrow(xsubset)) {
		this_list <- unlist(xsubset[i, ])
		ids <- which(this_list %in% c('First', 'Second'))

		if (length(ids) == 0) {
			this_names <- c(NA, NA)
			this_list <- c(NA, NA)
			} else {
				this_list <- this_list[ids]
				this_names <- names(this_list)
				this_list <- unname(this_list)
			}
		xres <- rbind(xres, this_list)
		xcond <- rbind(xcond, this_names)
		}

	# Put condition and response in data frame:

	myconds <- as_tibble(cbind(xcond, xres))
	
	# Clean up:
	
	myconds <- rename(myconds, FirstCond = V1, SecondCond = V2,
		FirstResp = V3, SecondResp = V4) %>% 
			mutate(FirstCond = str_replace(FirstCond, '\\.[0-9]', ''),
				SecondCond = str_replace(SecondCond, '\\.[0-9]', '')) %>%
			separate(FirstCond, into = c('FirstGender', 'FirstCond')) %>%
			separate(SecondCond, into = c('SecondGender', 'SecondCond'))		

	# Append info from main frame:

	myconds$Gender <- xdata$Gender
	myconds$Age <- xdata$Age
	myconds$Native <- xdata$Native
	myconds$ID <- xdata$V1
	myconds$KoreanVisit <- xdata$KoreanVisit
	myconds$KoreanDrama <- xdata$KoreanDrama
	myconds$KoreanLanguage <- xdata$KoreanLanguage
	myconds$SojuQuestion <- xdata$SojuQuestion
	myconds$KoreanRestaurant <- xdata$KoreanRestaurant
	myconds$KoreanFriends <- xdata$KoreanFriends
	
	# Append qualitative data — but only if it exists:
	
	if (any(colnames(xdata) == 'Qualitative.question')) {
		myconds$Qualitative <- xdata$Qualitative.question
		}

	if (any(colnames(xdata) == 'Qualitative')) {
		myconds$Qualitative <- xdata$Qualitative
		}

	anyqual <- any(colnames(xdata) == 'Qualitative')
	anyqual <- !(anyqual | any(colnames(xdata) == 'Qualitative.question'))
	if (anyqual) {
		myconds$Qualitative <- NA
		}
	
	# Reshuffle columns in better order:
	
	myconds <- select(myconds,
		ID, Gender, Age, Native,
		KoreanVisit:KoreanFriends, FirstGender:SecondResp, Qualitative)
	
	# Return to outside function:

	return(myconds)
	}

## Extract the relevant datasets:

hard <- extract_resp(hard)
soft <- extract_resp(soft)
rough <- extract_resp(rough)
smooth <- extract_resp(smooth)
male <- extract_resp(male)
female <- extract_resp(female)
percent15 <- extract_resp(percent15)
percent30 <- extract_resp(percent30)
sweet <- extract_resp(sweet)
bitter <- extract_resp(bitter)
pleasant <- extract_resp(pleasant)
unpleasant <- extract_resp(unpleasant)


## Function for making into long form:

make_long <- function(xdata) {
	# Subset that is doubled (participant-level info):
	
	this_subset <- select(xdata,
		ID:KoreanFriends, Qualitative)

	# Double:

	xlong <- rbind(this_subset, this_subset)
	
	# Extract values:
	
	values1 <- select(xdata, FirstCond, FirstGender, FirstResp)
	values2 <- select(xdata, SecondCond, SecondGender, SecondResp)
	
	# Rename for appending:
	
	values1 <- rename(values1,
		Condition = FirstCond, StimGender = FirstGender, Resp = FirstResp)
	values2 <- rename(values2,
		Condition = SecondCond, StimGender = SecondGender, Resp = SecondResp)
		
	# Append, arrange by ID and make into tibble:
	
	xlong <- cbind(xlong, rbind(values1, values2)) %>%
		arrange(ID) %>% as_tibble()
		
	return(xlong)
	}

## Do this for everyone (making the long format):

hard <- make_long(hard)
soft <- make_long(soft)
rough <- make_long(rough)
smooth <- make_long(smooth)
male <- make_long(male)
female <- make_long(female)
percent15 <- make_long(percent15)
percent30 <- make_long(percent30)
sweet <- make_long(sweet)
bitter <- make_long(bitter)
pleasant <- make_long(pleasant)
unpleasant <- make_long(unpleasant)

## Function for creating a response type column:

create_resp <- function(xdata) {
	# Extract condition and split by 'K' (separate Khya & Kheu):
	
	written_resp <- strsplit(xdata$Condition, split = 'K')
	
	# Vector for which one (first or second) was chosen):
	
	which_one <- as.numeric(as.factor(xdata$Resp)) + 1
	
	# Loop through and pick the one that was chosen:
	
	all_resp <- c()
	for (i in 1:length(written_resp)) {
		all_resp <- c(all_resp,
			str_c('K', written_resp[[i]][which_one[i]]))
	}
	
	# Append to data frame:
	
	xdata$RespType <- all_resp	
	
	return(xdata)
	}

## Do this for everyone:

hard <- create_resp(hard)
soft <- create_resp(soft)
rough <- create_resp(rough)
smooth <- create_resp(smooth)
male <- create_resp(male)
female <- create_resp(female)
percent15 <- create_resp(percent15)
percent30 <- create_resp(percent30)
sweet <- create_resp(sweet)
bitter <- create_resp(bitter)
pleasant <- create_resp(pleasant)
unpleasant <- create_resp(unpleasant)

## Append what question was asked: harder? softer? etc. :

hard$Question <- 'hard'
soft$Question <- 'soft'
rough$Question <- 'rough'
smooth$Question <- 'smooth'
male$Question <- 'male'
female$Question <- 'female'
percent15$Question <- '15%'
percent30$Question <- '30%'
sweet$Question <- 'sweet'
bitter$Question <- 'bitter'
pleasant$Question <- 'pleasant'
unpleasant$Question <- 'unpleasant'

## Put them into experiment files:

E1 <- rbind(hard, soft)
E2 <- rbind(rough, smooth)
E3 <- rbind(male, female)
E4 <- rbind(percent15, percent30)
E5 <- rbind(sweet, bitter)
E6 <- rbind(pleasant, unpleasant)


##------------------------------------------------------------------
## Write:
##------------------------------------------------------------------

## Write to table:

setwd(file.path(mainPath, 'processed_data/'))
write_csv(E1, 'E1_hardness.csv')
write_csv(E2, 'E2_roughness.csv')
write_csv(E3, 'E3_gender.csv')
write_csv(E4, 'E4_soju_content.csv')
write_csv(E5, 'E5_taste.csv')
write_csv(E6, 'E6_pleasant.csv')


