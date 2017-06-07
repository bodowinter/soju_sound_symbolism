## Bodo Winter
## June 7, 2017
## Function for loading in data

## Load in libraries:

library(tidyverse)
library(stringr)
library(lme4)

## Set working directory:

mainPath <- '/Users/winterb/Research/harkness_soju/analysis'
setwd(file.path(mainPath, 'raw_data/'))

## Load in column names:

mycols_long <- colnames(read.csv('qualtrics_long_column_names.csv', header = T))
mycols_short <- colnames(read.csv('qualtrics_short_column_names.csv', header = T))

## Load in experiment files:

hard <- read.csv('qualtrics_E1_hard.csv', header = F,
	stringsAsFactors = F)
soft <- read.csv('qualtrics_E1_soft.csv', header = F,
	stringsAsFactors = F)
rough <- read.csv('qualtrics_E2_rough.csv', header = F,
	stringsAsFactors = F)
smooth <- read.csv('qualtrics_E2_smooth.csv', header = F,
	stringsAsFactors = F)
male <- read.csv('qualtrics_E3_male.csv', header = F,
	stringsAsFactors = F)
female <- read.csv('qualtrics_E3_female.csv', header = F,
	stringsAsFactors = F)

## Name columns:

colnames(hard) <- mycols_long
colnames(soft) <- mycols_long
colnames(rough) <- mycols_short		# earlier version of experiment
colnames(smooth) <- mycols_long
colnames(male) <- mycols_long
colnames(female) <- mycols_long

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
	
	# Reshuffle columns in better order:
	
	if (any(colnames(xdata) == 'Qualitative.question')) {
		myconds <- select(myconds,
			ID, Gender, Native,
			KoreanVisit:KoreanFriends, FirstGender:SecondResp, Qualitative)
		} else {
			myconds <- select(myconds,
				ID, Gender, Native, KoreanVisit:KoreanFriends,
				FirstGender:SecondResp)	
			}
	
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

## Append empty qualitative question for 'rough' pilot:

rough$Qualitative <- NA

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

## Append what question was asked: harder? softer? etc. :

hard$Question <- 'hard'
soft$Question <- 'soft'
rough$Question <- 'rough'
smooth$Question <- 'smooth'
male$Question <- 'male'
female$Question <- 'female'

## Put them into experiment files:

E1 <- rbind(hard, soft)
E2 <- rbind(rough, smooth)
E3 <- rbind(male, female)

## Write to table:

setwd(file.path(mainPath, 'processed_data/'))
write_csv(E1, 'E1_hardness.csv')
write_csv(E2, 'E2_roughness.csv')
write_csv(E3, 'E3_gender.csv')


