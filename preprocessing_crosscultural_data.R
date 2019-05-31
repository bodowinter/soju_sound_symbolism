## Bodo Winter
## May 1, 2019
## Preprocessing cross-cultural data

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

mycols <- colnames(read.csv('qualtrics_multilingual_columns.csv', header = TRUE))

## Load in experiment files:

ger <- read.csv('Harkness_German.csv', header = FALSE,
	stringsAsFactors = FALSE, skip = 2)
ger_mturk <- read.csv('Harkness_German__MTurk.csv', header = FALSE,
	stringsAsFactors = FALSE, skip = 2)
spa <- read.csv('Harkness_Spanish.csv', header = FALSE,
	stringsAsFactors = FALSE, skip = 2)
spa_mturk <- read.csv('Harkness_Spanish__MTurk.csv', header = FALSE,
	stringsAsFactors = FALSE, skip = 2)
chi <- read.csv('Harkness_Chinese.csv', header = FALSE,
	stringsAsFactors = FALSE, skip = 2)

## Bind rows together:

ger <- rbind(ger, ger_mturk)
spa <- rbind(spa, spa_mturk)

## Name columns:

colnames(ger) <- mycols
colnames(spa) <- mycols
colnames(chi) <- mycols

## Get rid of the NA column:

ger <- ger[, -ncol(ger)]
spa <- spa[, -ncol(spa)]
chi <- chi[, -ncol(chi)]



##------------------------------------------------------------------
## Data carpentry:
##------------------------------------------------------------------

## Vector of cross-linguistic responses:

response_names <- c('Erste Aufnahme', 'Zweite Aufnahme',
	'Primero', 'Segundo',
	'第一个:', '第二个:')

## Create function for extracting responses:

extract_resp <- function(xdata) {
	## Subset with relevant columns that contain 'first' and 'second':
	
	xsubset <- select(xdata, Male_KheuKhya_weich:Male_KheuKhya_hart.4)
	
	# Results vectors:
	
	xres <- c()
	xcond <- c()
	
	# Loop through rows to extract 'first' and 'second' responses:
	
	for (i in 1:nrow(xsubset)) {
		this_list <- unlist(xsubset[i, ])
		ids <- which(this_list %in% response_names)

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
			separate(FirstCond, into = c('FirstGender', 'FirstCond', 'Question')) %>%
			separate(SecondCond, into = c('SecondGender', 'SecondCond', 'Question'))		

	# Append info from main frame:

	myconds$Gender <- xdata$Gender
	myconds$Age <- xdata$Age
	myconds$Native <- xdata$Native
	myconds$ID <- xdata$ResponseID
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

ger <- extract_resp(ger)
spa <- extract_resp(spa)
chi <- extract_resp(chi)


## Function for making into long form:

make_long <- function(xdata) {
	# Subset that is doubled (participant-level info):
	
	this_subset <- select(xdata,
		ID:KoreanFriends, Question, Qualitative)

	# Double:

	xlong <- rbind(this_subset, this_subset)
	
	# Extract values:
	
	values1 <- select(xdata, FirstCond, FirstGender, FirstResp)
	values2 <- select(xdata, SecondCond, SecondGender, SecondResp)
	
	# Rename for appending:
	
	values1 <- rename(values1,
		Condition = FirstCond, StimGender = FirstGender, RespType = FirstResp)
	values2 <- rename(values2,
		Condition = SecondCond, StimGender = SecondGender, RespType = SecondResp)
		
	# Append, arrange by ID and make into tibble:
	
	xlong <- cbind(xlong, rbind(values1, values2)) %>%
		arrange(ID) %>% as_tibble()
		
	return(xlong)
	}

## Do this for everyone (making the long format):

ger <- make_long(ger)
spa <- make_long(spa)
chi <- make_long(chi)

## Get rid of NAs for the reponses:

ger <- filter(ger, !is.na(RespType))
spa <- filter(spa, !is.na(RespType))
chi <- filter(chi, !is.na(RespType))

## Collapse 'weic' and 'weich':

ger <- mutate(ger, Question = ifelse(Question == 'weic', 'weich', Question))
spa <- mutate(spa, Question = ifelse(Question == 'weic', 'weich', Question))
chi <- mutate(chi, Question = ifelse(Question == 'weic', 'weich', Question))

## Function for creating a response type column:

create_resp <- function(xdata) {
	# Extract condition and split by 'K' (separate Khya & Kheu):
	
	written_resp <- strsplit(xdata$Condition, split = 'K')
	
	# Vector for which one (first or second) was chosen):
	
	which_one <- as.numeric(as.factor(xdata$RespType)) + 1
	
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

ger <- create_resp(ger)
spa <- create_resp(spa)
chi <- create_resp(chi)


##------------------------------------------------------------------
## Write:
##------------------------------------------------------------------

## Write to table:

setwd(file.path(mainPath, 'processed_data/'))
write_csv(ger, 'german_processed.csv')
write_csv(spa, 'spanish_processed.csv')
write_csv(chi, 'chinese_processed.csv')


