## -----------------------------------------------------------------------------
#'  1_import_trends_data.R
#'
#' by: Chris Holiday
#'
#'  A master script which runs data cleaning and analysis files. Resulting
#'  output takes the form of a scr
#'
#'
## -----------------------------------------------------------------------------
## Managing dependencies
## -----------------------------------------------------------------------------
##rm(list = ls())

library(data.table)
library(stringr)
library(purrr)
library(readr)
library(gtrendsR)
library(reshape2)


## -----------------------------------------------------------------------------
## Define Keywords to use
## -----------------------------------------------------------------------------

original_list <- c("Google Classroom", "Khan Academy", "Kahoot", "Seesaw", "Schoology", "Class Dojo",
  "D2L", "Nearpod", "Edmodo", "Flocabulary", "Starfall", "GoNoodle", "ClassDojo", "Flipgrid", "Socrative",
  "Online school", "Online classes", "Home school", "Online class", "Math game", 
  "Distance learning", "Math worksheets", "Online math", "Math problem", "Online reading",
  "Educational game", "Education game", "Online lessons", "Free preschool worksheets",
  "Educational apps", "Educational games", "Vocabulary game", "School worksheets", 
  "Reading game", "Online tutoring", "Virtual education", "Online lesson", 
  "Virtual school", "Educational videos", "Educational app", "Free school worksheets",
  "Education app", "Online science", "Online social studies", "Education games")

## Branded resources ##
top_branded <- c("Google Classroom", "Khan Academy", "Kahoot", "Seesaw", "Schoology", 
	"Class Dojo", "Flipgrid", "D2L", "Nearpod", "Edmodo")

## General Resources ##
top_general <- c("Online school", "Online classes", "Home school", "Online class", "Math game", 
  "Distance learning", "Math worksheets", "Online math", "Math problem", "Online reading",
  "Educational game", "Education game")



## -----------------------------------------------------------------------------
## National Levels US
## -----------------------------------------------------------------------------

## Use google classroom as basis needs to be removed, split into 4 groups##
original_list_to_search <- original_list[-1]
lists_to_search <- split(original_list_to_search, ceiling(seq_along(original_list_to_search)/4))
n_lists <- unique(ceiling(seq_along(original_list)/4))

## Do a for loop and combine data with normalization parameters ##
df_list = list()

for (i in n_lists){

	search_list <- append(unlist(lists_to_search[i]), "Google Classroom")
	trends_search <- gtrends(search_list, 
	                  gprop = "web",
	                  geo = "",
	                  time = "2015-08-01 2020-08-01",
	                  low_search_volume = T)
	## Convert low volume searches into 0 ##
	dataframe <- trends_search$interest_over_time
	dataframe$hits[dataframe$hits == "<1"] <- 0
	## Cast DF and use google classroom as normalization vector ##
	dataframe$hits <- as.numeric(dataframe$hits)
	dataframe_wide <- dcast(dataframe, date + geo ~ keyword, value.var = "hits")
	names(dataframe_wide)[names(dataframe_wide) == "Google Classroom"] <- paste0("GC_", i)
	## Assign DF and sleep ##
	df_list[[i]] <- dataframe_wide
	Sys.sleep(60)

}

## Reduce List together  ##
US_data_all <- Reduce(function(x,y) merge(x,y, by = c("geo", "date"), all = TRUE), df_list)


## Normalize Columns of the rest of the dataset ## Dont need to do this
for (i in names(US_data_all)[str_detect(names(US_data_all), "$GC_")]){
	if (i > 1) {
		for (j in 1:4){
			term <- lists_to_search$i[j]
			GC_col <- paste0("GC_1", i)
			US_data_all$term <- US_data_all$GC_1 / US_data_all$GC_col * US_data_all$term

		}
	}
}

## Export ##
write.csv(US_data_all, "/home/choliday/ECO2607 Final Project/US_national_new.csv")


## -----------------------------------------------------------------------------
## National Levels Canada
## -----------------------------------------------------------------------------

for (i in n_lists){

	search_list <- append(unlist(lists_to_search[i]), "Google Classroom")
	trends_search <- gtrends(search_list, 
	                  gprop = "web",
	                  geo = "CA",
	                  time = "2015-08-01 2015-08-01",
	                  low_search_volume = T)
	## Convert low volume searches into 0 ##
	dataframe <- trends_search$interest_over_time
	dataframe$hits[dataframe$hits == "<1"] <- 0
	## Cast DF and use google classroom as normalization vector ##
	dataframe$hits <- as.numeric(dataframe$hits)
	dataframe_wide <- dcast(dataframe, date + geo ~ keyword, value.var = "hits")
	names(dataframe_wide)[names(dataframe_wide) == "Google Classroom"] <- paste0("GC_", i)
	## Assign DF and sleep ##
	df_list[[i]] <-dataframe_wide
	Sys.sleep(60)

}

## Reduce List together  ##
Canada_data_all <- Reduce(function(x,y) merge(x,y, by = c("geo", "date"), all = TRUE), df_list)


## Normalize Columns of the rest of the dataset ##
for (i in names(US_data_all)[str_detect(names(Canada_data_all), "$GC_")]){
	if (i > 1) {
		for (j in 1:4){
			term <- lists_to_search$i[j]
			GC_col <- paste0("GC_1", i)
			Canada_data_all$term <- Canada_data_all$GC_1 / Canada_data_all$GC_col * Canada_data_all$term

		}
	}
}

## Export ##
write.csv(Canada_data_all, "/home/choliday/ECO2607 Final Project/Canada_national_new.csv")


 ## -----------------------------------------------------------------------------
