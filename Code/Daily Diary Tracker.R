library(qualtRics)
library(dplyr)
library(readxl)
library(lubridate)

qualtrics_api_credentials(api_key = "", # REMOVED FOR PUBLIC POSTING
                          base_url = "uwartsandsciences.az1.qualtrics.com")

d <- fetch_survey(surveyID = "SV_beid6EAUPC6cvul",
                  force_request = T, # Always download new survey with request
                  label = F,         # Import response as values (not choice text) 
                  convert = F)       # Do not convert question types when reading in data 

# Remove Unnecessary Columns ----------------------------------------------
d <- d %>%
  select(-c(ResponseID, ResponseSet, RecipientLastName, RecipientFirstName,
            RecipientEmail, ExternalDataReference, RDate))

# Remove duplicate entries ------------------------------------------------
# Keep submissions that are complete (Finished == 1).
# If more than 1 complete submission, keep most recent (max(EndDate))
d <- d %>%
  group_by(PID, DAY) %>%
  filter(!(n() > 1 & Finished == 0) & !(n() > 1 & all(Finished == 1) & EndDate != max(EndDate))) %>%
  ungroup()

# Helper Functions --------------------------------------------------------

#' Convert column classes across data.frame.
#' @param data a data.frame
#' @param class_names a vector of class names
#' @param ... additional arguments
#' @return data.frame with revised classes
convertClass <- function(data, class_names, ...){

  out <- lapply(1:ncol(data), FUN = function(i){
    convert <- switch(class_names[i],
                      character = as.character,
                      numeric = as.numeric,
                      factor = as.factor,
                      logical = as.logical,
                      POSIXct = as.POSIXct,
                      date = as.Date)
    convert(data[,i], ...)
  })
  names(out) <- colnames(data)
  return(as_tibble(out))
  
}

#' Check whether a participant has been enrolled in the Daily Diary surveys
#' @param IDs data.frame with study IDs and enrollment dates
#' @param date_col string specifying column name of date enrolled
#' @param id_col string specifying column name of study ID (indicates to user who is not enrolled)
#' @return IDs data.frame with column indicated participants who are enrolled
isEnrolled <- function(IDs, date_col, id_col){
  # Create column indicating whether participant is enrolled or not
  IDs <- IDs %>%
    mutate(enrolled = !is.na(IDs[, "Date Enrolled EMA"]) & !is.POSIXct(ids[, "Date Enrolled EMA"])) %>%
    filter(!`Drop Out`, !Finished)
  
  # Print warning indicating which IDs are do not have valid enrollment dates
  if(!all(IDs$enrolled)){
    message("WARNING:\nThe following participant(s) are not properly enrolled: ",
            paste(IDs[!IDs$enrolled, id_col], collapse = ", "))
  }
  
  return(IDs)
}

#' Create data.frame to track EMA completion
#' @param IDs data.frame with participant IDs (must have 'ID' in column name)
#' @param npings integer representing the number of pings desired in the tracker 
#' @return data.frame used to track participant completion
createTracker <- function(IDs, npings){
  # Add column to IDs indicating if participant is enrolled
  IDs <- isEnrolled(IDs, "Date Enrolled EMA", "Study ID")
  
  # Create tracker with Participant ID columns and columns for npings
  tracker <- data.frame(matrix(nrow = sum(IDs$enrolled),
                               ncol = length(grep("ID", colnames(IDs))) + npings))
  
  colnames(tracker) <- c(grep("ID", colnames(IDs), value = T), paste0("day", 1:npings))
  
  # Add participant IDs into tracker data.frame
  tracker <- merge(IDs[IDs$enrolled, grep("ID", colnames(tracker))], tracker, all.x = T)
  
  return(tracker)
}

#' Map out entire set of signals for participants
#' @param study_id study ID number for participant
#' @param enrollment_date date participant sent first signal
#' @return vector of 100L containing dates of participant prompts
mapSignals <- function(study_id, enrollment_date){

  dates <- as_date(enrollment_date) + days(1:100)
  dates <- as_datetime(paste(dates, "19:30:00"), tz = "US/Pacific")

  names(dates) <- paste0("day", 1:100)
  return(dates)
}

#' Convert column classes across data.frame.
#' @param tracker EMA tracker document to be summarized
#' @return a tibble containing summary data
summarizeTracker <- function(tracker){
  s <- tibble("Study ID" = tracker$`Study ID`,
              "Days Complete" = rowSums(select(tracker, starts_with("day")), na.rm = T),
              "Days Total" = rowSums(!is.na(select(tracker, starts_with("day")))),
              "Prop. Complete" = `Days Complete` / `Days Total`,
              "Total Misses" = `Days Total` - `Days Complete`,
              "Consecutive Misses" = apply(tracker[, grep("^day", colnames(tracker))], 1, function(x){
                record <- x[!is.na(x)]
                if(any(record)){
                  return(length(record) - which(record)[length(which(record))])
                } else {
                  return(NA)
                }
              }))
  return(s)
}

# Change data types of columns --------------------------------------------

column_classes <- c("IPAddress" = "character", "StartDate" = "POSIXct", "EndDate" = "POSIXct",
                    "Finished" = "numeric", "Status" = "numeric", "SSID" = "numeric", "PID" = "numeric", "SMS" = "numeric",
                    "DAY" = "numeric", "SIG" = "numeric", "RDate" = "POSIXct", "TIME" = "character", "RT" = "character",
                    "TimeZone" = "numeric", "addcode" = "numeric", "mobile" = "numeric", "FearOfIntimacy" = "numeric",
                    "lonely" = "numeric", "SocialConnection" = "numeric", "stress_4" ="numeric", "SocialInteraction" ="numeric",
                    "SocialSatisfaction" = "numeric", "depressedmood" = "numeric", "anhedonia" = "numeric", "oa_ppr" = "numeric",
                    "ee" = "numeric", "vsd" = "numeric", "ask" = "numeric", "ac_accepting" = "numeric", "ac_connecting" = "numeric",
                    "safety" = "numeric", "validate" = "numeric", "giving" = "numeric", "oa_aware" = "numeric", "oa_accepting" = "numeric",
                    "sa_aware" = "numeric", "sa_accepting" = "numeric", "ACLPracticed" = "numeric", "WhichACLPracticed_4" = "numeric",
                    "WhichACLPracticed_5" = "numeric", "WhichACLPracticed_6" = "numeric", "WhichACLPracticed_7" = "numeric",
                    "WhichACLPracticed_8" = "numeric", "WhichACLPracticed_9" = "numeric", "WhichACLPracticed_10" = "numeric",
                    "WhichACLPracticed_11" = "numeric", "WhichACLPracticed_12" = "numeric", "WhichACLPracticed_13" = "numeric",
                    "WhichACLPracticed_14" = "numeric", "WhichACLPracticed_15" = "numeric", "WhichACLPracticed_16" = "numeric",
                    "ACLHelpful" = "numeric", "who_primary" = "numeric", "who_primary_TEXT" = "character", "who_secondary_1" = "numeric",
                    "who_secondary_2" = "numeric", "who_secondary_3" = "numeric", "who_secondary_4" = "numeric",
                    "who_secondary_5" = "numeric", "who_secondary_6" = "numeric", "who_secondary_6_TEXT" = "numeric",
                    "RO-BR-FL_26" = "character", "RO-BL-Daily Diary" = "character", "LocationLatitude" = "numeric",
                    "LocationLongitude" = "numeric", "LocationAccuracy" = "numeric")

# Convert classes of daily diary data
d <- convertClass(as.data.frame(d), column_classes, tz = "UTC")

# Read in necessary documents ---------------------------------------------

# Contains Survey Signal IDs and Study IDs
ids <- read_xlsx("../Participant Roster.xlsx",
                 col_types = c(rep("text", 6), "date", "logical", "date", "logical"))

# Start Tracker -----------------------------------------------------------

# Create tracker document from list of IDs
tracker <- createTracker(ids, 100)

# Do the tracking!
for(pid in tracker$`Survey Signal ID`){
  # Map out entire set of signals for participant
  signals <- mapSignals(pid, ids$`Date Enrolled EMA`[ids$`Survey Signal ID` == pid])
    
  # Return TRUE for all dates in `signal` that are in `d`, else FALSE
  dates <- as.Date(signals) %in% as.Date(d$EndDate[d$PID == pid])
  
  # Return TRUE for all days in `signals` that are in `d`, else FALSE
  # (This is helpful if a participant completes a day's survey the next day)
  days <- sub("^day([0-9]*)$", "\\1", names(signals)) %in% d$DAY[d$PID == pid]
  
  # Replace FALSE on dates that have not yet occurred with NA
  # (If run before 7:30pm, will *not* count today)
  days[signals > now(tz = "US/Pacific")] <- NA
  
  # Fill in tracker
  tracker[tracker$`Survey Signal ID` == pid, grep("^day", colnames(tracker))] <- days
}

summarizeTracker(tracker)