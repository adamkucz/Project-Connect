#!/usr/bin/env Rscript 

library(qualtRics)
library(readxl)
library(dplyr)
library(rlang)
library(lubridate)
library(mailR)
library(crayon)

qualtrics_api_credentials(api_key = "", # REMOVED FOR PUBLIC POSTING
                          base_url = "uwartsandsciences.az1.qualtrics.com")

# Vector of survey IDs from Qualtrics
surveyIDs <- setNames(c("", "", "", "",  # REMOVED FOR PUBLIC POSTING
                        "", "", "", ""), # REMOVED FOR PUBLIC POSTING
                      nm = paste("Week", 1:8))

# Create list of webinar data for each week
webinars <- lapply(surveyIDs, function(survey){
  survey <- try(fetch_survey(surveyID = survey, force_request = T, label = F, convert = F))
  
  # Remove duplicate entries if there is data from that week
  # Keep submissions that are complete (Finished == 1).
  # If more than 1 complete submission, keep most recent (max(EndDate))
  if(!"try-error" %in% class(survey)){
    survey %>%
      group_by(pid, week) %>%
      filter(!(n() > 1 & Finished == 0) & !(n() > 1 & all(Finished == 1) & EndDate != max(EndDate))) %>%
      ungroup()
  }
  
})

# Helper Functions --------------------------------------------------------

#' Check whether a participant has been enrolled in the Daily Diary surveys
#' @param IDs data.frame with study IDs and enrollment dates
#' @param date_col string specifying column name of date enrolled
#' @param id_col string specifying column name of study ID (indicates to user who is not enrolled)
#' @return IDs data.frame with column indicated participants who are enrolled
isEnrolled <- function(IDs, date_col, id_col){
  # Create column indicating whether participant is enrolled or not
  IDs <- IDs %>%
    mutate(enrolled = !is.na(IDs[, "Date Enrolled EMA"]) & !is.POSIXct(ids[, "Date Enrolled EMA"]))
  # IDs$enrolled <- !is.na(IDs[, "Date Enrolled EMA"]) & !is.POSIXct(ids[, "Date Enrolled EMA"])
  
  # Print warning indicating which IDs are do not have valid enrollment dates
  if(!all(IDs$enrolled)){
    message("WARNING:\nThe following participant(s) are not properly enrolled: ",
            paste(IDs[!IDs$enrolled, id_col], collapse = ", "))
  }
  
  return(IDs)
}

#' Adds column indicating if in webinar condition or not
#' @param IDs data set with participant IDs
#' @param ctracker data set with Study ID numbers and conditions
#' @return tibble of participant IDs with column indicated whether webinar or not
isWebinar <- function(IDs, ctracker){
  # Create column indicating whether participant is  or not
  IDs <- IDs %>%
    mutate(webinar = ctracker$Condition == "Webinar")
  
  return(IDs)
}

#' Create tibble indicating when to send each participant their webinar link
#' @param IDs data set with participant IDs
#' @param groups data set indicating which condition each participant (Study ID) is in
#' @return tibble with dates indicating when to send webinar links
mapWebinars <- function(IDs, groups){
  
  # Add column indicated participants who are successfully enrolled
  IDs <- isEnrolled(IDs)
  IDs <- isWebinar(IDs, groups)
  
  IDs <- IDs %>%
    filter(enrolled & webinar)
  
  # If webinar dates does not already exist, create one
  if(!file.exists("./.Webinar Dates.Rda")){
    
    webinar_dates <- tibble("Study ID" = IDs$`Study ID`,
                            "Survey Signal ID" = IDs$`Survey Signal ID`,
                            "Week 1" = as_date(ceiling_date(IDs$`Date Enrolled EMA`, "week") + days(1)),
                            "Week 2" = `Week 1` + days(7),
                            "Week 3" = `Week 2` + days(7),
                            "Week 4" = `Week 3` + days(7),
                            "Week 5" = `Week 4` + days(7),
                            "Week 6" = `Week 5` + days(7),
                            "Week 7" = `Week 6` + days(7),
                            "Week 8" = `Week 7` + days(7))
  } else { # webinar dates document already exists, so load it and work with existing document
    
    load("./.Webinar Dates.Rda")
    
    # Add new participants
    IDs <- IDs %>%
      filter(!`Study ID` %in% webinar_dates$`Study ID`)
    
    webinar_dates <- webinar_dates %>%
      rbind(tibble("Study ID" = IDs$`Study ID`,
                   "Survey Signal ID" = IDs$`Survey Signal ID`,
                   "Week 1" = as_date(ceiling_date(IDs$`Date Enrolled EMA`, "week") + days(1)),
                   "Week 2" = `Week 1` + days(7),
                   "Week 3" = `Week 2` + days(7),
                   "Week 4" = `Week 3` + days(7),
                   "Week 5" = `Week 4` + days(7),
                   "Week 6" = `Week 5` + days(7),
                   "Week 7" = `Week 6` + days(7),
                   "Week 8" = `Week 7` + days(7)))
  }
  
  save(webinar_dates, file = "./.Webinar Dates.Rda")
  
  return(webinar_dates)
}

#' Change webinar dates for participant if they do not complete one week's webinar
#' @param ID Study ID of participant
#' @param week week to change
#' @param when which date to push webinar dates from (defaults to `today`)
pushWebinarDates <- function(ID, week, when = today()){
  load("./.Webinar Dates.Rda")
  
  # Add a week to all webinar dates
  webinar_dates[webinar_dates$`Study ID` == ID, paste("Week", (week-1):8)] <- webinar_dates[webinar_dates$`Study ID` == ID, paste("Week", (week-1):8)]+ 7
  
  save(webinar_dates, file = "./.Webinar Dates.Rda")
  
  return(webinar_dates)

}

#' Write HTML for webinar emails
#' @param name participant's first name
#' @param week week number of webinar
#' @param link link containing participant's unique parameters
#' @return HTML code to embed in emails to participants
htmlMaker <- function(name, week, link, warning){
  
  image <- "<img src='http://students.washington.edu/adamkucz/images/CSSC%20Logo%20(white%20background).png' alt='CSSC Logo' style='width:25em;height:2em;'>"
  greeting <- paste0("Hi ",  name, ",")
  line1 <- paste0("It's Week ", week, " of your participation in UW Project Connect! Here's your link to access this week's videos.")
  line2 <- paste0("<a href=", link, ">Click here to access the videos.</a>")
  line3 <- paste0("If you have any questions please do not hestiate to contact us:<br>&nbsp;&nbsp;&nbsp;- <u>Coordinator</u>: Adam Kuczynski, MS (<a href='mailto:projectconnect@uw.edu'>projectconnect@uw.edu</a>)<br>&nbsp;&nbsp;&nbsp;- <u>Principle Investigator</u>: Jonathan Kanter, PhD (<a href='mailto:jonkan@uw.edu'>jonkan@uw.edu</a>)")
  salutation <- paste0("Sincerely,<br>Adam Kuczynski, M.S.<br>Study Coordinator")
  
  warning1 <- "<span style='font-weight:bold;'>Our records indicate that you did not complete last week's webinar. Please complete it this week.</span>"
  warning2 <- "<span style='font-weight:bold;text-decoration:underline;'>Our records indicate that you have not completed this webinar yet. This is the last week to complete this webinar to continue participation in this research.</span>"
  
  # Add warnings to email if needed
  html <- switch(as.character(warning),
    "0" = paste0(c(image, greeting, line1, line2, line3, salutation), collapse = "<br><br>"),
    "1" = paste0(c(image, greeting, line1, line2, warning1, line3, salutation), collapse = "<br><br>"),
    "2" = paste0(c(image, greeting, line1, line2, warning2, line3, salutation), collapse = "<br><br>")
    )

  return(html)
  
}

#' Create/append email log (to prevent multiple emails)
#' @param ID Study ID number
#' @param webinar_num webinar number
logEmail <- function(ID, webinar_num, when = now()){
  
  # If log has not been created, create it
  if(!file.exists("./.Email Log.Rda")){
    email_log <- tibble("Study ID" = ID,
                        "Date Email 1" = when,
                        "Date Email 2" = as_datetime(NA),
                        "Date Email 3" = as_datetime(NA),
                        "Webinar Number" = webinar_num,
                        "Emails Sent" = 1)
    
    # If log has been created, append email to it 
  } else {
    load("./.Email Log.Rda")
    
    # If participant has been emailed for this webinar already
    if(nrow(email_log[email_log$`Study ID` == ID & email_log$`Webinar Number` == webinar_num, ]) == 1){
      
      # Set email count (how many times has participant been emailed for given week)
      ec <- email_log %>%
        filter(`Study ID` == ID & `Webinar Number` == webinar_num) %>% 
        pull(`Emails Sent`) + 1
      
      # rows to modify
      rows <- email_log$`Study ID` == ID & email_log$`Webinar Number` == webinar_num
      
            # Increment counter
      email_log[rows, "Emails Sent"] <- ec
      
      # Log date of Nth email
      email_log[rows, paste("Date Email", ec)] <- when

    } else { # Participant has not already been emailed for this webinar
      
      email_log <- email_log %>%
        add_row("Study ID" = ID,
                "Date Email 1" = when,
                "Date Email 2" = as_datetime(NA),
                "Date Email 3" = as_datetime(NA),
                "Webinar Number" = webinar_num,
                "Emails Sent" = 1)  
    }
  }
  
  # Save email log in current WD (hidden file)
  save(email_log, file = "./.Email Log.Rda")
  
}

#' Check if participant has been emailed already
#' @param log file path to email log
#' @param ID Study ID number
#' @param webinar_num webinar number
#' @return logical indicating if participant has been emailed already or not
inEmailLog <- function(log, ID, webinar_num){
  if(file.exists(log)){
    load(log)
    el <- email_log %>%
      filter(`Study ID` == ID & `Webinar Number` == webinar_num)
    
    if(nrow(el) > 0) return(TRUE) else return(FALSE)
    
  } else {
    return(FALSE)
  }
}

#' Send email based on chosen parameters
#' @param ID participant Study ID
#' @param week week of webinar
#' @param recipient_email participant's email address
#' @param recipient_name participant's first name
#' @param recipient_url URL with parameters unique to participant
#' @param warning 0, 1, or 2 indicating which warning to send (if participant does not complete webinar)
#' @param log email log ("./.Email Log.Rda)
#' @param when when the email is being sent (defaults to today)
sendEmail <- function(ID, week, recipient_email, recipient_name, recipient_url, warning, log, when = now()){
  
  # If participant is not in email log, email them
  if(!inEmailLog("./.Email Log.Rda", ID, week)){
    message(magenta("Emailing "), green(underline(bold(ID))), magenta(" link to webinar "), green(bold(underline(week))), magenta("..."))
    
    send.mail(from = "", # REMOVED FOR PUBLIC POSTING
              to = recipient_email,
              subject = paste("UW Project Connect: Week", week, "Webinar Link"),
              body = htmlMaker(name = recipient_name,
                               week = week,
                               link = recipient_url,
                               warning = warning),
              smtp = list(host.name = "", port = , # REMOVED FOR PUBLIC POSTING
                          user.name = "", passwd = "", # REMOVED FOR PUBLIC POSTING
                          ssl = T, tls = T),
              authenticate = T,
              send = T, html = T)
    
    # Log email to prevent from emailing twice
    logEmail(ID = ID, webinar_num = week, when)
    
  } else { # Participant is inside the email log, so we need to do more checking
    
    # Has participant been emailed recently?
    load(log)
    
    # Column name of last email
    col <- email_log %>%
      filter(`Study ID` == ID,
             `Webinar Number` == week) %>%
      select(starts_with("Date")) %>%
      sapply(function(x) !is.na(x)) %>%
      which() %>%
      names() %>%
      last()
    
    date_last_email <- email_log %>%
      filter(`Study ID` == ID,
             `Webinar Number` == week) %>%
      select(col) %>%
      pull()
    
    # Email count
    ec <- email_log %>%
      filter(`Study ID` == ID,
             `Webinar Number` == week) %>%
      select(`Emails Sent`) %>%
      pull()
    
    # if participant has been emailed recently, don't email them
    if(difftime(when, date_last_email, units = "days") < 7){
      
      message(yellow("Participant #", green(bold(underline(ID))), "has already been emailed webinar", green(bold(underline(week)))))
    
    # Did participant 3-miss out of the webinars?
    } else if(ec == 3){
      
      message(red("Participant #", green(bold(underline(ID))), "3-missed out of treatment on", green(bold(underline(date_last_email + days(7))))))
      
    } else { # The last email was sent more than 7 days ago, so it's time for another reminder (email them!)
      
      message(magenta("Emailing "), green(underline(bold(ID))), magenta(" link to webinar "), green(bold(underline(week))), magenta("..."))
      
      send.mail(from = "", # REMOVED FOR PUBLIC POSTING
                to = recipient_email,
                subject = paste("UW Project Connect: Week", week, "Webinar Link"),
                body = htmlMaker(name = recipient_name,
                                 week = week,
                                 link = recipient_url,
                                 warning = warning),
                smtp = list(host.name = "", port = , # REMOVED FOR PUBLIC POSTING
                            user.name = "", passwd = "", # REMOVED FOR PUBLIC POSTING
                            ssl = T, tls = T),
                authenticate = T,
                send = T, html = T)
      
      # Log email to prevent from emailing twice
      logEmail(ID = ID, webinar_num = week, when)
      
    }
  }
}

#' Sends participants links to this week's webinar
#' @param send_dates a dataset returned by mapWebinars() containing dates to send each webinar
#' @param IDs IDs data set with study IDs and email addresses
#' @param webinar_links a vector of webinar links for each week
#' @param webinar_list list containing data for each week's webinar data
#' @param when date object indicating when to send (default is today's date)
sendLinks <- function(send_dates, IDs, webinar_links, webinar_list, when = today()){
  
  weeks_to_send <- send_dates %>%
    mutate_at(vars(one_of(paste("Week", 1:8))), function(x) x == when)
  
  # tibble containing all information needed to send webinar links
  list_to_send <- tibble("Study ID" = weeks_to_send$`Study ID`,
                         "Survey Signal ID" = weeks_to_send$`Survey Signal ID`,
                         "First Name" = IDs$`First Name`[IDs$`Study ID` %in% `Study ID`],
                         "Email" = IDs$`Email Address`[IDs$`Study ID` %in% `Study ID`],
                         "Week" = apply(weeks_to_send[, paste("Week", 1:8)], 1, function(x) if(!any(x)) return(NA) else which(x)), # If completed, make week NA, otherwise populate with week number
                         "URL" = paste0(webinar_links[paste("Week", `Week`)], "?pid=", `Study ID`, "&ssid=", `Survey Signal ID`),
                         "Warning Number" = 0)
  
  # Update list_to_send and other docs, checking if participant completed last week's webinar
  for(pid in list_to_send$`Study ID`[list_to_send$Week > 1 & !is.na(list_to_send$Week)]){
    
    # Survey Signal ID
    ssid <- list_to_send$`Survey Signal ID`[list_to_send$`Study ID` == pid]
    # Week
    wk <- list_to_send$Week[list_to_send$`Study ID` == pid]
    # Webinar data
    wd <- webinar_list[[paste("Week", wk)]]
    
    # If participant did not complete the webinar for last week...
    if(!completedWebinar(pid, (wk-1), webinar_list)){
      
      # Decrement week in list_to_send so you send them next week's list again
      list_to_send$Week[list_to_send$`Study ID` == pid] <- wk - 1
      
      # Update URL to previous (correct) week's
      list_to_send$URL[list_to_send$`Study ID` == pid] <- paste0(webinar_links[paste("Week", (wk-1))], "?pid=", pid, "&ssid=", ssid)
      
      # Update ".Webinar Dates.Rda" to reflect revised schedule given participants miss
      send_dates <- pushWebinarDates(pid, wk, when)
      
      # Check email log to see how many times participant has been emailed about this webinar
      if(file.exists("./.Email Log.Rda")){
        
        load("./.Email Log.Rda")
        
        # Number of email sent already
        es <- email_log$`Emails Sent`[email_log$`Study ID` == pid & email_log$`Webinar Number` == (wk-1)]
        
        if(!is_empty(es) && es == 1){
          list_to_send$`Warning Number`[list_to_send$`Study ID` == pid] <- 1
        } else if(!is_empty(es) && es == 2){
          list_to_send$`Warning Number`[list_to_send$`Study ID` == pid] <- 2
        }
      }
    }
  }
  
  # Send emails!
  for(pid in list_to_send$`Study ID`[!is.na(list_to_send$Week)]){
    
    webinar_week <- list_to_send$Week[list_to_send$`Study ID` == pid]
    email <- list_to_send$Email[list_to_send$`Study ID` == pid]
    name <- list_to_send$`First Name`[list_to_send$`Study ID` == pid]
    url <- list_to_send$URL[list_to_send$`Study ID` == pid]
    warning <- list_to_send$`Warning Number`[list_to_send$`Study ID` == pid]
    
    sendEmail(pid, webinar_week, email, name, url, warning, log = "./.Email Log.Rda", when = when)
    
  }
}

#' Create tibble to tracker webinar completion
#' @param groups data set indicating which condition each participant (Study ID) is in
#' @return tbl used to track participant completion
createTracker <- function(groups){
  
  tracker <- groups %>%
    filter(`Condition` == "Webinar") %>%
    select(contains("ID")) %>%
    mutate("Week 1" = NA, "Week 2" = NA, "Week 3" = NA, "Week 4" = NA,
           "Week 5" = NA, "Week 6" = NA, "Week 7" = NA, "Week 8" = NA)
  
  return(tracker)
  
}

#' Check whether participant completed week's webinar
#' @param ID participant's study ID
#' @param week week number to check
#' @param webinar_list list containing data for each week's webinar data
#' @param tracker tbl returned from `createTracker` to track participant completion
completedWebinar <- function(ID, week, webinar_list, tracker){
  
  # Create tbl of webinars for specified week
  webdat <- webinar_list[[week]]
  
  # If there are any webinars responses for specified week, check if ID is inside
  if(is.tbl(webdat) && ID %in% webdat$pid && webdat$Finished[webdat$pid == ID & !is.na(webdat$pid)] == 1){

    # Returns TRUE if ID is in webinar completions and they finished the whole survey
    # (otherwise returns FALSE)
    return(ID %in% webdat$pid && webdat$Finished[webdat$pid == ID & !is.na(webdat$pid)] == 1)
    
  } else { # If no webinar responses, return FALSE
    return(FALSE)
  }
}

# Read in necessary documents ---------------------------------------------

ids <- read_xlsx("../Participant Roster.xlsx",
                 col_types = c(rep("text", 6), "date", "logical", "date", "logical"))

conditions <- read_xlsx("../Participant Condition Tracker.xlsx")

# Send Emails -------------------------------------------------------------

# REMOVED FOR PUBLIC POSTING (links to Qualtrics surveys containing webinar materials)
links <- c("Week 1" = "",
           "Week 2" = "",
           "Week 3" = "",
           "Week 4" = "",
           "Week 5" = "",
           "Week 6" = "",
           "Week 7" = "",
           "Week 8" = "")

sendLinks(mapWebinars(ids, conditions), ids, links, webinars, when = today())

# Track Participation -----------------------------------------------------

# Create tibble of webinar dates from "./.Webinar Dates.Rda"
# (NA indicates dates in the future)
# (Document should already be created from first time sendLinks() is called)
load("./.Webinar Dates.Rda")

wd <- webinar_dates %>%
  mutate_at(vars(starts_with("Week")), function(date) replace(date, date > today(), NA))

# Create empty tracker
tracker <- createTracker(conditions)

# Do the tracking!
for(week in grep("Week", colnames(webinar_dates), value = T)){
  if(is.tbl(webinars[[week]])){
    tracker[, week] <- tracker$`Study ID` %in% webinars[[week]]$pid
  } else {
    tracker[, week] <- FALSE
  }
  
  # Replace FALSE on dates that have not yet occurred with NA
  tracker[which(is.na(wd[, week])), week] <- NA
  
}

print(tracker)

