# Project Connect

The **Daily Diary Tracker** R script uses the Qualtrics API to track daily diary survey completion over time. The output of the script gives a breakdown by participant of: diaries completed, total diaries, proportion completed, total missed, and consecutive misses.

The **Webinar Tracker** R script uses the Qualtrics API to track webinar completion weekly as well as sends all participants in the webinar condition the appropriate webinar by email each week based on their previous week's completion. The output of the script gives a breakdown by participant of which webinars are have been completed by which they have been sent as well as the webinar that each participant was emailed.

Passwords, API keys, and survey links have been removed from the R scripts for public posting.

## Administrative Files
- **Participant Roster**
  - *Study ID*: `String`
  - *Survey Signal ID*: `String`
  - *First Name*: `String`
  - *Last Name*: `String`
  - *Phone Number*: `String`
  - *Email Address*: `String`
  - *Date Enrolled EMA*: `POSIXct`
  - *Drop Out*: `Boolean`
  - *Date Drop Out*: `POSIXct`
  - *Finished*: `Boolean`
- **Participant Condition Tracker**
  - *Study ID*: `String`
  - *Condition*: `String`
  - *Therapist*: `String`
  - *Notes*: `String`
