# Data Checks and Cleaning





## Setup

library(readr)
library(dplyr)

df <- read_csv("data/raw_data_anonymized.csv")





## Count Complete Responses

df %>%
  filter(lastpage == 20) %>%
  count()

# Total: 281
# One participant was marked as "timed-out" on prolific, but contacted me
# afterwards with the correct completion code. In line with the preregistered
# procedure, their response was included in the analysis.





## Analysis of Unfinished Responses

df %>%
  filter(lastpage < 20) %>%
  count()

# Total: 15
# A closer look at the ProlificIDs (not shared here for anonymity) and data from
# the prolific dashboard revealed that among those:
# - 9 finished a different attempt
# - 6 did not finish the study
#
# The 15 unfinished responses were excluded from further analysis.





## Comprehension Check

df %>%
  filter(group != ComprehensionCheck) %>%
  count()

# 47 participants did not answer the comprehension check correctly. Following
# the preregistered procedure, these were included in the analysis anyway, but
# an additional exploratory analysis excluding these cases was also conducted.





## Export and Relabel Data for Analysis

# Clean data for the main analysis
clean_data <- df %>%
  filter(lastpage == 20) %>%
  mutate(
    group = factor(group, levels = c(1, 3, 4, 2),
                   labels = c("expressive", "magical", "secretive",
                                     "suspenseful")),
    ComprehensionCheck = factor(ComprehensionCheck, levels = c(1, 3, 4, 2),
                   labels = c("expressive", "magical", "secretive",
                              "suspenseful")),
    SAMonteroPublic = `SAMontero[ComfortPublic]`,
    SAMonteroHome = `SAMontero[ComfortPrivate]`,
    LParking = `LocationsRico[parking]`,
    LCarpark = `LocationsRico[carpark]`,
    LServicearea = `LocationsRico[servicearea]`,
    LPavement = `LocationsRico[pavement]`,
    LHome = `LocationsRico[home]`,
    LDriving = `LocationsRico[driving]`,
    LPassenger = `LocationsRico[passenger]`,
    LPub = `LocationsRico[pub]`,
    LWorkplace = `LocationsRico[workplace]`,
    AAlone = `AudiencesRico[alone]`,
    APartner = `AudiencesRico[partner]`,
    AFriends = `AudiencesRico[friends]`,
    AColleagues = `AudiencesRico[colleagues]`,
    AStrangers = `AudiencesRico[strangers]`,
    AFamily = `AudiencesRico[family]`
  ) %>%
  select(-c(submitdate:StudyID), -c(Checks:IMIE),
         -c(`SAMontero[ComfortPublic]`:`SAMontero[ComfortPrivate]`),
         -c(`LocationsRico[parking]`:`AudiencesRico[family]`),
         -c(SurveyFinished:SurveyFinishedTime))

write_csv(clean_data, "data/clean_data.csv")

# Exclude participants who responded incorrectly to the comprehension check:
only_who_passed_the_comprehension_check <- clean_data %>%
  filter(group == ComprehensionCheck)

write_csv(only_who_passed_the_comprehension_check,
          "data/only_who_passed_the_comprehension_check.csv")

# Reassign participants who responded incorrectly to the comprehension checks to
# the condition they chose in the comprehension check, rather than their actual
# group:
participants_reassigned_to_comprehension_check_instead_of_groups <- df %>%
  filter(lastpage == 20) %>%
  mutate(
    group = factor(ComprehensionCheck, levels = c(1, 3, 4, 2),
                   labels = c("expressive", "magical", "secretive",
                              "suspenseful")),
    ComprehensionCheck = factor(ComprehensionCheck, levels = c(1, 3, 4, 2),
                                labels = c("expressive", "magical", "secretive",
                                           "suspenseful")),
    SAMonteroPublic = `SAMontero[ComfortPublic]`,
    SAMonteroHome = `SAMontero[ComfortPrivate]`,
    LParking = `LocationsRico[parking]`,
    LCarpark = `LocationsRico[carpark]`,
    LServicearea = `LocationsRico[servicearea]`,
    LPavement = `LocationsRico[pavement]`,
    LHome = `LocationsRico[home]`,
    LDriving = `LocationsRico[driving]`,
    LPassenger = `LocationsRico[passenger]`,
    LPub = `LocationsRico[pub]`,
    LWorkplace = `LocationsRico[workplace]`,
    AAlone = `AudiencesRico[alone]`,
    APartner = `AudiencesRico[partner]`,
    AFriends = `AudiencesRico[friends]`,
    AColleagues = `AudiencesRico[colleagues]`,
    AStrangers = `AudiencesRico[strangers]`,
    AFamily = `AudiencesRico[family]`
  ) %>%
  select(-c(submitdate:StudyID), -c(Checks:IMIE),
         -c(`SAMontero[ComfortPublic]`:`SAMontero[ComfortPrivate]`),
         -c(`LocationsRico[parking]`:`AudiencesRico[family]`),
         -c(SurveyFinished:SurveyFinishedTime))

write_csv(participants_reassigned_to_comprehension_check_instead_of_groups,
          "data/participants_reassigned_to_comprehension_check_instead_of_groups.csv")
