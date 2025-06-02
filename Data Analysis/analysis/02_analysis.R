# Data Analysis

## Setup

library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(skimr)
library(cowplot)

# This line uses the full data set as described in the article:

df <- read_csv("data/clean_data.csv")

# Uncomment the line below to run the same analysis instead with participants
# assigned to the condition they selected in their comprehension check, not
# their actual group:

#df <- read_csv("data/participants_reassigned_to_comprehension_check_instead_of_groups.csv")

# Uncomment the line below to run the same analysis but excluding participants
# who answered the comprehension check incorrectly:

#df <- read_csv("data/only_who_passed_the_comprehension_check.csv")

# Helper function to calculate effect size for Mann-Whitney-U-Tests (based on
# Rosenthal, 1991, p. 19 via Field 2012, 15.4.6):
effectMWU <- function(pvalue) {
  z <- qnorm(p=pvalue) * -1
  r <- z / sqrt(281)
  return(r)
}

# Load the onesampb function for robust one-sample testing:
source("Rallfun-v44.txt")





## Demographics

df %>%
  group_by(Gender) %>%
  count()

df %>%
  select(Age) %>%
  skim()





## H1 Testing

# Test if social acceptability (SA) is lower for suspenseful gestures, compared
# with all other gestures, using the measure reported by Montero et al. (2010).

kruskal.test(df$SAMonteroPublic~df$group)

pairwise.wilcox.test(df$SAMonteroPublic, df$group, p.adjust.method = "none")
#p_exp_sus = .00116
#p_mag_sus = .00087
#p_sec_sus = .01749

# Calculate effect sizes:
effectMWU(.00116) #exp_sus
effectMWU(.00087) #mag_sus
effectMWU(.01749) #sec_sus





## H2 Testing

# Test if social acceptability (SA) is lower for suspenseful gestures, compared
# with all other gestures, using the measure reported by Pearson et al. (2015)
# and Koelle et al. (2018).

### H2a (Pearson)

kruskal.test(df$SAPearson~df$group)

pairwise.wilcox.test(df$SAPearson, df$group, p.adjust.method = "none")
#p_exp_sus = .0028
#p_mag_sus = .0235
#p_sec_sus = .1255

# Calculate effect sizes:
effectMWU(.0028) #exp_sus
effectMWU(.0235) #mag_sus

### H2b (Koelle)

kruskal.test(df$SAKoelle~df$group)

pairwise.wilcox.test(df$SAKoelle, df$group, p.adjust.method = "none")
#p_exp_sus = .0033
#p_mag_sus = .0003
#p_sec_sus = .0179

# Calculate effect sizes:
effectMWU(.0033) #exp_sus
effectMWU(.0003) #mag_sus
effectMWU(.0179) #sec_sus





## Plot of the Three Social Acceptability Measures

plot_colors = c("#e66101", "#b2abd2", "#fdb863", "#5e3c99")

plot_montero <- df %>%
  ggplot(aes(x = group, y = SAMonteroPublic, color = group)) +
  geom_boxplot(width = .3, outlier.alpha = 0, size = 1) +
  scale_y_continuous(limits = c(1, 6), breaks = c(1,2,3,4,5,6)) +
  geom_hline(yintercept = 3.5, color = "gray", size = 1, linetype = 2) +
  scale_color_manual(values = plot_colors) +
  labs(x = "Montero et al. (2010)", y = "Social Acceptability") +
  theme_cowplot() +
  theme(axis.text.x = element_text(angle = 30, hjust=1)) +
  theme(legend.position="none")

plot_pearson <- df %>%
  ggplot(aes(x = group, y = SAPearson, color = group)) +
  geom_boxplot(width = .3, outlier.alpha = 0, size = 1) +
  geom_hline(yintercept = 3, color = "gray", size = 1, linetype = 2) +
  scale_color_manual(values = plot_colors) +
  scale_y_continuous(limits = c(1, 5), breaks = c(1,2,3,4,5)) +
  labs(x = "Pearson et al. (2015)", y = "") +
  theme_cowplot() +
  theme(axis.text.x = element_text(angle = 30, hjust=1)) +
  theme(legend.position="none")

plot_koelle <- df %>%
  ggplot(aes(x = group, y = SAKoelle, color = group)) +
  geom_boxplot(width = .3, outlier.alpha = 0, size = 1) +
  geom_hline(yintercept = 4, color = "gray", size = 1, linetype = 2) +
  scale_color_manual(values = plot_colors) +
  scale_y_continuous(limits = c(1, 7), breaks = c(1,2,3,4,5,6,7)) +
  labs(x = "Koelle et al. (2018)", y = "") +
  theme_cowplot() +
  theme(axis.text.x = element_text(angle = 30, hjust=1)) +
  theme(legend.position="none")


sa_plot <- plot_grid(plot_montero, plot_pearson, plot_koelle, ncol = 3)

save_plot("output/social_acceptability.png", sa_plot,
          ncol = 3,
          nrow = 1,
          base_height = 4,
          base_asp = 0.8,
          base_width = NULL)





## Comparisons with Scale Centers (Suspenseful Interaction Only)

df_sus <- df %>%
  filter(group == "suspenseful")

onesampb(x = df_sus$SAMonteroPublic, est = median, nv = 3.5, nboot = 10000)
onesampb(x = df_sus$SAPearson, est = median, nv = 3, nboot = 10000)
onesampb(x = df_sus$SAKoelle, est = median, nv = 4, nboot = 10000)






## ALA Analysis (Suspenseful Interaction Only)

### Locations

df_sus_loc <- df_sus %>%
  select(id,group,c(LParking:LWorkplace)) %>%
  pivot_longer(
    cols = c(LParking:LWorkplace),
    names_to = "location",
    names_prefix = "L",
    values_to = "comfort"
  )

# Identify locations where at least half of the participants responded that
# they do not think it makes sense for the interaction at hand
# Critical n: 34
# Critical n for "reassign to comprehension check group instead of actual group": 35
# Critical n for "only who passed the comprehension check": 31
df_sus_loc %>%
  group_by(location) %>%
  filter(comfort == 6) %>%
  count()
#Carpark: 2
#Driving: 53
#Home: 4
#Parking: 1
#Passenger: 51
#Pavement: 1
#Pub: 36
#Servicearea: 1
#Workplace: 13

# Filter out unsuitable locations and remove unsuitable cases (comfort == 6):
df_sus_loc <- df_sus_loc %>%
  filter(comfort < 6) %>%
  filter(location != "Driving" &
           location != "Passenger" &
           location != "Pub")

df_sus_loc %>%
  group_by(location) %>%
  summarise(
    skim(comfort)
  )
# Median for all locations is at the scale maximum (5)


### Audiences

df_sus_aud <- df_sus %>%
  select(id,group,c(AAlone:AFamily)) %>%
  pivot_longer(
    cols = c(AAlone:AFamily),
    names_to = "audience",
    names_prefix = "A",
    values_to = "comfort"
  )

# Identify audiences where at least half of the participants responded that
# they do not think it makes sense for the interaction at hand
# (critical n = 34):
df_sus_aud %>%
  group_by(audience) %>%
  filter(comfort == 6) %>%
  count()
#Alone: 3
#Colleagues: 1
#Partner: 1
#All others: 0


# Remove unsuitable cases (comfort == 6):
df_sus_aud <- df_sus_aud %>%
  filter(comfort < 6)


df_sus_aud %>%
  group_by(audience) %>%
  summarise(
    skim(comfort)
  )
# Median for all audiences is at the scale maximum (5)


## ALA Plot

# Draw locations plot:
loc_plot <- df_sus_loc %>%
  mutate(
    location = factor(location, levels = c("Home", "Carpark", "Parking",
                                           "Pavement", "Servicearea",
                                           "Workplace"),
                      labels = c("Home", "Outdoor Parking","Indoor Parking",
                                 "Pavement", "Service area", "Workplace"))
  ) %>%
  ggplot(aes(x = location, y = comfort)) +
  geom_jitter(width = .25, height = .25, alpha = .5, color = "purple") +
  labs(x = "Location", y = "") +
  theme_cowplot() +
  theme(axis.text.x = element_text(angle = 30, hjust=1))


# Draw audiences plot:
aud_plot <- df_sus_aud %>%
  ggplot(aes(x = audience, y = comfort)) +
  geom_jitter(width = .25, height = .25, alpha = .5, color = "darkgreen") +
  labs(x = "Audience", y = "Social Acceptability") +
  theme_cowplot() +
  theme(axis.text.x = element_text(angle = 30, hjust=1))

pg <- plot_grid(aud_plot, loc_plot)

save_plot(
  "output/ALA.png",
  pg,
  ncol = 2,
  nrow = 1,
  base_height = 3.71,
  base_asp = 1,
  base_width = NULL
)





## Exploratory Analysis: SA at Home (Montero) and Confidence (Koelle)

### SA at home

kruskal.test(df$SAMonteroHome~df$group)

### Confidence

kruskal.test(df$ConfidenceKoelle~df$group)

pairwise.wilcox.test(df$ConfidenceKoelle, df$group, p.adjust.method = "hochberg")
#p_exp_sus = .11354
#p_mag_sus = .00053
#p_sec_sus = .04138

# Calculate effect sizes:
effectMWU(.02567) #mag_exp - only significant in the "comprehension checks as groups" test
effectMWU(.00053) #mag_sus
effectMWU(.04138) #sec_sus
