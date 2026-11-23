library(haven)
library(tidyverse)

# Round 11 Introduces the Loneliness Items
# Compare 11-14 Over Time as predictors of cognitive measures and decline

# List only dat files
nhats_files <-
  list.files(
    path = "NHATS_R14_Final_Release_SAS_V2/",
    pattern = "*.dat"
  )

# Read in Files
# Read in SP File
sp <- read_sas(
  "NHATS_R14_Final_Release_SAS_V2/NHATS_Round_14_SP_File_V2.sas7bdat"
)

# Pinpoint Loneliness
# Lonely equals | wb14offelche5

# -9 Mssing
# -8 DK
# -7 RF
# -1 Inapplicable
# 1 Every day (7)
# 2 Most Days (5-6)
# 3 Some Days (2-4)
# 4 Rarely (Once a Week)
# 5 Never
sp |>
  select(wb14offelche5) |>
  filer(!wb14offelche5 %in% c("-", "-8", "-7", "-1"))

# Social isolatio n was  measured using a four-domain composite

# (1) living alone (household size=1) = hh14dhshldnum

# -1 Inapplicable
# 1
# 2
# 3
# 4
# 5
# 6
# 7

sp |>
  select(hh14dhshldnum) |>
  count(hh14dhshldnum)
# (2) no confidant (social network size=0) = sn14dnumsn | Number in Social Network
# -1 Inapplicable
# 0
# 1
# 2
# 3
# 4
# 5
sp |>
  select(sn14dnumsn) |>
  filter(sn14dnumsn != "-1") |>
  filter(sn14dnumsn == "0")

# (3) rstricted social participation
# (≥2 of 4 activities not performed:
# visiting friends/family = pa14vistfrfam ,
# attending religious services = pa14attrelser ,
# participating in clubs/classes = pa14clbmtgrac ,
# going out for enjoyment = pa14outfrenjy),
# yielding a 0–4

# score with ≥2 indicating social isolation (27, 28).

sp |>
  select(
    # EVER VISIT FRIENDS FAMILY
    pa14vistfrfam,
    # EVER ATTEND RELIG SERVCES
    pa14attrelser,
    # PA9 CLUB MEETINGS GRP ACTIVE
    pa14clbmtgrac,
    # EVER GO OUT FOR ENJOYMENT
    pa14outfrenjy
  ) |>
  filter_at(
    vars(
      starts_with("pa")
    ),
    any_vars(
      str_detect(., "
    [^]")
    )
  ) |>
  rowwise() |>
  mutate(
    social_so_3 =
      sum(c_across(pa14vistfrfam:pa1outfrenjy))
  ) |>
  select(social_iso_3) |>
  mutate(
    si_i4 =
      ifelse(social_iso_3 >= 2,
        "social isolation",
        "no social isola tion"
      )
  )

# fl14nooneta lk  = No one to talk to

# Social network size was measured as the number of
# persons (0–5) the participant ‘talks to about important things’ (sn#dnumsn).

# Memory Items: cg14prewrdrcl
# cg14dwrdimmrc - Score for recall
