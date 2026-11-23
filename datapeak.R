library(haven)
library(tidyverse)

# Round 11 introduces the loneliness items.
# Compare R11-14 over time as predictors of cognitive measures and decline.

# List only the .dat files in the release folder
nhats_files <-
  list.files(
    path = "NHATS_R14_Final_Release_SAS_V2/",
    pattern = "\\.dat$"
  )

# Read in the SP (sample person) file
sp <- read_sas(
  "NHATS_R14_Final_Release_SAS_V2/NHATS_Round_14_SP_File_V2.sas7bdat"
)

# ---- Loneliness ----------------------------------------------------------
# wb14offelche5 | WB1 "During the last month, how often have you felt lonely?"
# -9 Missing | -8 DK | -7 RF | -1 Inapplicable
#  1 Every day (7) | 2 Most days (5-6) | 3 Some days (2-4) |
#  4 Rarely (once a week) | 5 Never
sp |>
  select(wb14offelche5) |>
  filter(!wb14offelche5 %in% c(-9, -8, -7, -1)) |>
  count(wb14offelche5)

# ---- Social isolation: four-domain composite (He et al., 2026) -----------

# (1) Lives alone: household size = 1  -> hh14dhshldnum
sp |>
  count(hh14dhshldnum)

# (2) No confidant: social network size = 0 -> sn14dnumsn
#     (number of persons, 0-5, "talks to about important things")
sp |>
  select(sn14dnumsn) |>
  filter(sn14dnumsn != -1) |>
  count(sn14dnumsn)

# (3) Restricted participation: >= 2 of 4 activities NOT performed
#     pa14vistfrfam (PA1)  ever visit friends/family
#     pa14attrelser (PA5)  ever attend religious services
#     pa14clbmtgrac (PA9)  club meetings / group activities
#     pa14outfrenjy (PA13) ever go out for enjoyment
#     each: 1 = Yes, 2 = No (-1/-7/-8/-9 missing); "not performed" = 2
pa_items <- c("pa14vistfrfam", "pa14attrelser", "pa14clbmtgrac", "pa14outfrenjy")

sp |>
  select(all_of(pa_items)) |>
  mutate(
    n_not_performed = rowSums(across(all_of(pa_items), ~ .x == 2), na.rm = TRUE),
    restricted = if_else(n_not_performed >= 2, 1L, 0L)
  ) |>
  count(n_not_performed, restricted)

# ---- Composite score and isolation flag ----------------------------------
# NOTE: He et al. report a 0-4 score but describe three binary domains (max 3).
# The original Cudjoe (2020) index keeps religious attendance as its own domain.
# Reconcile before finalizing -- see index.qmd "reconcile the composite range".
sp |>
  transmute(
    lives_alone  = if_else(hh14dhshldnum == 1, 1L, 0L, missing = 0L),
    no_confidant = if_else(sn14dnumsn == 0, 1L, 0L, missing = 0L),
    restricted   = if_else(
      rowSums(across(all_of(pa_items), ~ .x == 2), na.rm = TRUE) >= 2, 1L, 0L
    )
  ) |>
  mutate(
    isolation_score = lives_alone + no_confidant + restricted,
    isolated = if_else(
      isolation_score >= 2, "social isolation", "no social isolation"
    )
  ) |>
  count(isolation_score, isolated)

# fl14noonetalk = SP has no one to talk to (flag; 1 = Yes) -- candidate 4th domain
sp |>
  count(fl14noonetalk)

# ---- Memory / cognition --------------------------------------------------
# cg14dwrdimmrc | immediate word recall score (0-10)
# cg14dwrddlyrc | delayed word recall score (0-10)
# (cg14prewrdrcl is the CG7PRE "intro to word recall" flag, NOT a score)
sp |>
  select(cg14dwrdimmrc, cg14dwrddlyrc) |>
  summary()
