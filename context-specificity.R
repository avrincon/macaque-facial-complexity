# calculate context specificity of Action Unit combinations

library(NetFACS)
library(readr)
library(dplyr)
library(tidyr)
library(purrr)

# MaqFACS data
maqfacs <- read_tsv("maqfacs.txt")
# Action Units
AU <- c("10", "12", "16", "17", "18", "1plus2", "26", "27", "41", "43", "5", "6", "8", "9", "ad185", "ad19", "eau1", "eau2", "eau3", "ad101", "ad59")

# prepare data
# keep clear contexts
d <-
  maqfacs %>%
  filter(context %in% c("affiliative", "aggressive", "submissive")) %>%
  mutate(across(species, factor, levels = c("rhesus", "barbary", "crested")))

d2 <-
  d %>%
  select(species, context, all_of(AU)) %>%
  arrange(species) %>%
  nest(data = -c(species))

d3 <-
  d2 %>%
  mutate(
    m = map(data, ~.x %>% select(all_of(AU)) %>% as.matrix()),
    context = map(data, ~.x$context)
  )

# calculate specificity
set.seed(2022)
d4 <-
  d3 %>%
  mutate(res = map2(m, context, ~specificity(.x, .y, upsample = TRUE)))

d5 <-
  d4 %>%
  select(species, res) %>%
  unnest("res")

# keep only combinations that occurred in >1% of observations
res_specificity <-
  d5 %>%
  filter(observed.prob >= 0.01)

# summary
res_specificity %>%
  group_by(species, condition) %>%
  summarise(across(
    specificity,
    list(
      mean = mean,
      n = ~n()
    ),
    .names = "{.fn}"
  ), .groups = "keep")
