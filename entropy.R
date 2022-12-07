# calculate entropy ratio

library(NetFACS)
library(readr)
library(dplyr)
library(tidyr)
library(purrr)

# MaqFACS data
maqfacs <- read_tsv("maqfacs.txt")
# Action Units
AU <- c("10", "12", "16", "17", "18", "1plus2", "26", "27", "41", "43", "5", "6", "8", "9", "ad185", "ad19", "eau1", "eau2", "eau3", "ad101", "ad59")

# clear contexts only
dc <-
  maqfacs %>%
  filter(context %in% c("affiliative", "aggressive", "submissive"))

# all contexts for overall entropy
da <-
  maqfacs %>%
  mutate(context = "overall")

dl <-
  list(c = dc,
       a = da)

dl2 <- bind_rows(dl)

set.seed(2022)
dl3 <-
  dl2 %>%
  select(c(species, context), all_of(AU)) %>%
  nest(data = -c(species, context)) %>%
  mutate(boot = map(data, ~rsample::bootstraps(.x, times = 100)))

dl4 <-
  dl3 %>%
  select(-data) %>%
  unnest(cols = c(boot)) %>%
  mutate(m = map(splits, ~.x %>% rsample::analysis() %>% as.matrix()))

# calculate entropy
set.seed(2022)
# ***** Takes a long time to run! ******
dl5 <-
  dl4 %>%
  mutate(
    entropy = lapply(m, function(x){
      NetFACS::entropy_overall(x)
    })
  )

# result
boot.ratio <-
  dl5 %>%
  select(species, context, id, entropy) %>%
  unnest(cols = entropy) %>%
  mutate(across(context, factor,
                levels = c("affiliative", "aggressive", "submissive", "overall")))

# summary
boot.ratio %>%
  group_by(species, context) %>%
  summarise(across(
    entropy.ratio,
    list(mean   = mean,
         min    = min,
         max    = max),
    .names = "{.fn}"
  ), .groups = "drop") %>%
  arrange(context)



