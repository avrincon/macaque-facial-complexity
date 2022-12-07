# Simulate FACS data
# calculate specificity when the number of observations per context is imbalanced
# calculate specificity after upsampling minority contexts

library(NetFACS)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)

elements <- as.character(1:10)
contexts <- LETTERS[1:3]
n_obs <- 1000 # n observations per context
# randomly generate probability of elements
set.seed(123)
probabilities <-
  sapply(elements, function(x) {
    p <- runif(length(contexts), min = 0.05, max = 0.95)
    setNames(round(p, 1), nm = contexts)
  })
set.seed(123)
sim_data <- NetFACS::sim_facs(probabilities, n_obs)

head(sim_data)
# try not to have empty rows
any(rowSums(sim_data) == 0)
sum(rowSums(sim_data) == 0)

# proportion of rows to sample. Contexts B and C will have fewer rows/observations
dp <-
  tibble(condition = contexts,
         prop = c(1, 0.5, 0.1)) %>%
  mutate(N = prop * n_obs)

# rowids per context
rowid <-
  list(
    A = which(rownames(sim_data)=="A"),
    B = which(rownames(sim_data)=="B"),
    C = which(rownames(sim_data)=="C")
  )

sampled_rows <- map2(rowid, dp$prop, ~sample(.x, size = .y*length(.x)))

imbalanced_data <- sim_data[sort(unlist(sampled_rows)), ]

table(rownames(imbalanced_data))

# calculate specificity
res.imb <-
  NetFACS::specificity(
    imbalanced_data,
    condition = rownames(imbalanced_data),
    combination.size = 1,
    upsample = FALSE # keep observations per condition imbalanced
  )
res.ups <-
  NetFACS::specificity(
    imbalanced_data,
    condition = rownames(imbalanced_data),
    combination.size = 1,
    upsample = TRUE # randomly upsample observations from conditions B and C
  )
# original data with true specificity
res.org <-
  NetFACS::specificity(
    sim_data,
    condition = rownames(sim_data),
    combination.size = 1,
    upsample = FALSE
  ) %>%
  rename(true_specificity = specificity) %>%
  select(condition, combination, true_specificity)

# combine results
res <-
  bind_rows(raw = res.imb,
            upsampled = res.ups,
            .id = "data") %>%
  select(data, condition, combination, specificity)

res2 <-
  res %>%
  left_join(res.org, by = c("condition", "combination")) %>%
  left_join(dp, by = c("condition"))


# calculate error between observed specificity and true specificity
res_specificity_supp <-
  res2 %>%
  mutate(diff = specificity - true_specificity) %>%
  mutate(across(combination, factor, levels = elements)) %>%
  rename(context = condition)

# plot
res_specificity_supp %>%
  mutate(label = paste0("Context: ", context,'\n(N = ',N,')')) %>%
  ggplot(aes(combination, diff, color = data)) +
  geom_point() +
  xlab("Element") +
  ylab("Deviation from true specificity") +
  facet_wrap(vars(label), ncol = 3)
