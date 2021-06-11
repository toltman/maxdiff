library(tidyverse)
library(stringr)
library(bwsTools)

df_raw = read.csv("data/UXRPS2849 Marketplace Comms - Calls Maxdiff_June 1, 2021_11.39.csv")

# keep column detail rows in col_details frame
col_details <- df_raw %>% filter(row_number() %in% c(1,2))

df <- df_raw %>% 
  # filter out rows with column details
  filter( ! row_number() %in% c(1,2) ) %>% 
  # select only needed vars
  select(c("ResponseId", "Q37_1":"Q52_5", "item1_1":"item5_16"))


# change column names to match bwsTools format
colnames(df) <- c("rid", 
                  paste0("q", rep(1:16, each = 5), "_i", rep(1:5, 16), c(rep("_y", 16*5))),
                  paste0("q", rep(1:16, 5), "_i", rep(1:5, each = 16), c(rep("_t", 16*5))))

# create dataset compatible with bwsTools package
data <- df %>% 
  gather("temp", "value", -rid) %>% 
  separate(temp, c("block", "item", "t_or_y"), sep = "_") %>%
  spread(t_or_y, value) %>%
  mutate(
    y = case_when(
      y == "Most important" ~ 1,
      y == "Least important" ~ -1, 
      y == "" ~ 0
    )
  ) %>%
  select(-item)

# aggregate scores
dat.agg <- data %>% 
  group_by(t) %>%
  summarize(
    total = n(),
    best = sum(y == 1),
    worst = sum(y == -1)
  )

# use shorter label descriptions
data$t <- str_match(data$t, "<b>(.*):</b>")[,2]
dat.agg$t <- str_match(dat.agg$t, "<b>(.*):</b>")[,2]

# MNL model
res1 <- ae_mnl(dat.agg, "total", "best", "worst")

# Visualization of MNL model
dat.agg %>% 
  bind_cols(res1) %>% 
  arrange(b) %>% 
  mutate(t = factor(t, t)) %>% 
  ggplot(aes(x = t, y = b)) +
  geom_point() +
  geom_errorbar(aes(ymin = lb, ymax = ub), width = 0) +
  coord_flip()


# Individual scores

set.seed(1839)
res1_i <- diffscoring(data, "rid", "block", "t", "y")
res2_i <- e_bayescoring(data, "rid", "block", "t", "y")
res3_i <- eloscoring(data, "rid", "block", "t", "y")
res4_i <- walkscoring(data, "rid", "block", "t", "y")
res5_i <- prscoring(data, "rid", "block", "t", "y")


indiv %>%
  filter(block == 2)

head(data)
data %>%
  filter(block == "q1") %>%
  head()

# full list of items
sort(unique(data$t))


sort(unique((data %>% filter(rid == "R_0dM20dYX057MyNX"))$t))
sort(unique((data %>% filter(rid == "R_z7IDQ4IuQCIRKCJ"))$t))


# each respondents gets different numbers of each option
table((data %>% filter(rid == "R_0dM20dYX057MyNX"))$t)
table((data %>% filter(rid == "R_z7IDQ4IuQCIRKCJ"))$t)



test <- lapply(unique(data[['rid']]), function(x) {
  sort(unique(data[['t']][data[['rid']] == x]))
})

unique(data[['rid']])[!Vectorize(identical, "x")(test, test[[1]])]

# rids to drop
rids_drop <- unique(data[['rid']])[!Vectorize(identical, "x")(test, test[[1]])]

data_f <- data %>% filter(!rid %in% rids_drop)

set.seed(1839)
res1_i <- diffscoring(data_f, "rid", "block", "t", "y")
res2_i <- e_bayescoring(data_f, "rid", "block", "t", "y")
res3_i <- eloscoring(data, "rid", "block", "t", "y")
res4_i <- walkscoring(data, "rid", "block", "t", "y")
res5_i <- prscoring(data, "rid", "block", "t", "y")


E <- 0.1

# aggregate estimates
agg_dat <- data %>% 
  group_by(t) %>%
  summarize(
    bests = sum(y == 1),
    worsts = sum(y == -1),
    all = n()
  ) %>%
  mutate(p_j = (all - worsts + bests) / (2 * all)) %>%
  select(t, p_j)

# individual estimates
ind_dat <- data %>%
  group_by(rid, t) %>%
  summarize(
    bests = sum(y == 1),
    worsts = sum(y == -1),
    all = n()
  ) %>%
  mutate(p_ij = (all - worsts + bests) / (2 * all)) %>%
  ungroup() %>%
  mutate(
    p_ij = case_when(
      p_ij == 0 ~ E,
      p_ij == 1 ~ (1 - E),
      TRUE ~ p_ij
    )
  )

# combine to get empirical bayes
alpha <- 1
e_bayes <- ind_dat %>%
  left_join(agg_dat, by = "t") %>%
  mutate(
    p_ij = ((1 / (1 + alpha)) * p_ij) + ((alpha / (1 + alpha)) * p_j)
  ) %>%
  mutate(b_ebayes = log(p_ij / (1 - p_ij))) %>%
  select(rid, t, b_ebayes)

e_bayes %>% filter(t == "'Cloud' Voicemail") %>%
  ggplot(mapping = aes(b_ebayes)) + 
  geom_density()

ggplot(e_bayes, mapping = aes(b_ebayes)) +
  geom_density() +
  facet_wrap(vars(t))
 