# ===============================
# Setup
# ===============================
library(tidyverse)
library(janitor)
library(stringr)
library(gsheet)
library(hrbrthemes)
library(ggthemes)

# List your Google Sheet URLs (replace these with yours)
urls <- c(
  cp0 = "https://docs.google.com/spreadsheets/d/1yibTZuzSG4rMMmJLEnu2LTN7mPBr9pFgoRYg4buPEV4/edit?usp=share_link",
  cp1 = "https://docs.google.com/spreadsheets/d/1nEO-tAHDVlFK0u1oaJIARfAMjzTHFTQFdxi9hQlTRaQ/edit?usp=share_link",
  cp2 = "https://docs.google.com/spreadsheets/d/1hmgFg2N28uWsHpD9AM-A7z5EJLyy3LUg0Okqb79___M/edit?usp=share_link",
  cp3 = "https://docs.google.com/spreadsheets/d/1bw1trN8umHa9TJBBtPLMXxPnJQwk7EjfLqeARTzGq8E/edit?usp=share_link",
  cp4 = "https://docs.google.com/spreadsheets/d/1aDaoFzQhVKGZtRNgVYNM72VWQ9wYoFPIMlHT191OK-8/edit?usp=share_link"
)

gsheet2tbl(urls[1])  |> 
  filter(!is.na(harvest)) |> 
  mutate(`submission-time` = ymd_hm(`submission-time`)) |> 
  group_by(`id-number`) |> 
  filter(max(`submission-time`) == `submission-time`) |> 
  filter(row_number() == 1) |> 
  ungroup() |> 
  mutate(`id-number` = NA) |>
  slice_sample(prop = 1) |> 
  write_csv('/Users/bchoe/Documents/websites/bcdanl.github.io/data/econ-340-redd-exp-cp0-2025-1006.csv')

gsheet2tbl(urls[2]) |> 
  mutate(`submission-time` = ymd_hm(`submission-time`)) |> 
  mutate(pes = ifelse(is.na(pes), "PES", pes)) |> 
  group_by(`id-number`) |> 
  filter(max(`submission-time`) == `submission-time`) |> 
  filter(row_number() == 1) |>  
  ungroup() |> 
  mutate(`id-number` = NA) |> 
  slice_sample(prop = 1) |> 
  write_csv('/Users/bchoe/Documents/websites/bcdanl.github.io/data/econ-340-redd-exp-cp1-2025-1006.csv')

gsheet2tbl(urls[3]) |> 
  mutate(`submission-time` = ymd_hm(`submission-time`)) |> 
  group_by(`id-number`) |> 
  filter(max(`submission-time`) == `submission-time`) |> 
  filter(row_number() == 1) |>  
  ungroup() |> 
  mutate(`id-number` = NA) |> 
  mutate(pes = ifelse(is.na(pes), "PES", pes)) |> 
  slice_sample(prop = 1) |> 
  write_csv('/Users/bchoe/Documents/websites/bcdanl.github.io/data/econ-340-redd-exp-cp2-2025-1006.csv')

gsheet2tbl(urls[4]) |> 
  mutate(`submission-time` = ymd_hm(`submission-time`)) |> 
  group_by(`id-number`) |> 
  filter(max(`submission-time`) == `submission-time`) |> 
  filter(row_number() == 1) |>  
  ungroup() |> 
  mutate(`id-number` = NA,
         community = case_when(community == "A" ~ "community_Brazil",
                               community == "B" ~ "community_Peru",
                               community == "C" ~ "community_Indonesia",
                               community == "D" ~ "community_India",
                               community == "E" ~ "community_DRCongo",
                               community == "F" ~ "community_CostaRica",
         )) |> 
  mutate(split_rule = ifelse(community == "community_DRCongo", 
                             "floor_plus_proportional", split_rule),
         pes_group = ifelse(community == "community_Peru", 
                             "PES", pes_group)
         ) |> 
  arrange(community) |> 
  write_csv('/Users/bchoe/Documents/websites/bcdanl.github.io/data/econ-340-redd-exp-cp3-2025-1006.csv')

gsheet2tbl(urls[5]) |> 
  mutate(`submission-time` = ymd_hm(`submission-time`)) |> 
  group_by(`id-number`) |> 
  filter(max(`submission-time`) == `submission-time`) |> 
  filter(row_number() == 1) |>  
  ungroup() |> 
  mutate(`id-number` = NA,
         community = case_when(community == "A" ~ "community_Brazil",
                               community == "B" ~ "community_Peru",
                               community == "C" ~ "community_Indonesia",
                               community == "D" ~ "community_India",
                               community == "E" ~ "community_DRCongo",
                               community == "F" ~ "community_CostaRica",
         )) |>
  mutate(split_rule = ifelse(community == "community_Brazil", 
                             "remove_benefits_illegal_equal", split_rule),
         split_rule = ifelse(community == "community_CostaRica", 
                             "compliers_only_equal", split_rule),
         split_rule = ifelse(community == "community_DRCongo", 
                             "equal", split_rule),
         split_rule = ifelse(community == "community_Peru", 
                             "no_PES", split_rule),
         police = ifelse(community == "community_CostaRica", 
                         "Yes", police),
         police = ifelse(community == "community_Peru", 
                         "No", police),
         pes_group = ifelse(community == "community_Brazil", 
                            "PES", pes_group),
         pes_group = ifelse(community == "community_Peru", 
                            "No", pes_group),
         illegal = ifelse(community == "community_Peru", 
                            "no_PES", illegal)
  ) |> 
  arrange(community) |> 
  write_csv('/Users/bchoe/Documents/websites/bcdanl.github.io/data/econ-340-redd-exp-cp4-2025-1006.csv')

df_0 <- gsheet2tbl(urls[1]) |> 
  filter(!is.na(harvest)) |> 
  mutate(`submission-time` = ymd_hm(`submission-time`)) |> 
  group_by(`id-number`) |> 
  filter(max(`submission-time`) == `submission-time`) |> 
  filter(row_number() == 1) |> 
  ungroup() |> 
  mutate(period = "cp0", .before = 1,
         harvest = ifelse(harvest == "No", 0, 1),
         harvest_value = ifelse(harvest_value == 0, 1, harvest_value),
         earning = 70 + harvest_value * harvest) |> 
  relocate(`id-number`, period, earning)

df_1 <- gsheet2tbl(urls[2]) |> 
  mutate(`submission-time` = ymd_hm(`submission-time`)) |> 
  group_by(`id-number`) |> 
  filter(max(`submission-time`) == `submission-time`) |> 
  filter(row_number() == 1) |> 
  ungroup() |> 
  mutate(period = "cp1", .before = 1,
         pes = ifelse(pes == "No", 0, 1),
         pes_payment = 50 * pes,
         harvest_value = ifelse(harvest_value == 0, 1, harvest_value),
         harvest_payment = harvest_value * (1-pes),
         earning = 70 + pes_payment + harvest_payment) |> 
  relocate(`id-number`, period, earning)

set.seed(14454)
df_2 <- gsheet2tbl(urls[3]) |> 
  mutate(`submission-time` = ymd_hm(`submission-time`)) |> 
  group_by(`id-number`) |> 
  filter(max(`submission-time`) == `submission-time`) |> 
  filter(row_number() == 1) |> 
  ungroup() |> 
  mutate(period = "cp2", .before = 1,
         pes = ifelse(pes == "Yes" | is.na(pes), 1, 0), # obs w/ is.na(pes) did illegal
         illegal = ifelse(illegal == "N" | is.na(illegal), 0, 1),
         pes_payment = 50 * pes,
         harvest_value = ifelse(harvest_value == 0, 1, harvest_value),
         harvest_payment = harvest_value * (1-pes),
         fine = ifelse(
           pes == 1,
           rbinom(n(), 1, 0.25),  # 25% if PES participant
           0
         ),
         earning = 70 + pes_payment + harvest_payment + illegal*harvest_value +
                  - fine*pes*illegal*harvest_value - fine*70 - fine*50) |> 
  relocate(`id-number`, period, earning) |> 
  arrange(-pes,-fine)


df_3 <- gsheet2tbl(urls[4]) |> 
  mutate(`submission-time` = ymd_hm(`submission-time`)) |> 
  group_by(`id-number`) |> 
  filter(max(`submission-time`) == `submission-time`) |> 
  filter(row_number() == 1) |> 
  ungroup() |> 
  mutate(split_rule = ifelse(community == "E", 
                             "floor_plus_proportional", split_rule),
         pes_group = ifelse(community == "B", 
                            "PES", pes_group)
  ) |> 
  mutate(period = "cp3", .before = 1,
         pes = ifelse(pes_group == "No", 0, 1),
         pes_payment = 50 * pes,
         harvest_value = ifelse(harvest_value == 0, 1, harvest_value),
         harvest_payment = harvest_value * (1-pes)) |> 
  group_by(community) |> 
  mutate(additional = case_when(split_rule == 'equal' ~ pes_payment,
                                split_rule == 'proportional_card' ~  harvest_value * pes_payment*n() / sum(harvest_value),
                                split_rule == 'floor_plus_proportional' ~ .75*pes_payment*n() + .25*harvest_value * pes_payment*n() / sum(harvest_value),
                                split_rule == 'floor_plus_needs' ~  .75*pes_payment*n() + .25*(1/harvest_value) * pes_payment*n() / sum(1/harvest_value),
                                is.na(split_rule) | split_rule == "no_PES" ~ 0)
         ) |> 
  ungroup() |> 
  mutate(earning = round(70 + harvest_payment + additional, 2)) |> 
  relocate(`id-number`, period, earning)


set.seed(14454)
df_4 <- gsheet2tbl(urls[5]) |>
  mutate(`submission-time` = ymd_hm(`submission-time`)) |> 
  group_by(`id-number`) |> 
  filter(max(`submission-time`) == `submission-time`) |> 
  filter(row_number() == 1) |> 
  ungroup() |> 
  mutate(split_rule = ifelse(community == "A", 
                             "remove_benefits_illegal_equal", split_rule),
         split_rule = ifelse(community == "F", 
                             "compliers_only_equal", split_rule),
         split_rule = ifelse(community == "E", 
                             "equal", split_rule),
         split_rule = ifelse(community == "B", 
                             "no_PES", split_rule),
         police = ifelse(community == "F", 
                         "Yes", police),
         police = ifelse(community == "B", 
                         "No", police),
         pes_group = ifelse(community == "A", 
                            "PES", pes_group),
         pes_group = ifelse(community == "B", 
                            "No", pes_group),
         illegal = ifelse(community == "B", 
                          "no_PES", illegal)
  ) |> 
  mutate(period = "cp4", .before = 1,
         pes = ifelse(pes_group == "Yes", 1, 0),
         pes = ifelse(community == "A", 1, pes),
         police = ifelse(police == "No" | police == "no_PES", 0, 1),
         illegal = ifelse(
           is.na(illegal),
           rbinom(4, 1, 0.67),
           illegal
         ), # Many members in community F left illegal NA
         illegal = ifelse(illegal == "Y", 1, 0),
         pes_payment = 50 * pes,
         harvest_value = ifelse(harvest_value == 0, 1, harvest_value),
         harvest_payment = harvest_value * (1-pes)) |>
  group_by(community) |>
  mutate(
    illegal_n = sum(illegal),
    audit = ifelse(
      pes == 1 & police == 0 & !is.na(pes),
      rbinom(n(), 1, 0.1 * illegal_n),
      0
    ),
    audit = ifelse(sum(audit) > 0, 1, 0),
    additional = case_when(illegal_n == 0 & police == 0 & split_rule == 'equal' ~ pes_payment,
                           illegal_n == 0 & police == 0 & split_rule == 'compliers_only_equal' & illegal == 0 ~ pes_payment,
                           illegal_n == 0 & police == 0 & split_rule == 'compliers_only_equal' & illegal == 1 ~ 0,
                           illegal_n == 0 & police == 0 & split_rule == 'remove_benefits_illegal_equal' & illegal == 0 ~ pes_payment*n()/(n()-illegal_n),
                           illegal_n == 0 & police == 0 & split_rule == 'remove_benefits_illegal_equal' & illegal == 1 ~ 0,
                           illegal_n == 0 & police == 0 & split_rule == 'remove_benefits_illegal_proportional' & illegal == 0 ~  pes_payment*n() * harvest_value / sum(harvest_value*(1-illegal)),
                           illegal_n == 0 & police == 0 & split_rule == 'remove_benefits_illegal_proportional' & illegal == 1 ~  0,
                           illegal_n == 0 & police == 1 & split_rule %in% c('equal', 'compliers_only_equal', 'remove_benefits_illegal_equal') ~ pes_payment,
                           illegal_n == 0 & police == 1 & split_rule == 'remove_benefits_illegal_proportional' ~ pes_payment * harvest_value/sum(harvest_value),

                           audit == 0 & illegal_n > 0 & police == 0 & split_rule == 'equal' ~ pes_payment,
                           audit == 0 & illegal_n > 0 & police == 0 & split_rule == 'compliers_only_equal' & illegal == 0 ~ pes_payment,
                           audit == 0 & illegal_n > 0 & police == 0 & split_rule == 'compliers_only_equal' & illegal == 1 ~ 0,
                           audit == 0 & illegal_n > 0 & police == 0 & split_rule == 'remove_benefits_illegal_equal' & illegal == 0 ~ pes_payment*n()/(n()-illegal_n),
                           audit == 0 & illegal_n > 0 & police == 0 & split_rule == 'remove_benefits_illegal_equal' & illegal == 1 ~ 0,
                           audit == 0 & illegal_n > 0 & police == 0 & split_rule == 'remove_benefits_illegal_proportional' & illegal == 0 ~  pes_payment*n() * harvest_value / sum(harvest_value*(1-illegal)),
                           audit == 0 & illegal_n > 0 & police == 0 & split_rule == 'remove_benefits_illegal_proportional' & illegal == 1 ~  0,
                           audit == 0 & illegal_n > 0 & police == 1 & split_rule %in% c('equal', 'compliers_only_equal', 'remove_benefits_illegal_equal') ~ pes_payment,
                           audit == 0 & illegal_n > 0 & police == 1 & split_rule == 'remove_benefits_illegal_proportional' ~ pes_payment * harvest_value/sum(harvest_value),

                           audit == 1 & illegal_n > 0  ~ -harvest_payment - 70,
                           is.na(split_rule) | split_rule == "no_PES" ~ 0)
  ) |>
  ungroup() |>
  mutate(earning = round(70 + harvest_payment + additional + illegal * harvest_value
                         - 5*police,
                         2)) |>
  relocate(`id-number`, period, earning)


all_cp <- bind_rows(df_0, df_1, df_2, df_3, df_4) |> 
  mutate(period = case_when(period == "cp0" ~ "Baseline",
                            period == "cp1" ~ "PES",
                            period == "cp2" ~ "PES + Ill. Harvest",
                            period == "cp3" ~ "Community PES",
                            period == "cp4" ~ "Community PES + Ill. Harvest"),
         period = factor(period,
                         levels = 
                           c("Baseline", "PES", "PES + Ill. Harvest",
                             "Community PES", "Community PES + Ill. Harvest"))
         )



# summary -----------------------------------------------------------------

# CP0 — baseline harvest rate by card value
cp0_harvest <- all_cp |>
  filter(period == "Baseline") |>
  summarise(
    n = n(),
    harvest_rate = mean(harvest %in% TRUE, na.rm = TRUE)
  )

# CP1 — PES adoption rate
cp1_pes <- all_cp |>
  filter(period == "PES") |>
  summarise(
    n = n(),
    pes_rate = mean(pes %in% TRUE, na.rm = TRUE)
  )

# CP2 — PES adoption + illegal rate
cp2_rates <- all_cp |>
  filter(period == "PES + Ill. Harvest") |>
  summarise(
    n = n(),
    pes_rate = mean(pes %in% TRUE, na.rm = TRUE),
    illegal_rate = mean(illegal %in% TRUE, na.rm = TRUE)
  )

# CP3 — community PES choice and split-rule distribution
cp3_summary <- all_cp |>
  filter(period == "Community PES") |>
  summarise(
    n = n(),
    pes_group_rate = mean(pes %in% TRUE, na.rm = TRUE)
  )

cp3_split_dist <- all_cp |>
  filter(period == "Community PES") |>
  count(split_rule, sort = TRUE)

# CP4 — community PES, policing, illegal, split rules
cp4_summary <- all_cp |>
  filter(period == "Community PES + Ill. Harvest") |>
  summarise(
    n = n(),
    pes_group_rate = mean(pes %in% TRUE, na.rm = TRUE),
    policing_rate  = mean(police %in% TRUE, na.rm = TRUE),
    illegal_rate   = mean(illegal %in% TRUE, na.rm = TRUE)
  )

cp4_split_dist <- all_cp |>
  filter(period == "Community PES + Ill. Harvest") |>
  count(split_rule, sort = TRUE)


# PES vs harvest_value (CP1–CP2)
pes_vs_harvestValue <- all_cp |>
  filter(period %in% c("PES","PES + Ill. Harvest"), !is.na(harvest_value)) |>
  group_by(period, harvest_value) |>
  summarise(pes_rate = mean(pes %in% TRUE, na.rm = TRUE), n = n(), .groups="drop")

# Illegal vs harvest_value (CP2 & CP4)
illegal_vs_harvestValue <- all_cp |>
  filter(period %in% c("PES + Ill. Harvest",
                       "Community PES + Ill. Harvest"), !is.na(harvest_value)) |>
  group_by(period, harvest_value) |>
  summarise(illegal_rate = mean(illegal %in% TRUE, na.rm = TRUE), n = n(), .groups="drop")


# Community-level views (CP3 & CP4)
comm_cp3 <- all_cp |>
  filter(period == "Community PES") |>
  group_by(community) |>
  summarise(
    n = n(),
    pes_group_rate = mean(pes %in% TRUE, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  arrange(community)


comm_cp3_split <- all_cp |>
  filter(period == "Community PES") |>
  select(community, pes, split_rule) |> 
  distinct() |>
  count(split_rule) |> 
  arrange(-n)

comm_cp4 <- all_cp |>
  filter(period == "Community PES + Ill. Harvest") |>
  group_by(community) |>
  summarise(
    n = n(),
    pes_group_rate = mean(pes %in% TRUE, na.rm = TRUE),
    policing_rate  = mean(police %in% TRUE, na.rm = TRUE),
    illegal_rate   = mean(illegal %in% TRUE, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  arrange(community)

comm_cp4_split <- all_cp |>
  filter(period == "Community PES + Ill. Harvest") |>
  select(community, pes, split_rule) |> 
  distinct() |>
  count(split_rule) |> 
  arrange(-n)




# viz summary ------------------------------------------------------------


illegal_vs_harvestValue |> 
  ggplot(aes(x = harvest_value,
             y = illegal_rate)) +
  geom_col(aes(fill = period),
           position = position_dodge2(preserve = "single"),
           alpha = .67) +
  geom_smooth(method = lm, se = F,
              aes(color = period)) +
  scale_color_tableau() +
  scale_fill_tableau() +
  scale_y_percent() +
  labs(x = "Harvest Value",
       y = "Illegal Rate",
       fill = "Period",
       color = "Period") +
  theme_ipsum() +
  theme(axis.title.x = element_text(size = rel(2)),
        axis.title.y = element_text(angle = 0,
                                    size = rel(2)),
  )

# 💵 Distribution of Earnings Across Periods
all_cp %>%
  ggplot(aes(y = period, x = earning, fill = period)) +
  geom_boxplot(alpha = 0.8) +
  labs(
    title = "Distribution of Earnings Across Contract Periods",
    x = "Contract Period",
    y = "Earnings ($)"
  ) +
  scale_fill_tableau() +
  theme_ipsum(base_size = 13) +
  theme(legend.position = "none")


# 🪵 Effect of Harvest Value on PES Decision
all_cp %>%
  filter(period %in% c("PES", "PES + Ill. Harvest")) %>%
  ggplot(aes(x = harvest_value, y = pes, color = period)) +
  geom_jitter(width = 0.3, height = 0.03, alpha = 0.5) +
  geom_smooth(method = "glm", 
              method.args = list(family = binomial), 
              se = FALSE) +
  scale_y_percent() +
  labs(
    title = "PES Adoption vs. Harvest Value",
    x = "Harvest Value (Card × $10)",
    y = "Proportion of PES"
  ) +
  scale_color_tableau() +
  theme_ipsum(base_size = 13)

# 💡 Community Policing Impact
all_cp %>%
  filter(period == "Community PES + Ill. Harvest") %>%
  mutate(police = ifelse(police == 0, "No Policing", "Policing")) |> 
  group_by(police) %>%
  summarise(mean_earning = mean(earning, na.rm = TRUE)) %>%
  ggplot(aes(x = factor(police), y = mean_earning, fill = factor(police))) +
  geom_col() +
  labs(
    title = "Average Earnings by Policing Decision (CP4)",
    x = "",
    y = "Mean Earnings ($)"
  ) +
  scale_fill_tableau() +
  theme_ipsum(base_size = 13) +
  theme(legend.position = "none")


# data frame summary ------------------------------------------------------

# 👥 Community-Level Insights (CP3–CP4)
comm_summary <- all_cp %>%
  filter(period %in% c("Community PES", "Community PES + Ill. Harvest")) %>%
  group_by(period, community) %>%
  summarise(
    n = n(),
    mean_earning = mean(earning, na.rm = TRUE),
    pes_rate = mean(pes, na.rm = TRUE),
    illegal_rate = mean(illegal, na.rm = TRUE),
    policing_rate = mean(police, na.rm = TRUE)
  )


# 📊 Split Rule Preferences (CP3 & CP4)
df_rules <- all_cp %>%
  ungroup() |> 
  select(-`id-number`) |> 
  filter(period %in% c("Community PES", "Community PES + Ill. Harvest"), 
         !is.na(split_rule)) %>%
  count(period, split_rule) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  arrange(period, desc(n))

# 🎯 Top vs. Bottom Earners
all_cp %>%
  group_by(`id-number`) %>%
  summarise(total_earning = sum(earning, na.rm = TRUE)) %>%
  arrange(desc(total_earning)) %>%
  mutate(rank = row_number()) %>%
  slice_head(n = 10)


# lottery -----------------------------------------------------------------


sum <- all_cp |> 
  group_by(`id-number`) |> 
  summarise(real_earning = round(sum(earning, na.rm = T) / 100))


set.seed(1)
sum |> 
  slice_sample(n = 2)


