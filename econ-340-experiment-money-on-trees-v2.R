library(tidyverse)
library(janitor)
library(stringr)
library(gsheet)
library(hrbrthemes)
library(ggthemes)
library(lubridate)

# exporting data ----------------------------------------------------------

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




# data prep ---------------------------------------------------------------
# ----------------------------
# CP0
# ----------------------------
df_0 <- read_csv("/Users/bchoe/Documents/websites/bcdanl.github.io/data/econ-340-redd-exp-cp0-2025-1006.csv") |>
  filter(!is.na(harvest)) |>
  mutate(
    period = "cp0",
    harvest = ifelse(harvest == "No", 0, 1),
    # NOTE: if you truly want face/Joker (0) to be 0, remove the next line:
    harvest_value = ifelse(harvest_value == 0, 1, harvest_value),
    earning = 70 + harvest_value * harvest
  ) |>
  relocate(`id-number`, period, earning)

# ----------------------------
# CP1
# ----------------------------
df_1 <- read_csv("/Users/bchoe/Documents/websites/bcdanl.github.io/data/econ-340-redd-exp-cp1-2025-1006.csv") |>
  mutate(
    period = "cp1",
    pes = ifelse(pes == "PES", 1, 0),
    pes_payment = 50 * pes,
    harvest_payment = harvest_value * (1 - pes),
    earning = 70 + pes_payment + harvest_payment
  ) |>
  relocate(`id-number`, period, earning)

# ----------------------------
# CP2 (PES + Illegal)
# Audit should penalize ONLY if (pes==1 & illegal==1). 
# If audited and cheated: lose PES (50), lose illegal harvest value, and pay 70 fine.
# ----------------------------
set.seed(14454)
df_2 <- read_csv("/Users/bchoe/Documents/websites/bcdanl.github.io/data/econ-340-redd-exp-cp2-2025-1006.csv") |>
  mutate(
    period = "cp2",
    illegal = ifelse(pes == "No", "no_PES", illegal),
    illegal = ifelse(pes == "PES" & is.na(illegal), "Y", illegal),  # assumed
    pes = ifelse(pes == "PES", 1, 0),  
    illegal = ifelse(illegal == "Y", 1, 0),          # NA -> 0 here; adjust if you want otherwise
    pes_payment = 50 * pes,
    harvest_payment = harvest_value * (1 - pes),
    
    # draw audits only for PES participants (fine is an audit indicator)
    fine = ifelse(pes == 1, rbinom(n(), 1, 0.25), 0),
    
    # Base earnings: 70 + (PES if pes==1) + (legal harvest if not in PES) + (illegal harvest if cheating)
    base_earn = 70 + pes_payment + harvest_payment + illegal * harvest_value,
    
    # If audited AND cheated (pes==1 & illegal==1 & fine==1): subtract PES (50), illegal harvest, and 70 fine.
    penalty = ifelse(fine == 1 & pes == 1 & illegal == 1, 50 + harvest_value + 70, 0),
    
    earning = base_earn - penalty
  ) |>
  relocate(`id-number`, period, earning) |>
  arrange(desc(pes), desc(fine))

# ----------------------------
# CP3 (Community PES)
# Assumes: if community joins PES, every member has a PES “pot” (50 each) to split by chosen rule.
# Your scheme mixes a per-person base (50) with redistributions that sum to group total (50*n()).
# Kept your intent, but made the keeping-last-submission robust.
# ----------------------------
df_3 <- read_csv("/Users/bchoe/Documents/websites/bcdanl.github.io/data/econ-340-redd-exp-cp3-2025-1006.csv") |>
  mutate(
    # override a couple of communities per your custom choices
    split_rule = ifelse(community == "E", "floor_plus_proportional", split_rule),
    pes_group  = ifelse(community == "B", "PES", pes_group),
    
    period = "cp3",
    pes = ifelse(pes_group == "PES", 1, 0),
    pes_payment = 50 * pes,
    harvest_payment = harvest_value * (1 - pes)
  ) |>
  group_by(community) |>
  mutate(
    # weights use (harvest_value + 10) to avoid zero weight
    additional = case_when(
      pes == 0 ~ 0,  # if community didn't join PES, no PES addition
      split_rule == "equal" ~ pes_payment,  # 50 each
      split_rule == "proportional_card" ~ (harvest_value + 10) * pes_payment * n() / sum(harvest_value + 10),
      split_rule == "floor_plus_proportional" ~ 0.75 * pes_payment + 0.25 * (harvest_value + 10) * pes_payment * n() / sum(harvest_value + 10),
      split_rule == "floor_plus_needs" ~ 0.75 * pes_payment + 0.25 * (1 / (harvest_value + 10)) * pes_payment * n() / sum(1 / (harvest_value + 10)),
      is.na(split_rule) | split_rule == "no_PES" ~ 0,
      TRUE ~ 0
    )
  ) |>
  ungroup() |>
  mutate(
    earning = round(70 + harvest_payment + additional, 2)
  ) |>
  relocate(`id-number`, period, earning)

# ----------------------------
# CP4 (Community + Illegal)
# Fixes:
# - keep illegal as numeric 0/1 consistently
# - rbinom(n(), ...) not rbinom(4, ...)
# - cap audit prob at 1: pmin(0.1 * illegal_n, 1)
# - on audit: remove illegal harvest and fine everyone (–70), do NOT add PES (voided)
# ----------------------------
set.seed(14454)
df_4 <- read_csv("/Users/bchoe/Documents/websites/bcdanl.github.io/data/econ-340-redd-exp-cp4-2025-1006.csv") |>
  mutate(
    period = "cp4",
    illegal = ifelse(pes_group == "PES" & police == "Yes", "N", illegal),
    illegal = ifelse(pes_group == "No", "no_PES", illegal),
    split_rule = ifelse(pes_group == "No", "no_PES", split_rule)
  ) |>
  mutate(
    pes = ifelse(pes_group == "PES", 1, 0),
    pes_payment = 50 * pes,
    harvest_payment = harvest_value * (1 - pes),
    illegal = ifelse(illegal == "Y", 1, 0),
    police = ifelse(police == "Yes", 1, 0)
  ) |>
  group_by(community) |>
  mutate(
    illegal_n = sum(illegal),
    audit_prob = pmin(0.1 * illegal_n, 1),
    audit_draw = ifelse(pes == 1 & police == 0, rbinom(n(), 1, audit_prob), 0),
    audit = ifelse(sum(audit_draw) > 0, 1, 0),
    
    additional = case_when(
      # No PES
      pes == 0 ~ 0,
      
      # PES + Police: no illegal possible; just apply split rule
      pes == 1 & police == 1 & split_rule %in% c("equal", "compliers_only_equal", "remove_benefits_illegal_equal") ~ pes_payment,
      pes == 1 & police == 1 & split_rule == "remove_benefits_illegal_proportional" ~ pes_payment * (harvest_value + 10) / sum(harvest_value + 10),
      
      # PES, no police, NO audit → apply split rule with/without excluding illegal
      audit == 0 & pes == 1 & police == 0 & split_rule == "equal" ~ pes_payment,
      audit == 0 & pes == 1 & police == 0 & split_rule == "compliers_only_equal" ~ ifelse(illegal == 0, pes_payment, 0),
      audit == 0 & pes == 1 & police == 0 & split_rule == "remove_benefits_illegal_equal" ~ ifelse(illegal == 0, pes_payment * n() / (n() - illegal_n), 0),
      audit == 0 & pes == 1 & police == 0 & split_rule == "remove_benefits_illegal_proportional" ~ ifelse(
        illegal == 0,
        pes_payment * (harvest_value + 10) * n() / sum((harvest_value + 10) * (1 - illegal)),
        0
      ),
      
      # PES, no police, AUDIT → contracts void + illegal harvest confiscated + fine everyone 70
      audit == 1 & pes == 1 & police == 0 & split_rule == "equal" ~ pes_payment,
      audit == 1 & pes == 1 & police == 0 & split_rule == "compliers_only_equal" ~ ifelse(illegal == 0, pes_payment, - (illegal * harvest_value) - 70),
      audit == 1 & pes == 1 & police == 0 & split_rule == "remove_benefits_illegal_equal" ~ ifelse(illegal == 0, pes_payment * n() / (n() - illegal_n), - (illegal * harvest_value) - 70),
      audit == 1 & pes == 1 & police == 0 & split_rule == "remove_benefits_illegal_proportional" ~ ifelse(
        illegal == 0,
        pes_payment * (harvest_value + 10) * n() / sum((harvest_value + 10) * (1 - illegal)),
        - (illegal * harvest_value) - 70
      ),
      
      # no_PES or missing
      is.na(split_rule) | split_rule == "no_PES" ~ 0,
      
      TRUE ~ 0
    )
  ) |>
  ungroup() |>
  mutate(
    earning = round(70 + harvest_payment + additional + illegal * harvest_value - 5 * police, 2)
  ) |>
  relocate(`id-number`, period, earning)

# ----------------------------
# Bind all + nice period labels
# ----------------------------
all_cp <- bind_rows(df_0, df_1, df_2, df_3, df_4) |>
  mutate(
    period = case_when(
      period == "cp0" ~ "Baseline",
      period == "cp1" ~ "PES",
      period == "cp2" ~ "PES + Ill. Harvest",
      period == "cp3" ~ "Community PES",
      period == "cp4" ~ "Community PES + Ill. Harvest",
      TRUE ~ period
    ),
    period = factor(
      period,
      levels = c("Baseline", "PES", "PES + Ill. Harvest", "Community PES", "Community PES + Ill. Harvest")
    ),
    diff = earning - (70 + harvest_value)
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

# 💵 Relationship between Earning and Harvest Value Across Periods
all_cp %>%
  
  ggplot(aes(x = harvest_value + 70, y = earning, color = period)) +
  geom_point(alpha = 0.8) +
  geom_smooth(se=F, method = lm) +
  labs(
    title = "Relationship between Harvest Value and Earning\nAcross Contract Periods",
    x = "Harvest Value ($)",
    y = "Earnings ($)",
    color = "Period"
  ) +
  scale_color_tableau() +
  theme_ipsum(base_size = 13)

# 💵 Distribution of Harvest Value Across Periods
all_cp %>%
  ggplot(aes(y = period, x = harvest_value+70, fill = period)) +
  geom_boxplot(alpha = 0.8) +
  labs(
    title = "Distribution of Farming+Harvest Values",
    y = "",
    x = "Harvest Values ($)"
  ) +
  scale_fill_tableau() +
  theme_ipsum(base_size = 13) +
  theme(legend.position = "none")

# 💵 Distribution of Earnings Across Periods
all_cp %>%
  ggplot(aes(y = period, x = earning, fill = period)) +
  geom_boxplot(alpha = 0.8) +
  labs(
    title = "Distribution of Earnings",
    y = "",
    x = "Earnings ($)"
  ) +
  scale_fill_tableau() +
  theme_ipsum(base_size = 13) +
  theme(legend.position = "none")


# Reshape data into long format
all_cp %>%
  mutate(farming_harvest = harvest_value + 70) %>%
  select(period, farming_harvest, earning) %>%
  pivot_longer(cols = c(farming_harvest, earning),
               names_to = "type", values_to = "value") |> 
  ggplot(aes(y = period, x = value, fill = type)) +
  geom_boxplot(alpha = 0.8, position = position_dodge(width = 0.8)) +
  labs(
    title = "Distribution of Farming+Harvest Values vs. Earnings by Period",
    y = "",
    x = "Value ($)",
    fill = ""
  ) +
  scale_fill_tableau(labels = c("Farming + Harvest", "Earnings")) +
  theme_ipsum(base_size = 13) +
  theme(legend.position = "top")


# 💵 Distribution of Earnings Across Periods
all_cp %>%
  # filter(diff >-51) |> 
  filter(period != "Baseline") |> 
  ggplot(aes(y = period, x = diff, fill = period)) +
  geom_boxplot(alpha = 0.8) +
  geom_vline(xintercept = 0, color = 'darkred', linetype = 2, linewidth = 1.25) +
  labs(
    title = "Distribution of Earnings Differences: PES vs. Non-PES Participants",
    y = "",
    x = "(PES Earnings) - (Farming + Harvest)"
  ) +
  scale_fill_tableau() +
  theme_ipsum(base_size = 13) +
  theme(legend.position = "none",
        axis.title.x = element_text(size = rel(1.25)))


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
  summarise(mean_earning = mean(earning, na.rm = TRUE),
            median_earning = median(earning, na.rm = TRUE)) %>%
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


# sum <- all_cp |> 
#   group_by(`id-number`) |> 
#   summarise(real_earning = round(sum(earning, na.rm = T) / 100))
# 
# 
# set.seed(1)
# sum |> 
#   slice_sample(n = 2)


