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

df_0 <- gsheet2tbl(urls[1]) |> 
  mutate(period = "cp0", .before = 1,
         harvest = ifelse(harvest == "No", 0, 1),
         harvest_value = ifelse(harvest_value == 0, 1, harvest_value),
         earning = 70 + harvest_value * harvest) |> 
  relocate(`id-number`, period, earning)

df_1 <- gsheet2tbl(urls[2]) |> 
  mutate(period = "cp1", .before = 1,
         pes = ifelse(pes == "No", 0, 1),
         pes_payment = 50 * pes,
         harvest_value = ifelse(harvest_value == 0, 1, harvest_value),
         harvest_payment = harvest_value * (1-pes),
         earning = 70 + pes_payment + harvest_payment) |> 
  relocate(`id-number`, period, earning)

set.seed(14454)
df_2 <- gsheet2tbl(urls[3]) |> 
  mutate(period = "cp2", .before = 1,
         pes = ifelse(pes == "No", 0, 1),
         illegal = ifelse(illegal == "N", 0, 1),
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
                                is.na(split_rule) ~ 0)
         ) |> 
  ungroup() |> 
  mutate(earning = round(70 + harvest_payment + additional, 2)) |> 
  relocate(`id-number`, period, earning)


set.seed(14454)
df_4 <- gsheet2tbl(urls[5]) |>
  mutate(period = "cp4", .before = 1,
         pes = ifelse(pes_group == "No", 0, 1),
         police = ifelse(police == "Yes", 1, 0),
         illegal = ifelse(illegal == "N", 0, 1),
         pes_payment = 50 * pes,
         harvest_value = ifelse(harvest_value == 0, 1, harvest_value),
         harvest_payment = harvest_value * (1-pes)) |>
  group_by(community) |>
  mutate(
    illegal_n = sum(illegal),
    audit = ifelse(
      pes == 1 & police == 0,
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
                           is.na(split_rule) ~ 0)
  ) |>
  ungroup() |>
  mutate(earning = round(70 + harvest_payment + additional + illegal * harvest_value
                         - 5*police,
                         2)) |>
  relocate(`id-number`, period, earning)


all_cp <- bind_rows(df_0, df_1, df_2, df_3, df_4)



# summary -----------------------------------------------------------------

# CP0 — baseline harvest rate by card value
cp0_harvest <- all_cp |>
  filter(period == "cp0") |>
  summarise(
    n = n(),
    harvest_rate = mean(harvest %in% TRUE, na.rm = TRUE)
  )

# CP1 — PES adoption rate
cp1_pes <- all_cp |>
  filter(period == "cp1") |>
  summarise(
    n = n(),
    pes_rate = mean(pes %in% TRUE, na.rm = TRUE)
  )

# CP2 — PES adoption + illegal rate
cp2_rates <- all_cp |>
  filter(period == "cp2") |>
  summarise(
    n = n(),
    pes_rate = mean(pes %in% TRUE, na.rm = TRUE),
    illegal_rate = mean(illegal %in% TRUE, na.rm = TRUE)
  )

# CP3 — community PES choice and split-rule distribution
cp3_summary <- all_cp |>
  filter(period == "cp3") |>
  summarise(
    n = n(),
    pes_group_rate = mean(pes %in% TRUE, na.rm = TRUE)
  )

cp3_split_dist <- all_cp |>
  filter(period == "cp3") |>
  count(split_rule, sort = TRUE)

# CP4 — community PES, policing, illegal, split rules
cp4_summary <- all_cp |>
  filter(period == "cp4") |>
  summarise(
    n = n(),
    pes_group_rate = mean(pes %in% TRUE, na.rm = TRUE),
    policing_rate  = mean(police %in% TRUE, na.rm = TRUE),
    illegal_rate   = mean(illegal %in% TRUE, na.rm = TRUE)
  )

cp4_split_dist <- all_cp |>
  filter(period == "cp4") |>
  count(split_rule, sort = TRUE)


# PES vs harvest_value (CP1–CP2)
pes_vs_harvestValue <- all_cp |>
  filter(period %in% c("cp1","cp2"), !is.na(harvest_value)) |>
  group_by(period, harvest_value) |>
  summarise(pes_rate = mean(pes %in% TRUE, na.rm = TRUE), n = n(), .groups="drop")

# Illegal vs harvest_value (CP2 & CP4)
illegal_vs_harvestValue <- all_cp |>
  filter(period %in% c("cp2","cp4"), !is.na(harvest_value)) |>
  group_by(period, harvest_value) |>
  summarise(illegal_rate = mean(illegal %in% TRUE, na.rm = TRUE), n = n(), .groups="drop")


# Community-level views (CP3 & CP4)
comm_cp3 <- all_cp |>
  filter(period == "cp3") |>
  group_by(community) |>
  summarise(
    n = n(),
    pes_group_rate = mean(pes %in% TRUE, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  arrange(community)


comm_cp3_split <- all_cp |>
  filter(period == "cp3") |>
  select(community, pes, split_rule) |> 
  distinct() |>
  count(split_rule) |> 
  arrange(-n)

comm_cp4 <- all_cp |>
  filter(period == "cp4") |>
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
  filter(period == "cp4") |>
  select(community, pes, split_rule) |> 
  distinct() |>
  count(split_rule) |> 
  arrange(-n)




# viz summary ------------------------------------------------------------


illegal_vs_harvestValue |> 
  ggplot() +
  geom_col(aes(x = harvest_value,
               y = illegal_rate,
               fill = period),
           position = "dodge") +
  scale_fill_tableau() +
  scale_y_percent() +
  labs(x = "Harvest Value",
       y = "Illegal Rate",
       fill = "Period") +
  theme_ipsum() +
  theme(axis.title.x = element_text(size = rel(2)),
        axis.title.y = element_text(angle = 0,
                                    size = rel(2)),
  )

# 💵 Distribution of Earnings Across Periods
all_cp %>%
  ggplot(aes(x = period, y = earning, fill = period)) +
  geom_boxplot(alpha = 0.8) +
  labs(
    title = "Distribution of Earnings Across Contract Periods",
    x = "Contract Period",
    y = "Earnings ($)"
  ) +
  theme_ipsum(base_size = 13) +
  theme(legend.position = "none")


# 🪵 Effect of Harvest Value on PES Decision
all_cp %>%
  filter(period %in% c("cp1", "cp2")) %>%
  ggplot(aes(x = harvest_value, y = pes, color = period)) +
  geom_jitter(width = 0.3, height = 0.03, alpha = 0.5) +
  geom_smooth(method = "glm", method.args = list(family = binomial), se = FALSE) +
  labs(
    title = "PES Adoption vs. Harvest Value",
    x = "Harvest Value (Card × $10)",
    y = "P(PES = 1)"
  ) +
  theme_minimal(base_size = 13)

# 💡 Community Policing Impact
all_cp %>%
  filter(period == "cp4") %>%
  group_by(police) %>%
  summarise(mean_earning = mean(earning, na.rm = TRUE)) %>%
  ggplot(aes(x = factor(police), y = mean_earning, fill = factor(police))) +
  geom_col() +
  labs(
    title = "Average Earnings by Policing Decision (CP4)",
    x = "Police (1 = Yes)",
    y = "Mean Earnings ($)"
  ) +
  scale_fill_tableau() +
  theme_ipsum(base_size = 13) +
  theme(legend.position = "none")


# data frame summary ------------------------------------------------------



# 👥 Community-Level Insights (CP3–CP4)
comm_summary <- all_cp %>%
  filter(period %in% c("cp3", "cp4")) %>%
  group_by(period, community) %>%
  summarise(
    n = n(),
    mean_earning = mean(earning, na.rm = TRUE),
    pes_rate = mean(pes, na.rm = TRUE),
    illegal_rate = mean(illegal, na.rm = TRUE),
    policing_rate = mean(police, na.rm = TRUE)
  )


# 📊 Split Rule Preferences (CP3 & CP4)
all_cp %>%
  filter(period %in% c("cp3", "cp4"), !is.na(split_rule)) %>%
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


