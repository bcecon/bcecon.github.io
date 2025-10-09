# ECON 340-01 - Money Growing on Trees ------------------------------------

# Experiment Date: 2025-10-06
# Instructor: Byeong-Hak Choe

# Packages and Raw Data ---------------------------------------------------

library(tidyverse)
library(janitor)
library(stringr)
library(gsheet)
library(hrbrthemes)
library(ggthemes)
library(lubridate)
library(readxl)
library(lubridate)

df_0 <- read_csv("https://bcdanl.github.io/data/econ-340-redd-exp-cp0-2025-1006.csv")

df_1 <- read_csv("https://bcdanl.github.io/data/econ-340-redd-exp-cp1-2025-1006.csv")

df_2 <- read_csv("https://bcdanl.github.io/data/econ-340-redd-exp-cp2-2025-1006.csv")

df_3 <- read_csv("https://bcdanl.github.io/data/econ-340-redd-exp-cp3-2025-1006.csv") |> 
  relocate(community, .after = 2)

df_4 <- read_csv("https://bcdanl.github.io/data/econ-340-redd-exp-cp4-2025-1006.csv") |> 
  relocate(community, .after = 2)


# CP0 (Baseline) ----------------------------------------------------------
df_0 <- df_0 |>
  mutate(
    harvest = ifelse(harvest == "No", 0, 1),
    earning = 70 + harvest_value * harvest
  ) |>
  relocate(earning, .after = `submission-time`)



# CP1 (PES) ---------------------------------------------------------------
df_1 <- df_1 |>
  mutate(
    pes_payment = 50 * pes,
    harvest_payment = harvest_value * (1 - pes),
    earning = 70 + pes_payment + harvest_payment
  ) |>
  relocate(earning, .after = `submission-time`)


# CP2 (PES + Illegal) -----------------------------------------------------
# If audited and cheated: 
  # lose PES (50), lose illegal harvest value, and pay 70 fine.

set.seed(1)
df_2 <- df_2 |>
  ungroup() |>
  mutate(     
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
  relocate(earning, .after = `submission-time`)


# CP3 (Community PES) -----------------------------------------------------
# Assumes: if community joins PES, 
  # every member has a PES “pot” (50 each) to split by chosen rule.

df_3 <- df_3 |>
  mutate(
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
  relocate(earning, .after = `submission-time`)



# CP4 (Community + Illegal) -----------------------------------------------
# The randomization with set.seed(1)
  # resulted in the CP4 community with one illegal harvester 
  # being caught during the audit.
# set.seed(1)

set.seed(2)
df_4_v1 <- df_4 |>
  mutate(
    pes = ifelse(pes_group == "PES", 1, 0),
    pes_payment = 50 * pes,
    harvest_payment = harvest_value * (1 - pes)
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
    earning = round(70 + harvest_payment + additional + illegal * harvest_value - 5 * police, 2),
    earning = ifelse(audit == 1, 0, earning)
  ) |>
  relocate(earning, .after = `submission-time`)


set.seed(1)
df_4_v2 <- df_4 |>
  mutate(
    pes = ifelse(pes_group == "PES", 1, 0),
    pes_payment = 50 * pes,
    harvest_payment = harvest_value * (1 - pes)
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
    earning = round(70 + harvest_payment + additional + illegal * harvest_value - 5 * police, 2),
    earning = ifelse(audit == 1, 0, earning)
  ) |>
  relocate(earning, .after = `submission-time`)



# Bind All ----------------------------------------------------------------

all_cp <- bind_rows(df_0, df_1, df_2, df_3, df_4_v1) |>
  mutate(
    diff = earning - (70 + harvest_value),
    .after = earning
  ) |> 
  mutate(
    period = factor(
      period,
      levels = c("Baseline", "PES", "PES + Ill. Harvest", "Community PES", "Community PES + Ill. Harvest")
    )
  )

all_cp_v2 <- bind_rows(df_0, df_1, df_2, df_3, df_4_v2) |>
  mutate(
    diff = earning - (70 + harvest_value),
    .after = earning
  ) |> 
  mutate(
    period = factor(
      period,
      levels = c("Baseline", "PES", "PES + Ill. Harvest", "Community PES", "Community PES + Ill. Harvest")
    )
  )


# Checking the number of participations
id_sum <- all_cp |>
  count(`id-number`)


# Viz ------------------------------------------------------------

# Illegal vs harvest_value (CP2 & CP4)
all_cp |>
  filter(period %in% c("PES + Ill. Harvest",
                       "Community PES + Ill. Harvest"), !is.na(harvest_value)) |>
  group_by(period, harvest_value) |>
  summarise(illegal_rate = mean(illegal %in% TRUE, na.rm = TRUE), n = n(), .groups="drop") |> 
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

# Relationship between Earning and Harvest Value Across Periods
all_cp |>
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

# Distribution of Earning Differences Across Periods
all_cp |>
  filter(period != "Baseline") |> 
  ggplot(aes(y = period, x = diff, fill = period)) +
  geom_boxplot(alpha = 0.8, outlier.colour = 'darkred', outlier.size = rel(2)) +
  # geom_vline(xintercept = 0, color = 'darkred', linetype = 2, linewidth = 1.25) +
  labs(
    title = "What If You (and Your Community) Joined the PES Program?",
    subtitle = expression(
      paste(
        "Distribution of Earnings Differences Between PES Participants and Their Counterfactual Non-PES Incomes",
      )
    ),
    y = "",
    x = "(PES Earnings) - (Non-PES Earnings)",
    caption = "(Non-PES Earnings) = (Farming Income) + (Harvest Value)\n(PES earnings) become negative when an audit identifies illegal harvesting."
  ) +
  scale_fill_tableau() +
  scale_x_continuous(breaks = seq(-175,75,25)) +
  theme_ipsum(base_size = 13) +
  theme(legend.position = "none",
        axis.title.x = element_text(size = rel(1.25)))



# Distribution of Earning Differences Across Periods - v2
  # More community gets audited
all_cp_v2 |>
  filter(period != "Baseline") |> 
  ggplot(aes(y = period, x = diff, fill = period)) +
  geom_boxplot(alpha = 0.8, outlier.colour = 'darkred', outlier.size = rel(2)) +
  # geom_vline(xintercept = 0, color = 'darkred', linetype = 2, linewidth = 1.25) +
  labs(
    title = "What If You (and Your Community) Joined the PES Program?",
    subtitle = expression(
      paste(
        "Distribution of Earnings Differences Between PES Participants and Their Counterfactual Non-PES Incomes",
      )
    ),
    y = "",
    x = "(PES Earnings) - (Non-PES Earnings)",
    caption = "(Non-PES Earnings) = (Farming Income) + (Harvest Value)\n(PES earnings) become negative when an audit identifies illegal harvesting."
  ) +
  scale_fill_tableau() +
  scale_x_continuous(breaks = seq(-175,75,25)) +
  theme_ipsum(base_size = 13) +
  theme(legend.position = "none",
        axis.title.x = element_text(size = rel(1.25)))


# Effect of Harvest Value on PES Decision
all_cp |>
  filter(period %in% c("PES", "PES + Ill. Harvest")) |>
  ggplot(aes(x = harvest_value, y = pes, color = period)) +
  geom_jitter(width = 0.3, height = 0.03, alpha = 0.5) +
  geom_smooth(method = "glm", 
              method.args = list(family = binomial), 
              se = FALSE) +
  scale_y_percent() +
  labs(
    title = "PES Adoption vs. Harvest Value",
    x = "Harvest Value",
    y = "Proportion of PES",
    color = "Period"
  ) +
  scale_color_tableau() +
  theme_ipsum(base_size = 13) +
  theme(axis.title.x = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5))
  )

# Community Policing Impact
all_cp |>
  filter(period == "Community PES + Ill. Harvest") |>
  mutate(police = ifelse(police == 0, "No Policing", "Policing")) |> 
  group_by(police) |>
  summarise(mean_earning = mean(earning, na.rm = TRUE),
            median_earning = median(earning, na.rm = TRUE)) |>
  ggplot(aes(x = factor(police), y = mean_earning, fill = factor(police))) +
  geom_col() +
  labs(
    title = "Average Earnings by Policing Decision (CP4)",
    x = "",
    y = "Mean Earnings ($)",
    color = "Period"
  ) +
  scale_fill_tableau() +
  theme_ipsum(base_size = 13) +
  theme(legend.position = "none")

# Community Policing Impact - v2
  # More community gets audited
all_cp_v2 |>
  filter(period == "Community PES + Ill. Harvest") |>
  mutate(police = ifelse(police == 0, "No Policing", "Policing")) |> 
  group_by(police) |>
  summarise(mean_earning = mean(earning, na.rm = TRUE),
            median_earning = median(earning, na.rm = TRUE)) |>
  ggplot(aes(x = factor(police), y = mean_earning, fill = factor(police))) +
  geom_col() +
  labs(
    title = "Average Earnings by Policing Decision (CP4)",
    x = "",
    y = "Mean Earnings ($)",
    color = "Period"
  ) +
  scale_fill_tableau() +
  theme_ipsum(base_size = 13) +
  theme(legend.position = "none")


# Data Frame Summary ------------------------------------------------------

# Community-Level Insights (CP3–CP4)
comm_summary <- all_cp |>
  filter(period %in% c("Community PES", "Community PES + Ill. Harvest")) |>
  group_by(period, community) |>
  summarise(
    n = n(),
    mean_earning = mean(earning, na.rm = TRUE),
    pes_rate = mean(pes, na.rm = TRUE),
    harvest_value = mean(harvest_value, na.rm = TRUE),
    illegal_rate = mean(illegal, na.rm = TRUE),
    policing_rate = mean(police, na.rm = TRUE)
  )

# Split Rule Preferences (CP3 & CP4)
df_rules <- all_cp |>
  ungroup() |> 
  select(-`id-number`) |> 
  filter(period %in% c("Community PES", "Community PES + Ill. Harvest"), 
         !is.na(split_rule)) |>
  count(period, split_rule) |>
  mutate(pct = round(100 * n / sum(n), 1)) |>
  arrange(period, desc(n))

# Top vs. Bottom Earners
all_cp |>
  group_by(`id-number`) |>
  summarise(total_earning = sum(earning, na.rm = TRUE)) |>
  arrange(desc(total_earning)) |>
  mutate(rank = row_number()) |>
  slice_head(n = 5)

all_cp |>
  group_by(`id-number`) |>
  summarise(total_earning = sum(earning, na.rm = TRUE)) |>
  arrange(desc(total_earning)) |>
  mutate(rank = row_number()) |>
  slice_tail(n = 5)


# Lottery -----------------------------------------------------------------

sum <- all_cp |>
  group_by(`id-number`) |>
  summarise(real_earning = round(sum(earning, na.rm = T) / 100))

set.seed(1)
sum |>
  slice_sample(n = 2)

