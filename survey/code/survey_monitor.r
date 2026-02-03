library(data.table)
library(ggplot2)
library(knitr)

data <- fread("C:\\Users\\rhuang11\\Desktop\\University of Chicago Booth_SNAP Benefits Survey Tracker W1_Raw Data_12.31.25.csv")
data <- data[-1]
data[, lf_random := fifelse(`LF RANDOMIZATION_1` == "GROUP A", 1L, 0L)]

theme_set(
  theme_classic() +
    theme(
      axis.text.x  = element_text(size = 10),
      axis.text.y  = element_text(size = 10),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      legend.position = "bottom",
      legend.title = element_blank()
    )
)

# ----------------
# region Q1
# ----------------
q1_x_map <- c(
  "Never" = 0,
  "1 time" = 1,
  "2-3 times" = 2,
  "4-6 times" = 3,
  "1 time per day" = 4,
  "2 times per day" = 5,
  "3 times per day or more" = 6
)

total_respondents <- nrow(data)

q1_codes <- unname(q1_x_map[as.character(data$Q1)])
q1_codes[is.na(data$Q1)] <- 7

q1_counts <- as.data.table(table(q1_codes, useNA = "ifany"))
setnames(q1_counts, c("q1_code", "N"))
q1_counts[, q1_code := as.numeric(as.character(q1_code))]
setorder(q1_counts, q1_code)
q1_counts[, share := N / total_respondents]
q1_counts <- rbind(q1_counts, data.table(q1_code = 7, N = 0, share = 0), fill = TRUE)

legend_lab <- sprintf("Total respondents: %s", total_respondents)

p <- ggplot(q1_counts, aes(x = q1_code, y = share)) +
  geom_col(fill = "skyblue", width = 0.8) +
  geom_text(aes(label = sprintf("%.0f", share * total_respondents)), vjust = -0.25, size = 3) +
  scale_x_continuous(
    breaks = 0:7,
    labels = c("Never", "1", "2-3", "4-6", "1/day", "2/day", "3+/day", "N/A")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) + # mult means percentage expansion
  labs(
    x = "Q1: Number of times purchased soda unscanned in past week - all respondents",
    y = "Share of total responses"
  ) +
  geom_col(aes(fill = legend_lab), alpha = 1, width = 0.8) +
  scale_fill_manual(values = setNames("skyblue", legend_lab)) +
  theme(legend.position = "bottom")
ggsave("q1_soda_purchase_distribution.pdf", p, width = 8, height = 5, units = "in")
# endregion Q1


# ----------------
# region Q1 fl oz
# ----------------
q1_codes <- unname(q1_x_map[as.character(data$Q1)])
q1_codes[is.na(data$Q1)] <- 7

q1_counts <- as.data.table(table(q1_codes, useNA = "ifany"))
setnames(q1_counts, c("q1_code", "N"))
q1_counts[, q1_code := as.numeric(as.character(q1_code))]
setorder(q1_counts, q1_code)
q1_counts[, share := N / total_respondents]
q1_counts <- rbind(q1_counts, data.table(q1_code = 7, N = 0, share = 0), fill = TRUE)

legend_lab <- sprintf("Total respondents: %s", total_respondents)

p <- ggplot(q1_counts, aes(x = q1_code, y = share)) +
  geom_col(fill = "skyblue", width = 0.8) +
  geom_text(aes(label = sprintf("%.0f", share * total_respondents)), vjust = -0.25, size = 3) +
  scale_x_continuous(
    breaks = 0:7,
    labels = c("Never", "12 oz", "24-36 oz", "48-72 oz", "12 oz/day", "24 oz/day", "36+ oz/day", "N/A")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    x = "Q1: Number of times purchased soda unscanned in past week - all respondents",
    y = "Share of total responses"
  ) +
  geom_col(aes(fill = legend_lab), alpha = 1, width = 0.8) +
  scale_fill_manual(values = setNames("skyblue", legend_lab))
ggsave("q1_soda_purchase_oz_distribution.pdf", p, width = 8, height = 5, units = "in")
# endregion Q1 fl oz


# ----------------
# region lf randomization
# ----------------
lf_random_counts <- data[, .N, by = lf_random][order(lf_random)]
lf_random_counts[, share := N / total_respondents]

legend_lab <- sprintf("Total respondents: %s", total_respondents)

p <- ggplot(lf_random_counts, aes(x = lf_random, y = share)) +
  geom_col(fill = "skyblue", width = 0.5) +
  geom_text(aes(label = sprintf("%.0f", share * total_respondents)), vjust = -0.25, size = 3) +
  scale_x_continuous(breaks = c(0, 1), labels = c("Group B", "Group A")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(x = "Group randomization", y = "Share of total responses") +
  geom_col(aes(fill = legend_lab), alpha = 1, width = 0.5) +
  scale_fill_manual(values = setNames("skyblue", legend_lab))
ggsave("lf_randomization_distribution.pdf", p, width = 7, height = 5, units = "in")
# endregion lf randomization


# ----------------
# region Q2 (Group A vs B)
# ----------------
x_map <- c(
  "Somewhat oppose" = 0,
  "Strongly oppose" = 1,
  "Neither oppose nor support" = 2,
  "Somewhat support" = 3,
  "Strongly support" = 4
)
width <- 0.45

dt_a <- data[!is.na(`LF RANDOMIZATION_1`)]
dt_b <- data[!is.na(`LF RANDOMIZATION_2`)]

q2_codes <- unname(x_map[as.character(dt_a$Q2)])
q2_codes[is.na(dt_a$Q2)] <- 5

q13_codes <- unname(x_map[as.character(dt_b$Q13)])
q13_codes[is.na(dt_b$Q13)] <- 5

q2_counts <- as.data.table(table(q2_codes, useNA = "ifany"))
setnames(q2_counts, c("code", "N"))
q2_counts[, code := as.numeric(as.character(code))]
setorder(q2_counts, code)
q2_counts[, share := N / nrow(dt_a)]
q2_counts[, x := code - width/2]
q2_counts[, grp := sprintf("Group A - %s responses", nrow(dt_a))]
q2_counts[, nlab := share * nrow(dt_a)]
q2_counts <- rbind(q2_counts, data.table(code = 5, N = 0, share = 0, x = 5, grp = q2_counts$grp[1], nlab = 0), fill = TRUE)

q13_counts <- as.data.table(table(q13_codes, useNA = "ifany"))
setnames(q13_counts, c("code", "N"))
q13_counts[, code := as.numeric(as.character(code))]
setorder(q13_counts, code)
q13_counts[, share := N / nrow(dt_b)]
q13_counts[, x := code + width/2]
q13_counts[, grp := sprintf("Group B - %s responses", nrow(dt_b))]
q13_counts[, nlab := share * nrow(dt_b)]
q13_counts <- rbind(q13_counts, data.table(code = 5, N = 0, share = 0, x = 5, grp = q13_counts$grp[1], nlab = 0), fill = TRUE)

plot_dt <- rbind(q2_counts, q13_counts, fill = TRUE)

p <- ggplot(plot_dt, aes(x = x, y = share, fill = grp)) +
  geom_col(width = width) +
  geom_text(aes(label = sprintf("%.0f", nlab)), vjust = -0.25, size = 3) +
  scale_fill_manual(values = setNames(c("skyblue", "lightgreen"), unique(plot_dt$grp))) +
  scale_x_continuous(
    breaks = 0:5,
    labels = c("Somewhat oppose", "Strongly oppose", "Neither", "Somewhat support", "Strongly support", "N/A")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    x = "Q2 & 13: SNAP soda restriction policy support (Group A & B)",
    y = "Share of total responses within group"
  ) +
  theme(axis.text.x = element_text(size = 8))
ggsave("q2_snap_soda_policy_support_distribution_a_b.pdf", p, width = 8, height = 5, units = "in")
# endregion Q2


# ----------------
# region Q2 agg
# ----------------
q2_list  <- unname(x_map[as.character(data[!is.na(`LF RANDOMIZATION_1`)]$Q2)])
q2_list[is.na(data[!is.na(`LF RANDOMIZATION_1`)]$Q2)] <- 5

q13_list <- unname(x_map[as.character(data[!is.na(`LF RANDOMIZATION_2`)]$Q13)])
q13_list[is.na(data[!is.na(`LF RANDOMIZATION_2`)]$Q13)] <- 5

concated <- c(q2_list, q13_list)

concated_counts <- as.data.table(table(concated, useNA = "ifany"))
setnames(concated_counts, c("code", "N"))
concated_counts[, code := as.numeric(as.character(code))]
setorder(concated_counts, code)
concated_counts[, share := N / total_respondents]
concated_counts <- rbind(concated_counts, data.table(code = 5, N = 0, share = 0), fill = TRUE)

legend_lab <- sprintf("Total respondents: %s", total_respondents)

p <- ggplot(concated_counts, aes(x = code, y = share)) +
  geom_col(fill = "skyblue", width = 0.8) +
  geom_text(aes(label = sprintf("%.0f", share * total_respondents)), vjust = -0.25, size = 3) +
  scale_x_continuous(
    breaks = 0:5,
    labels = c("Somewhat oppose", "Strongly oppose", "Neither", "Somewhat support", "Strongly support", "N/A")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    x = "Q2 & 13: SNAP soda restriction policy support - all respondents",
    y = "Share of total responses"
  ) +
  theme(axis.text.x = element_text(size = 8)) +
  geom_col(aes(fill = legend_lab), alpha = 1, width = 0.8) +
  scale_fill_manual(values = setNames("skyblue", legend_lab))
ggsave("q2_snap_soda_policy_support_distribution_all.pdf", p, width = 8, height = 5, units = "in")
# endregion Q2 agg


# ----------------
# region Q3
# ----------------
x_map <- c(
  "Not at all" = 0,
  "Very little" = 1,
  "Somewhat" = 2,
  "Quite a bit" = 3,
  "A great deal" = 4
)

q3_codes <- unname(x_map[as.character(data$Q3)])
q3_codes[is.na(data$Q3)] <- 5

q3_counts <- as.data.table(table(q3_codes, useNA = "ifany"))
setnames(q3_counts, c("code", "N"))
q3_counts[, code := as.numeric(as.character(code))]
setorder(q3_counts, code)
q3_counts[, share := N / total_respondents]
q3_counts <- rbind(q3_counts, data.table(code = 5, N = 0, share = 0), fill = TRUE)

legend_lab <- sprintf("Total respondents: %s", total_respondents)

p <- ggplot(q3_counts, aes(x = code, y = share)) +
  geom_col(fill = "skyblue", width = 0.8) +
  geom_text(aes(label = sprintf("%.0f", share * total_respondents)), vjust = -0.25, size = 3) +
  scale_x_continuous(
    breaks = 0:5,
    labels = c("Not at all", "Very little", "Somewhat", "Quite a bit", "A great deal", "N/A")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    x = "Q3: Perceived risk of soda consumption to health - all respondents",
    y = "Share of total responses"
  ) +
  theme(axis.text.x = element_text(size = 8)) +
  geom_col(aes(fill = legend_lab), alpha = 1, width = 0.8) +
  scale_fill_manual(values = setNames("skyblue", legend_lab))
ggsave("q3_perceived_risk_soda_health_distribution.pdf", p, width = 8, height = 5, units = "in")
# endregion Q3


# ----------------
# region Q4
# ----------------
x_map <- c(
  "No" = 0,
  "Yes" = 1,
  "Don\u2019t Know" = 2
)

q4_codes <- unname(x_map[as.character(data$Q4)])
q4_codes[is.na(data$Q4)] <- 3

q4_counts <- as.data.table(table(q4_codes, useNA = "ifany"))
setnames(q4_counts, c("code", "N"))
q4_counts[, code := as.numeric(as.character(code))]
setorder(q4_counts, code)
q4_counts[, share := N / total_respondents]
q4_counts <- rbind(q4_counts, data.table(code = 3, N = 0, share = 0), fill = TRUE)

legend_lab <- sprintf("Total respondents: %s", total_respondents)

p <- ggplot(q4_counts, aes(x = code, y = share)) +
  geom_col(fill = "skyblue", width = 0.8) +
  geom_text(aes(label = sprintf("%.0f", share * total_respondents)), vjust = -0.25, size = 3) +
  scale_x_continuous(
    breaks = 0:3,
    labels = c("No", "Yes", "Don't know", "N/A")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    x = "Q4: Past 3 months SNAP participation - all respondents",
    y = "Share of total responses"
  ) +
  theme(axis.text.x = element_text(size = 8)) +
  geom_col(aes(fill = legend_lab), alpha = 1, width = 0.8) +
  scale_fill_manual(values = setNames("skyblue", legend_lab))
ggsave("q4_snap_participation_distribution.pdf", p, width = 8, height = 5, units = "in")
# endregion Q4


# ----------------
# region Q5 (SNAP participants)
# ----------------
x_map <- c(
  "Never" = 0,
  "Rarely" = 1,
  "Sometimes" = 2,
  "Often" = 3,
  "Most or all of the time" = 4,
  "I did not pay for groceries with my [SNAPbyState] benefits in the past 3 months" = 5
)

dt <- data[Q4 == "Yes"]
total_respondents <- nrow(dt)

q5_codes <- unname(x_map[as.character(dt$Q5)])
q5_codes[is.na(dt$Q5)] <- 6

q5_counts <- as.data.table(table(q5_codes, useNA = "ifany"))
setnames(q5_counts, c("code", "N"))
q5_counts[, code := as.numeric(as.character(code))]
setorder(q5_counts, code)
q5_counts[, share := N / total_respondents]
q5_counts <- rbind(q5_counts, data.table(code = 6, N = 0, share = 0), fill = TRUE)

legend_lab <- sprintf("Total SNAP participants in Q4: %s", total_respondents)

p <- ggplot(q5_counts, aes(x = code, y = share)) +
  geom_col(fill = "skyblue", width = 0.8) +
  geom_text(aes(label = sprintf("%.0f", share * total_respondents)), vjust = -0.25, size = 3) +
  scale_x_continuous(
    breaks = 0:6,
    labels = c("Never", "Rarely", "Sometimes", "Often", "Most/all of the time", "Didn’t use EBT", "N/A")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    x = "Q5: Embarrassment - SNAP participants",
    y = "Share of total SNAP participants in Q4"
  ) +
  theme(axis.text.x = element_text(size = 7.5)) +
  geom_col(aes(fill = legend_lab), alpha = 1, width = 0.8) +
  scale_fill_manual(values = setNames("skyblue", legend_lab))
ggsave("q5_embarrassment_snap_participants_distribution.pdf", p, width = 8, height = 5, units = "in")
# endregion Q5


# ----------------
# region Q6 (non-SNAP participants)
# ----------------
x_map <- c(
  "Never" = 0,
  "Rarely" = 1,
  "Sometimes" = 2,
  "Often" = 3,
  "Most or all of the time" = 4
)

dt <- data[Q4 == "No"]
total_respondents <- nrow(dt)

q6_codes <- unname(x_map[as.character(dt$Q6)])
q6_codes[is.na(dt$Q6)] <- 5

q6_counts <- as.data.table(table(q6_codes, useNA = "ifany"))
setnames(q6_counts, c("code", "N"))
q6_counts[, code := as.numeric(as.character(code))]
setorder(q6_counts, code)
q6_counts[, share := N / total_respondents]
q6_counts <- rbind(q6_counts, data.table(code = 5, N = 0, share = 0), fill = TRUE)

legend_lab <- sprintf("Total non-SNAP participants in Q4: %s", total_respondents)

p <- ggplot(q6_counts, aes(x = code, y = share)) +
  geom_col(fill = "skyblue", width = 0.8) +
  geom_text(aes(label = sprintf("%.0f", share * total_respondents)), vjust = -0.25, size = 3) +
  scale_x_continuous(
    breaks = 0:5,
    labels = c("Never", "Rarely", "Sometimes", "Often", "Most/all of the time", "N/A")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    x = "Q6: Embarrassment - non-SNAP participants",
    y = "Share of total non-SNAP participants in Q4"
  ) +
  theme(axis.text.x = element_text(size = 8)) +
  geom_col(aes(fill = legend_lab), alpha = 1, width = 0.8) +
  scale_fill_manual(values = setNames("skyblue", legend_lab))
ggsave("q6_embarrassment_non_snap_participants_distribution.pdf", p, width = 8, height = 5, units = "in")
# endregion Q6


# ----------------
# region Q7
# ----------------
x_map <- c(
  "Strongly disagree" = 0,
  "Somewhat disagree" = 1,
  "Neither disagree nor agree" = 2,
  "Somewhat agree" = 3,
  "Strongly agree" = 4
)

total_respondents <- nrow(data)
q7_codes <- unname(x_map[as.character(data$Q7)])
q7_codes[is.na(data$Q7)] <- 5

q7_counts <- as.data.table(table(q7_codes, useNA = "ifany"))
setnames(q7_counts, c("code", "N"))
q7_counts[, code := as.numeric(as.character(code))]
setorder(q7_counts, code)
q7_counts[, share := N / total_respondents]
q7_counts <- rbind(q7_counts, data.table(code = 5, N = 0, share = 0), fill = TRUE)

legend_lab <- sprintf("Total responses: %s", total_respondents)

p <- ggplot(q7_counts, aes(x = code, y = share)) +
  geom_col(fill = "skyblue", width = 0.8) +
  geom_text(aes(label = sprintf("%.0f", share * total_respondents)), vjust = -0.25, size = 3) +
  scale_x_continuous(
    breaks = 0:5,
    labels = c("Strongly disagree", "Somewhat disagree", "Neither", "Somewhat agree", "Strongly agree", "N/A")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    x = "Q7: Perceived social stigma of SNAP participation - all respondents",
    y = "Share of total responses"
  ) +
  theme(axis.text.x = element_text(size = 8)) +
  geom_col(aes(fill = legend_lab), alpha = 1, width = 0.8) +
  scale_fill_manual(values = setNames("skyblue", legend_lab))
ggsave("q7_perceived_social_stigma_snap_participation_distribution.pdf", p, width = 8, height = 5, units = "in")
# endregion Q7


# ----------------
# region Q8
# ----------------
q8_codes <- unname(x_map[as.character(data$Q8)])
q8_codes[is.na(data$Q8)] <- 5

q8_counts <- as.data.table(table(q8_codes, useNA = "ifany"))
setnames(q8_counts, c("code", "N"))
q8_counts[, code := as.numeric(as.character(code))]
setorder(q8_counts, code)
q8_counts[, share := N / total_respondents]
q8_counts <- rbind(q8_counts, data.table(code = 5, N = 0, share = 0), fill = TRUE)

legend_lab <- sprintf("Total responses: %s", total_respondents)

p <- ggplot(q8_counts, aes(x = code, y = share)) +
  geom_col(fill = "skyblue", width = 0.8) +
  geom_text(aes(label = sprintf("%.0f", share * total_respondents)), vjust = -0.25, size = 3) +
  scale_x_continuous(
    breaks = 0:5,
    labels = c("Strongly disagree", "Somewhat disagree", "Neither", "Somewhat agree", "Strongly agree", "N/A")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    x = "Q8: Perceived social stigma of SNAP participation - all respondents",
    y = "Share of total responses"
  ) +
  theme(axis.text.x = element_text(size = 8)) +
  geom_col(aes(fill = legend_lab), alpha = 1, width = 0.8) +
  scale_fill_manual(values = setNames("skyblue", legend_lab))
ggsave("q8_perceived_social_stigma_snap_participation_distribution.pdf", p, width = 8, height = 5, units = "in")
# endregion Q8


# ----------------
# region Q9
# ----------------
x_map <- c(
  "$0" = 0, "$10" = 1, "$20" = 2, "$30" = 3, "$40" = 4, "$50" = 5,
  "$60" = 6, "$70" = 7, "$80" = 8, "$90" = 9, "$100" = 10
)

dt <- data[Q4 == "Yes"]
total_respondents <- nrow(dt)

dt[, Q9 := gsub(" ", "", Q9)]
q9_codes <- unname(x_map[as.character(dt$Q9)])
q9_codes[is.na(dt$Q9)] <- 11

avg_value <- mean(q9_codes * 10, na.rm = TRUE)

q9_counts <- as.data.table(table(q9_codes, useNA = "ifany"))
setnames(q9_counts, c("code", "N"))
q9_counts[, code := as.numeric(as.character(code))]
setorder(q9_counts, code)
q9_counts[, share := N / total_respondents]
q9_counts <- rbind(q9_counts, data.table(code = 11, N = 0, share = 0), fill = TRUE)

legend_lab <- sprintf("Total SNAP participants in Q4: %s\nAverage: $%.2f", total_respondents, avg_value)

p <- ggplot(q9_counts, aes(x = code, y = share)) +
  geom_col(fill = "skyblue", width = 0.8) +
  geom_text(aes(label = sprintf("%.0f", share * total_respondents)), vjust = -0.25, size = 3) +
  scale_x_continuous(
    breaks = 0:11,
    labels = c("$0", "$10", "$20", "$30", "$40", "$50", "$60", "$70", "$80", "$90", "$100", "N/A")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    x = "Q9: Mental accounting of SNAP benefits - SNAP participants",
    y = "Share of total SNAP participants in Q4"
  ) +
  theme(axis.text.x = element_text(size = 8)) +
  geom_col(aes(fill = legend_lab), alpha = 1, width = 0.8) +
  scale_fill_manual(values = setNames("skyblue", legend_lab))
ggsave("q9_mental_accounting_snap_participants_distribution.pdf", p, width = 8, height = 5, units = "in")
# endregion Q9


# ----------------
# region Q10
# ----------------
dt <- data[Q4 == "No"]
total_respondents <- nrow(dt)

dt[, Q10 := gsub(" ", "", Q10)]
q10_codes <- unname(x_map[as.character(dt$Q10)])
q10_codes[is.na(dt$Q10)] <- 11

avg_value <- mean(q10_codes * 10, na.rm = TRUE)

q10_counts <- as.data.table(table(q10_codes, useNA = "ifany"))
setnames(q10_counts, c("code", "N"))
q10_counts[, code := as.numeric(as.character(code))]
setorder(q10_counts, code)
q10_counts[, share := N / total_respondents]
q10_counts <- rbind(q10_counts, data.table(code = 11, N = 0, share = 0), fill = TRUE)

legend_lab <- sprintf("Total non-SNAP participants in Q4: %s\nAverage: $%.2f", total_respondents, avg_value)

p <- ggplot(q10_counts, aes(x = code, y = share)) +
  geom_col(fill = "skyblue", width = 0.8) +
  geom_text(aes(label = sprintf("%.0f", share * total_respondents)), vjust = -0.25, size = 3) +
  scale_x_continuous(
    breaks = 0:11,
    labels = c("$0", "$10", "$20", "$30", "$40", "$50", "$60", "$70", "$80", "$90", "$100", "N/A")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    x = "Q10: Mental accounting of SNAP benefits - non-SNAP participants",
    y = "Share of total non-SNAP participants in Q4"
  ) +
  theme(axis.text.x = element_text(size = 8)) +
  geom_col(aes(fill = legend_lab), alpha = 1, width = 0.8) +
  scale_fill_manual(values = setNames("skyblue", legend_lab))
ggsave("q10_mental_accounting_non_snap_participants_distribution.pdf", p, width = 8, height = 5, units = "in")
# endregion Q10


# ----------------
# region Q11
# ----------------
total_respondents <- nrow(data)
data[, Q11 := gsub(" ", "", Q11)]

q11_codes <- unname(x_map[as.character(data$Q11)])
q11_codes[is.na(data$Q11)] <- 11

avg_value <- mean(q11_codes * 10, na.rm = TRUE)

q11_counts <- as.data.table(table(q11_codes, useNA = "ifany"))
setnames(q11_counts, c("code", "N"))
q11_counts[, code := as.numeric(as.character(code))]
setorder(q11_counts, code)
q11_counts[, share := N / total_respondents]
q11_counts <- rbind(q11_counts, data.table(code = 11, N = 0, share = 0), fill = TRUE)

legend_lab <- sprintf("Total responses: %s\nAverage value: $%.2f", total_respondents, avg_value)

p <- ggplot(q11_counts, aes(x = code, y = share)) +
  geom_col(fill = "skyblue", width = 0.8) +
  geom_text(aes(label = sprintf("%.0f", share * total_respondents)), vjust = -0.25, size = 3) +
  scale_x_continuous(
    breaks = 0:11,
    labels = c("$0", "$10", "$20", "$30", "$40", "$50", "$60", "$70", "$80", "$90", "$100", "N/A")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    x = "Q11: Mental accounting of cash - all respondents",
    y = "Share of total responses"
  ) +
  theme(axis.text.x = element_text(size = 8)) +
  geom_col(aes(fill = legend_lab), alpha = 1, width = 0.8) +
  scale_fill_manual(values = setNames("skyblue", legend_lab))
ggsave("q11_mental_accounting_cash_all_respondents_distribution.pdf", p, width = 8, height = 5, units = "in")
# endregion Q11


# ----------------
# region Q12
# ----------------
x_map <- c("$110 one month from now" = 0, "$100 today" = 1)

q12_codes <- unname(x_map[as.character(data$Q12)])
q12_codes[is.na(data$Q12)] <- 2

q12_counts <- as.data.table(table(q12_codes, useNA = "ifany"))
setnames(q12_counts, c("code", "N"))
q12_counts[, code := as.numeric(as.character(code))]
setorder(q12_counts, code)
q12_counts[, share := N / total_respondents]
q12_counts <- rbind(q12_counts, data.table(code = 2, N = 0, share = 0), fill = TRUE)

legend_lab <- sprintf("Total responses: %s", total_respondents)

p <- ggplot(q12_counts, aes(x = code, y = share)) +
  geom_col(fill = "skyblue", width = 0.8) +
  geom_text(aes(label = sprintf("%.0f", share * total_respondents)), vjust = -0.25, size = 3) +
  scale_x_continuous(
    breaks = 0:2,
    labels = c("$110 one month from now", "$100 today", "N/A")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    x = "Q12: Time preference - all respondents",
    y = "Share of total responses"
  ) +
  theme(axis.text.x = element_text(size = 8)) +
  geom_col(aes(fill = legend_lab), alpha = 1, width = 0.8) +
  scale_fill_manual(values = setNames("skyblue", legend_lab))
ggsave("q12_time_preference_all_respondents_distribution.pdf", p, width = 8, height = 5, units = "in")
# endregion Q12


# ----------------
# region Q14
# ----------------
x_map <- c("Not at all" = 0, "Somewhat" = 1, "Mostly" = 2, "Definitely" = 3)

q14_codes <- unname(x_map[as.character(data$Q14)])
q14_codes[is.na(data$Q14)] <- 4

q14_counts <- as.data.table(table(q14_codes, useNA = "ifany"))
setnames(q14_counts, c("code", "N"))
q14_counts[, code := as.numeric(as.character(code))]
setorder(q14_counts, code)
q14_counts[, share := N / total_respondents]
q14_counts <- rbind(q14_counts, data.table(code = 4, N = 0, share = 0), fill = TRUE)

legend_lab <- sprintf("Total responses: %s", total_respondents)

p <- ggplot(q14_counts, aes(x = code, y = share)) +
  geom_col(fill = "skyblue", width = 0.8) +
  geom_text(aes(label = sprintf("%.0f", share * total_respondents)), vjust = -0.25, size = 3) +
  scale_x_continuous(
    breaks = 0:4,
    labels = c("Not at all", "Somewhat", "Mostly", "Definitely", "N/A")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    x = "Q14: Self control - all respondents",
    y = "Share of total responses"
  ) +
  theme(axis.text.x = element_text(size = 8)) +
  geom_col(aes(fill = legend_lab), alpha = 1, width = 0.8) +
  scale_fill_manual(values = setNames("skyblue", legend_lab))
ggsave("q14_self_control_all_respondents_distribution.pdf", p, width = 8, height = 5, units = "in")
# endregion Q14


# ----------------
# region income dist
# ----------------
income_map <- c(
  "'- $20k" = 0,
  "$20k-40k" = 1,
  "$40k-60k" = 2,
  "$60k-80k" = 3,
  "$80k-100k" = 4,
  "$100k-125k" = 5,
  "$125k +" = 6
)

width <- 0.4

dt_snap <- data[Q4 == "Yes"]
total_snap_respondents <- nrow(dt_snap)
income_codes_snap <- unname(income_map[as.character(dt_snap$income)])

income_counts_snap <- as.data.table(table(income_codes_snap, useNA = "ifany"))
setnames(income_counts_snap, c("code", "N"))
income_counts_snap[, code := as.numeric(as.character(code))]
setorder(income_counts_snap, code)
income_counts_snap[, share := N / total_snap_respondents]
income_counts_snap[, x := code - width/2]
income_counts_snap[, grp := sprintf("SNAP participants in Q4 - %s responses", total_snap_respondents)]
income_counts_snap[, nlab := share * total_snap_respondents]

dt_nonsnap <- data[Q4 == "No"]
total_nonsnap_respondents <- nrow(dt_nonsnap)
income_codes_nonsnap <- unname(income_map[as.character(dt_nonsnap$income)])

income_counts_nonsnap <- as.data.table(table(income_codes_nonsnap, useNA = "ifany"))
setnames(income_counts_nonsnap, c("code", "N"))
income_counts_nonsnap[, code := as.numeric(as.character(code))]
setorder(income_counts_nonsnap, code)
income_counts_nonsnap[, share := N / total_nonsnap_respondents]
income_counts_nonsnap[, x := code + width/2]
income_counts_nonsnap[, grp := sprintf("Non-SNAP participants in Q4 - %s responses", total_nonsnap_respondents)]
income_counts_nonsnap[, nlab := share * total_nonsnap_respondents]

plot_dt <- rbind(income_counts_snap, income_counts_nonsnap, fill = TRUE)

p <- ggplot(plot_dt, aes(x = x, y = share, fill = grp)) +
  geom_col(width = width) +
  geom_text(aes(label = sprintf("%.0f", nlab)), vjust = -0.25, size = 3) +
  scale_fill_manual(values = setNames(c("skyblue", "lightgreen"), unique(plot_dt$grp))) +
  scale_x_continuous(
    breaks = 0:6,
    labels = c("<$20k", "$20k-40k", "$40k-60k", "$60k-80k", "$80k-100k", "$100k-125k", "$125k+")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    x = "Income distribution by Q4 SNAP participation",
    y = "Share of total responses within group"
  ) +
  theme(legend.position = "bottom")
ggsave("income_distribution_by_q4_snap_participation.pdf", p, width = 8, height = 5, units = "in")
# endregion income dist


# ----------------
# region q9 q11 (SNAP)
# ----------------
wdith <- 0.4
dt <- data[Q4 == "Yes"]
total_snap_respondents <- nrow(dt)

dt[, Q9 := gsub(" ", "", Q9)]
data[, Q11 := gsub(" ", "", Q11)]

q9_codes <- unname(x_map[as.character(dt$Q9)])
q11_codes <- unname(x_map[as.character(dt$Q11)])

q9_counts <- as.data.table(table(q9_codes, useNA = "ifany"))
setnames(q9_counts, c("code", "N"))
q9_counts[, code := as.numeric(as.character(code))]
setorder(q9_counts, code)
q9_counts[, share := N / total_snap_respondents]
q9_counts[, x := code - wdith/2]
q9_counts[, grp := sprintf("Q9 - SNAP participants - %s responses", total_snap_respondents)]
q9_counts[, nlab := share * total_snap_respondents]

q11_counts <- as.data.table(table(q11_codes, useNA = "ifany"))
setnames(q11_counts, c("code", "N"))
q11_counts[, code := as.numeric(as.character(code))]
setorder(q11_counts, code)
q11_counts[, share := N / total_snap_respondents]
q11_counts[, x := code + wdith/2]
q11_counts[, grp := sprintf("Q11 - SNAP participants - %s responses", total_snap_respondents)]
q11_counts[, nlab := share * total_snap_respondents]

plot_dt <- rbind(q9_counts, q11_counts, fill = TRUE)

p <- ggplot(plot_dt, aes(x = x, y = share, fill = grp)) +
  geom_col(width = wdith) +
  geom_text(aes(label = sprintf("%.0f", nlab)), vjust = -0.25, size = 3) +
  scale_fill_manual(values = setNames(c("skyblue", "lightgreen"), unique(plot_dt$grp))) +
  scale_x_continuous(
    breaks = 0:10,
    labels = c("$0", "$10", "$20", "$30", "$40", "$50", "$60", "$70", "$80", "$90", "$100")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    x = "Q9 & Q11: Mental accounting of SNAP benefits (Q9) vs cash (Q11)",
    y = "Share of total responses within group"
  ) +
  theme(axis.text.x = element_text(size = 8))
ggsave("q9_q11_mental_accounting_snap_cash_distribution_snap_participants.pdf", p, width = 8, height = 5, units = "in")
# endregion q9 q11


# ----------------
# region q10 q11 (non-SNAP)
# ----------------
wdith <- 0.4
dt <- data[Q4 == "No"]
total_snap_respondents <- nrow(dt)

dt[, Q10 := gsub(" ", "", Q10)]
data[, Q11 := gsub(" ", "", Q11)]

q10_codes <- unname(x_map[as.character(dt$Q10)])
q11_codes <- unname(x_map[as.character(dt$Q11)])

q10_counts <- as.data.table(table(q10_codes, useNA = "ifany"))
setnames(q10_counts, c("code", "N"))
q10_counts[, code := as.numeric(as.character(code))]
setorder(q10_counts, code)
q10_counts[, share := N / total_snap_respondents]
q10_counts[, x := code - wdith/2]
q10_counts[, grp := sprintf("Q10 - Non-SNAP participants - %s responses", total_snap_respondents)]
q10_counts[, nlab := share * total_snap_respondents]

q11_counts <- as.data.table(table(q11_codes, useNA = "ifany"))
setnames(q11_counts, c("code", "N"))
q11_counts[, code := as.numeric(as.character(code))]
setorder(q11_counts, code)
q11_counts[, share := N / total_snap_respondents]
q11_counts[, x := code + wdith/2]
q11_counts[, grp := sprintf("Q11 - Non-SNAP participants - %s responses", total_snap_respondents)]
q11_counts[, nlab := share * total_snap_respondents]

plot_dt <- rbind(q10_counts, q11_counts, fill = TRUE)

p <- ggplot(plot_dt, aes(x = x, y = share, fill = grp)) +
  geom_col(width = wdith) +
  geom_text(aes(label = sprintf("%.0f", nlab)), vjust = -0.25, size = 3) +
  scale_fill_manual(values = setNames(c("skyblue", "lightgreen"), unique(plot_dt$grp))) +
  scale_x_continuous(
    breaks = 0:10,
    labels = c("$0", "$10", "$20", "$30", "$40", "$50", "$60", "$70", "$80", "$90", "$100")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    x = "Q10 & Q11: Mental accounting of SNAP benefits (Q10) vs cash (Q11)",
    y = "Share of total responses within group"
  ) +
  theme(axis.text.x = element_text(size = 8))
ggsave("q10_q11_mental_accounting_snap_cash_distribution_non_snap_participants.pdf", p, width = 8, height = 5, units = "in")
# endregion q10 q11


# ----------------
# region q12 by snap
# ----------------
x_map <- c("$110 one month from now" = 0, "$100 today" = 1)
width <- 0.4

dt_yes <- data[Q4 == "Yes"]
total_snap_respondents <- nrow(dt_yes)
q12_codes_yes <- unname(x_map[as.character(dt_yes$Q12)])

cnt_yes <- as.data.table(table(q12_codes_yes, useNA = "ifany"))
setnames(cnt_yes, c("code", "N"))
cnt_yes[, code := as.numeric(as.character(code))]
setorder(cnt_yes, code)
cnt_yes[, share := N / total_snap_respondents]
cnt_yes[, x := code - width/2]
cnt_yes[, grp := sprintf("SNAP participants in Q4 - %s responses", total_snap_respondents)]
cnt_yes[, nlab := share * total_snap_respondents]

dt_no <- data[Q4 == "No"]
total_nonsnap_respondents <- nrow(dt_no)
q12_codes_no <- unname(x_map[as.character(dt_no$Q12)])

cnt_no <- as.data.table(table(q12_codes_no, useNA = "ifany"))
setnames(cnt_no, c("code", "N"))
cnt_no[, code := as.numeric(as.character(code))]
setorder(cnt_no, code)
cnt_no[, share := N / total_nonsnap_respondents]
cnt_no[, x := code + width/2]
cnt_no[, grp := sprintf("Non-SNAP participants in Q4 - %s responses", total_nonsnap_respondents)]
cnt_no[, nlab := share * total_nonsnap_respondents]

plot_dt <- rbind(cnt_yes, cnt_no, fill = TRUE)

p <- ggplot(plot_dt, aes(x = x, y = share, fill = grp)) +
  geom_col(width = width) +
  geom_text(aes(label = sprintf("%.0f", nlab)), vjust = -0.25, size = 3) +
  scale_fill_manual(values = setNames(c("skyblue", "lightgreen"), unique(plot_dt$grp))) +
  scale_x_continuous(
    breaks = 0:1,
    labels = c("$110 one month from now", "$100 today")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    x = "Q12: Time preference by Q4 SNAP participation",
    y = "Share of total responses within group"
  )
ggsave("q12_time_preference_by_q4_snap_participation.pdf", p, width = 8, height = 5, units = "in")
# endregion q12 by snap


# ----------------
# region mental accounting (SNAP): snap_cash_diff = Q9 - Q11 by Q12
# ----------------
width <- 0.4
x_map <- c(
  "$0" = 0, "$10" = 1, "$20" = 2, "$30" = 3, "$40" = 4, "$50" = 5,
  "$60" = 6, "$70" = 7, "$80" = 8, "$90" = 9, "$100" = 10
)

data[, snap_cash_diff := unname(x_map[as.character(Q9)]) - unname(x_map[as.character(Q11)])]

dt_110 <- data[Q12 == "$110 one month from now" & Q4 == "Yes"]
total_110_respondents <- nrow(dt_110)

cnt_110 <- as.data.table(table(dt_110$snap_cash_diff, useNA = "ifany"))
setnames(cnt_110, c("diff", "N"))
cnt_110[, diff := as.numeric(as.character(diff))]
setorder(cnt_110, diff)
cnt_110[, share := N / total_110_respondents]
cnt_110[, x := diff - width/2]
cnt_110[, grp := sprintf("$110 one month from now - %s responses", total_110_respondents)]

dt_100 <- data[Q12 == "$100 today" & Q4 == "Yes"]
total_100_respondents <- nrow(dt_100)

cnt_100 <- as.data.table(table(dt_100$snap_cash_diff, useNA = "ifany"))
setnames(cnt_100, c("diff", "N"))
cnt_100[, diff := as.numeric(as.character(diff))]
setorder(cnt_100, diff)
cnt_100[, share := N / total_100_respondents]
cnt_100[, x := diff + width/2]
cnt_100[, grp := sprintf("$100 today - %s responses", total_100_respondents)]

plot_dt <- rbind(cnt_110, cnt_100, fill = TRUE)

p <- ggplot(plot_dt, aes(x = x, y = share, fill = grp)) +
  geom_col(width = width) +
  scale_fill_manual(values = setNames(c("skyblue", "lightgreen"), unique(plot_dt$grp))) +
  scale_x_continuous(breaks = -10:10, labels = as.character((-10:10) * 10)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    x = "Difference in mental accounting of SNAP/cash by Q12 time preference",
    y = "Share of total responses within group"
  )
ggsave("difference_mental_accounting_snap_cash_by_q12_time_preference_snap.pdf", p, width = 8, height = 5, units = "in")
# endregion mental accounting


# ----------------
# region mental accounting nonsnap: snap_cash_diff = Q10 - Q11 by Q12
# ----------------
data[, snap_cash_diff := unname(x_map[as.character(Q10)]) - unname(x_map[as.character(Q11)])]

dt_110 <- data[Q12 == "$110 one month from now" & Q4 == "No"]
total_110_respondents <- nrow(dt_110)

cnt_110 <- as.data.table(table(dt_110$snap_cash_diff, useNA = "ifany"))
setnames(cnt_110, c("diff", "N"))
cnt_110[, diff := as.numeric(as.character(diff))]
setorder(cnt_110, diff)
cnt_110[, share := N / total_110_respondents]
cnt_110[, x := diff - width/2]
cnt_110[, grp := sprintf("$110 one month from now - %s responses", total_110_respondents)]

dt_100 <- data[Q12 == "$100 today" & Q4 == "No"]
total_100_respondents <- nrow(dt_100)

cnt_100 <- as.data.table(table(dt_100$snap_cash_diff, useNA = "ifany"))
setnames(cnt_100, c("diff", "N"))
cnt_100[, diff := as.numeric(as.character(diff))]
setorder(cnt_100, diff)
cnt_100[, share := N / total_100_respondents]
cnt_100[, x := diff + width/2]
cnt_100[, grp := sprintf("$100 today - %s responses", total_100_respondents)]

plot_dt <- rbind(cnt_110, cnt_100, fill = TRUE)

p <- ggplot(plot_dt, aes(x = x, y = share, fill = grp)) +
  geom_col(width = width) +
  scale_fill_manual(values = setNames(c("skyblue", "lightgreen"), unique(plot_dt$grp))) +
  scale_x_continuous(breaks = -10:10, labels = as.character((-10:10) * 10)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    x = "Difference in mental accounting of SNAP/cash by Q12 time preference",
    y = "Share of total responses within group"
  )
ggsave("difference_mental_accounting_snap_cash_by_q12_time_preference_non_snap.pdf", p, width = 8, height = 5, units = "in")
# endregion mental accounting nonsnap


# ----------------
# region mental accounters
# ----------------
width <- 0.4

dt_yes <- data[Q4 == "Yes"]
total_snap_respondents <- nrow(dt_yes)
data[Q4 == "Yes", accter := fifelse(unname(x_map[as.character(Q9)]) > unname(x_map[as.character(Q11)]), 1L, 0L)]

cnt_yes <- data[Q4 == "Yes", .N, by = accter][order(accter)]
cnt_yes[, share := N / total_snap_respondents]
cnt_yes[, x := accter - width/2]
cnt_yes[, grp := sprintf("SNAP participants in Q4 - %s responses", total_snap_respondents)]
cnt_yes[, nlab := share * total_snap_respondents]

dt_no <- data[Q4 == "No"]
total_nonsnap_respondents <- nrow(dt_no)
data[Q4 == "No", accter := fifelse(unname(x_map[as.character(Q10)]) > unname(x_map[as.character(Q11)]), 1L, 0L)]

cnt_no <- data[Q4 == "No", .N, by = accter][order(accter)]
cnt_no[, share := N / total_nonsnap_respondents]
cnt_no[, x := accter + width/2]
cnt_no[, grp := sprintf("Non-SNAP participants in Q4 - %s responses", total_nonsnap_respondents)]
cnt_no[, nlab := share * total_nonsnap_respondents]

plot_dt <- rbind(cnt_yes, cnt_no, fill = TRUE)

p <- ggplot(plot_dt, aes(x = x, y = share, fill = grp)) +
  geom_col(width = width) +
  geom_text(aes(label = sprintf("%.0f", nlab)), vjust = -0.25, size = 3) +
  scale_fill_manual(values = setNames(c("skyblue", "lightgreen"), unique(plot_dt$grp))) +
  scale_x_continuous(
    breaks = c(0, 1),
    labels = c("Q9/Q10 \u2264 Q11 (no mental accounting)", "Q9/Q10 > Q11 (mental accounter)")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    x = "Mental accounting by Q4 SNAP participation",
    y = "Share of total responses within group"
  )
ggsave("mental_accounting_by_q4_snap_participation.pdf", p, width = 8, height = 5, units = "in")
# endregion mental accounters


# ----------------
# region table (kable)
# ----------------
tmp <- copy(data)
tmp[, tab_name := fcase(
  tab_name == "1 Untreated SNAP", "Untreated SNAP",
  tab_name == "1 treated SNAP", "Treated SNAP",
  tab_name == "2 treated SNAP", "Treated SNAP",
  tab_name == "2 untreated SNAP", "Untreated SNAP",
  tab_name == "Treatment NON SNAP", "Treated Non-SNAP",
  tab_name == "Untreated NON SNAP", "Untreated Non-SNAP",
  default = tab_name
)]
tmp1 <- dcast(tmp, tab_name ~ Q4, value.var = "age_exact", fun.aggregate = length, fill = 0)
writeLines(kable(tmp1, format = "latex", booktabs = TRUE), "treatment_by_q4_counts.tex")

tmp <- copy(data)
tmp[, tab_name := fcase(
  tab_name == "1 Untreated SNAP", "Untreated SNAP",
  tab_name == "1 treated SNAP", "Treated SNAP",
  tab_name == "2 treated SNAP", "Treated SNAP",
  tab_name == "2 untreated SNAP", "Untreated SNAP",
  tab_name == "Treatment NON SNAP", "Treated Non-SNAP",
  tab_name == "Untreated NON SNAP", "Untreated Non-SNAP",
  default = tab_name
)]
tmp2 <- dcast(tmp, tab_name ~ snap_used, value.var = "age_exact", fun.aggregate = length, fill = 0)
writeLines(kable(tmp2, format = "latex", booktabs = TRUE), "treatment_by_snap_used_counts.tex")
# endregion table
