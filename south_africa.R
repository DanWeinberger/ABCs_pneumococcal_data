library(tidyverse)
sa1 <- read.csv("https://zenodo.org/records/17877255/files/SA_disease_point_base.csv?download=1")
sa2 <- sa1 %>%
       mutate( agec = if_else(ageyears<2 ,1,
                        if_else(ageyears>=2 & ageyears<5,2,
                           if_else(ageyears>=5 & ageyears<18,3,
                               if_else(ageyears>=18 & ageyears<40,4,
                                  if_else(ageyears>=40,5, 999
       )))))
        ) %>%
       filter(serotype!='')

# ---- Ratio of ratios: 40+ (agec=5) vs <2 (agec=1), by serotype -------------------------------------
# (post-PCV13 2016-2019 / pre-PCV 2006-2008) in 40+  divided by the same ratio in <2.
# Case counts only (no population denominators in this file), so population change within an age group
# is assumed to be small relative to the serotype change. Zero cells -> add 0.5 to all four cells.
pre_yrs  <- 2006:2008
post_yrs <- 2016:2019
pcv7_types  <- c("4", "6B", "9V", "14", "18C", "19F", "23F")
pcv13_added <- c("1", "3", "5", "6A", "7F", "19A")
min_cases   <- 20

cells <- sa2 %>%
  filter(agec %in% c(1, 5), year %in% c(pre_yrs, post_yrs)) %>%
  mutate(period = ifelse(year %in% pre_yrs, "pre", "post"),
         grp = ifelse(agec == 1, "lt2", "ge40")) %>%
  count(serotype, grp, period) %>%
  complete(serotype, grp, period, fill = list(n = 0)) %>%
  pivot_wider(names_from = c(grp, period), values_from = n)

ror <- function(d) {
  d %>%
    mutate(total_cases = lt2_pre + lt2_post + ge40_pre + ge40_post,
           adj = if_else(lt2_pre == 0 | lt2_post == 0 | ge40_pre == 0 | ge40_post == 0, 0.5, 0),
           yr_scale = length(pre_yrs) / length(post_yrs),  # post period has more years -> convert to per-year rates
           irr_lt2  = (lt2_post + adj) / (lt2_pre + adj) * yr_scale,
           irr_ge40 = (ge40_post + adj) / (ge40_pre + adj) * yr_scale,
           ratio_of_ratios = irr_ge40 / irr_lt2,
           se = sqrt(1/(lt2_pre + adj) + 1/(lt2_post + adj) + 1/(ge40_pre + adj) + 1/(ge40_post + adj)),
           lower95 = ratio_of_ratios * exp(-1.96 * se),
           upper95 = ratio_of_ratios * exp( 1.96 * se))
}

ror_st <- cells %>%
  ror() %>%
  mutate(vaccine_group = factor(case_when(serotype %in% pcv13_added ~ "PCV13-added serotypes",
                                          serotype %in% pcv7_types  ~ "PCV7 serotypes",
                                          TRUE                      ~ "Non-PCV13 serotypes"),
                                levels = c("PCV13-added serotypes", "PCV7 serotypes", "Non-PCV13 serotypes"))) %>%
  arrange(vaccine_group, desc(ratio_of_ratios))
print(as.data.frame(ror_st %>% select(-adj, -se, -yr_scale)), digits = 3)
write.csv(ror_st, "south_africa_ratio_of_ratios_40plus_vs_lt2.csv", row.names = FALSE)

# Pooled by vaccine group
ror_grp <- cells %>%
  mutate(vaccine_group = case_when(serotype %in% pcv13_added ~ "PCV13-added serotypes",
                                   serotype %in% pcv7_types  ~ "PCV7 serotypes",
                                   TRUE                      ~ "Non-PCV13 serotypes")) %>%
  group_by(vaccine_group) %>%
  summarise(across(c(lt2_pre, lt2_post, ge40_pre, ge40_post), sum), .groups = "drop") %>%
  ror()
print(as.data.frame(ror_grp %>% select(-adj, -se, -yr_scale)), digits = 3)

# Chart
plot_df <- ror_st %>% filter(total_cases >= min_cases) %>%
  mutate(serotype = factor(serotype, levels = serotype[order(ratio_of_ratios)]))
p_sa <- ggplot(plot_df, aes(y = serotype, x = ratio_of_ratios)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey60") +
  geom_linerange(aes(xmin = lower95, xmax = upper95, colour = vaccine_group), linewidth = 0.9) +
  geom_point(aes(colour = vaccine_group), size = 2) +
  scale_x_log10() +
  scale_colour_manual(values = c("PCV13-added serotypes" = "#6250d6", "PCV7 serotypes" = "#2a78d6",
                                 "Non-PCV13 serotypes" = "#eb6834"), guide = "none") +
  facet_grid(vaccine_group ~ ., scales = "free_y", space = "free_y") +
  labs(x = "Ratio of ratios, 40+ vs <2 (log scale)", y = NULL,
       title = "South Africa IPD: (2016-19 / 2006-08) in 40+ relative to <2",
       subtitle = "Case counts; 95% CI from Poisson log-ratio SE; serotypes with >=20 cases") +
  theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank(), plot.title.position = "plot",
        strip.text.y = element_text(angle = 0, hjust = 0))
ggsave("south_africa_ratio_of_ratios.png", p_sa, width = 8, height = 9, dpi = 200, bg = "white")

# ---- X-Y plot: post/pre ratio in <2 (x) vs 40+ (y), by serotype ---------------------------------
xy_df <- ror_st %>% filter(total_cases >= min_cases, !grepl("NEG|POOL|UNK|NVT", serotype))
lab_df <- xy_df %>% filter(total_cases >= 60 | vaccine_group != "Non-PCV13 serotypes")
brk <- c(0.01, 0.03, 0.1, 0.3, 1, 3, 10, 30, 100)
p_xy <- ggplot(xy_df, aes(x = irr_lt2, y = irr_ge40, colour = vaccine_group)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey60") +
  geom_hline(yintercept = 1, colour = "grey85") + geom_vline(xintercept = 1, colour = "grey85") +
  geom_point(aes(size = total_cases), alpha = 0.8) +
  geom_text(data = lab_df, aes(label = serotype), size = 3, vjust = -0.9, show.legend = FALSE) +
  scale_x_log10(breaks = brk, labels = brk) + scale_y_log10(breaks = brk, labels = brk) +
  scale_size_area(max_size = 8, name = "Total cases") +
  scale_colour_manual(values = c("PCV13-added serotypes" = "#6250d6", "PCV7 serotypes" = "#2a78d6",
                                 "Non-PCV13 serotypes" = "#eb6834"), name = NULL) +
  coord_equal() +
  labs(x = "Ratio in <2 (2016-19 / 2006-08)", y = "Ratio in 40+ (2016-19 / 2006-08)",
       title = "South Africa IPD: change by serotype, 40+ vs <2",
       subtitle = "Dashed line: equal change in both groups; points above it fell less (or rose more) in 40+") +
  theme_minimal(base_size = 11) + theme(panel.grid.minor = element_blank(), plot.title.position = "plot")
ggsave("south_africa_xy_ratio.png", p_xy, width = 8, height = 7.5, dpi = 200, bg = "white")

# ---- Time series: IPD cases by age group, by year ----------------------------------------------
age_labs <- c("<2", "2-4", "5-17", "18-39", "40+")
ts_df <- sa2 %>%
  filter(agec %in% 1:5) %>%
  count(year, agec) %>%
  complete(year, agec, fill = list(n = 0)) %>%
  mutate(age_group = factor(age_labs[agec], levels = age_labs))
p_ts <- ggplot(ts_df, aes(x = year, y = n, colour = age_group)) +
  geom_line(linewidth = 0.9) + geom_point(size = 1.5) +
  labs(x = NULL, y = "Cases per year", colour = "Age group",
       title = "South Africa IPD: cases by age group") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), plot.title.position = "plot")
ggsave("south_africa_cases_by_age.png", p_ts, width = 8, height = 5, dpi = 200, bg = "white")

# ---- Ratio of ratios: 18-39 (agec=4) vs <2 (agec=1), by serotype -------------------------------------
# Same calculation as the 40+ vs <2 section above. ror() expects columns named ge40_*, so the 18-39
# counts are put in those slots and the outputs renamed to *_ge18_39.
cells_18 <- sa2 %>%
  filter(agec %in% c(1, 4), year %in% c(pre_yrs, post_yrs)) %>%
  mutate(period = ifelse(year %in% pre_yrs, "pre", "post"),
         grp = ifelse(agec == 1, "lt2", "ge40")) %>%   # "ge40" slot holds the 18-39 group here
  count(serotype, grp, period) %>%
  complete(serotype, grp, period, fill = list(n = 0)) %>%
  pivot_wider(names_from = c(grp, period), values_from = n)

vg <- function(x) factor(case_when(x %in% pcv13_added ~ "PCV13-added serotypes",
                                   x %in% pcv7_types  ~ "PCV7 serotypes",
                                   TRUE               ~ "Non-PCV13 serotypes"),
                         levels = c("PCV13-added serotypes", "PCV7 serotypes", "Non-PCV13 serotypes"))
ror_st18 <- cells_18 %>% ror() %>% mutate(vaccine_group = vg(serotype)) %>%
  rename(irr_ge18_39 = irr_ge40) %>%
  arrange(vaccine_group, desc(ratio_of_ratios))
print(as.data.frame(ror_st18 %>% select(-adj, -se, -yr_scale)), digits = 3)
write.csv(ror_st18, "south_africa_ratio_of_ratios_18_39_vs_lt2.csv", row.names = FALSE)

ror_grp18 <- cells_18 %>% mutate(vaccine_group = vg(serotype)) %>%
  group_by(vaccine_group) %>%
  summarise(across(c(lt2_pre, lt2_post, ge40_pre, ge40_post), sum), .groups = "drop") %>%
  ror()
print(as.data.frame(ror_grp18 %>% select(-adj, -se, -yr_scale)), digits = 3)

plot_df18 <- ror_st18 %>% filter(total_cases >= min_cases) %>%
  mutate(serotype = factor(serotype, levels = serotype[order(ratio_of_ratios)]))
p_sa18 <- ggplot(plot_df18, aes(y = serotype, x = ratio_of_ratios)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey60") +
  geom_linerange(aes(xmin = lower95, xmax = upper95, colour = vaccine_group), linewidth = 0.9) +
  geom_point(aes(colour = vaccine_group), size = 2) +
  scale_x_log10() +
  scale_colour_manual(values = c("PCV13-added serotypes" = "#6250d6", "PCV7 serotypes" = "#2a78d6",
                                 "Non-PCV13 serotypes" = "#eb6834"), guide = "none") +
  facet_grid(vaccine_group ~ ., scales = "free_y", space = "free_y") +
  labs(x = "Ratio of ratios, 18-39 vs <2 (log scale)", y = NULL,
       title = "South Africa IPD: (2016-19 / 2006-08) in 18-39 relative to <2",
       subtitle = "Case counts; 95% CI from Poisson log-ratio SE; serotypes with >=20 cases") +
  theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank(), plot.title.position = "plot",
        strip.text.y = element_text(angle = 0, hjust = 0))
ggsave("south_africa_ratio_of_ratios_18_39.png", p_sa18, width = 8, height = 9, dpi = 200, bg = "white")

# ---- X-Y plot: post/pre ratio in <2 (x) vs 18-39 (y), by serotype -------------------------------
xy_df18 <- ror_st18 %>% filter(total_cases >= min_cases, !grepl("NEG|POOL|UNK|NVT", serotype))
lab_df18 <- xy_df18 %>% filter(total_cases >= 60 | vaccine_group != "Non-PCV13 serotypes")
p_xy18 <- ggplot(xy_df18, aes(x = irr_lt2, y = irr_ge18_39, colour = vaccine_group)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey60") +
  geom_hline(yintercept = 1, colour = "grey85") + geom_vline(xintercept = 1, colour = "grey85") +
  geom_point(aes(size = total_cases), alpha = 0.8) +
  geom_text(data = lab_df18, aes(label = serotype), size = 3, vjust = -0.9, show.legend = FALSE) +
  scale_x_log10(breaks = brk, labels = brk) + scale_y_log10(breaks = brk, labels = brk) +
  scale_size_area(max_size = 8, name = "Total cases") +
  scale_colour_manual(values = c("PCV13-added serotypes" = "#6250d6", "PCV7 serotypes" = "#2a78d6",
                                 "Non-PCV13 serotypes" = "#eb6834"), name = NULL) +
  coord_equal() +
  labs(x = "Ratio in <2 (2016-19 / 2006-08)", y = "Ratio in 18-39 (2016-19 / 2006-08)",
       title = "South Africa IPD: change by serotype, 18-39 vs <2",
       subtitle = "Dashed line: equal change in both groups; points above it fell less (or rose more) in 18-39") +
  theme_minimal(base_size = 11) + theme(panel.grid.minor = element_blank(), plot.title.position = "plot")
ggsave("south_africa_xy_ratio_18_39.png", p_xy18, width = 8, height = 7.5, dpi = 200, bg = "white")
