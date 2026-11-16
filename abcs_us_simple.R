# US CDC ABCs IPD: simple case-count analyses mirroring south_africa.R, comparing 65+ vs <2.
#  1. Time series of cases by age group
#  2. Ratio of ratios by serotype: (post / pre) in 65+  divided by (post / pre) in <2
#  3. X-Y plot of the two post/pre ratios
# Case counts only (no population denominators), as in the South Africa analysis.
# Data: CDC ABCs 1998-2024 serotype data by age group and site (data.cdc.gov qvzb-qs6p), all sites summed.
library(tidyverse)

cdc_url    <- "https://data.cdc.gov/api/views/qvzb-qs6p/rows.csv?accessType=DOWNLOAD"
mirror_url <- "https://raw.githubusercontent.com/PopHIVE/Ingest/main/data/abcs/raw/qvzb-qs6p.csv.xz"
read_csv_url <- function(url, ext) {
  f <- tempfile(fileext = ext)
  download.file(url, f, mode = "wb", quiet = TRUE)
  read.csv(f, check.names = FALSE, stringsAsFactors = FALSE)
}
raw <- tryCatch(read_csv_url(cdc_url, ".csv"), error = function(e) read_csv_url(mirror_url, ".csv.xz"))
abcs <- tibble(age = trimws(raw[["Age Group (years)"]]), site = trimws(raw[["Site"]]),
               year = as.integer(raw[["Year"]]), serotype = trimws(as.character(raw[["IPD Serotype"]])),
               n = as.numeric(raw[["Frequency Count"]])) %>%
  filter(site != "All_Sites")   # sum the individual sites ourselves

# Periods (PCV13 era; 2020+ excluded because of COVID-19)
pre_yrs  <- 2007:2009
post_yrs <- 2015:2019
pcv7_types  <- c("4", "6B", "9V", "14", "18C", "19F", "23F")
pcv13_added <- c("1", "3", "5", "6A", "7F", "19A")
min_cases   <- 20
vg <- function(x) factor(case_when(x %in% pcv13_added ~ "PCV13-added serotypes",
                                   x %in% pcv7_types  ~ "PCV7 serotypes",
                                   TRUE               ~ "Non-PCV13 serotypes"),
                         levels = c("PCV13-added serotypes", "PCV7 serotypes", "Non-PCV13 serotypes"))
cols <- c("PCV13-added serotypes" = "#6250d6", "PCV7 serotypes" = "#2a78d6", "Non-PCV13 serotypes" = "#eb6834")

# ---- 1. Time series by age group ----------------------------------------------------------------
age_levels <- c("Age <2", "Age 2-4", "Age 5-17", "Age 18-49", "Age 50-64", "Age 65+")
ts_df <- abcs %>% group_by(year, age) %>% summarise(n = sum(n), .groups = "drop") %>%
  mutate(age_group = factor(sub("Age ", "", age), levels = sub("Age ", "", age_levels)))
p_ts <- ggplot(ts_df, aes(year, n, colour = age_group)) +
  geom_line(linewidth = 0.9) + geom_point(size = 1.5) +
  labs(x = NULL, y = "Cases per year", colour = "Age group",
       title = "US ABCs IPD: cases by age group",
       subtitle = "All ABCs sites combined; site/catchment populations changed over time") +
  theme_minimal(base_size = 11) + theme(panel.grid.minor = element_blank(), plot.title.position = "plot")
ggsave("abcs_us_cases_by_age.png", p_ts, width = 8, height = 5, dpi = 200, bg = "white")

# ---- 2. Ratio of ratios, 65+ vs <2, by serotype ---------------------------------------------------
cells <- abcs %>%
  filter(!serotype %in% c("MISS", "NT", ""), age %in% c("Age <2", "Age 65+"), year %in% c(pre_yrs, post_yrs)) %>%
  mutate(period = ifelse(year %in% pre_yrs, "pre", "post"), grp = ifelse(age == "Age <2", "lt2", "ge65")) %>%
  group_by(serotype, grp, period) %>% summarise(n = sum(n), .groups = "drop") %>%
  complete(serotype, grp, period, fill = list(n = 0)) %>%
  pivot_wider(names_from = c(grp, period), values_from = n)

ror <- function(d) {
  d %>%
    mutate(total_cases = lt2_pre + lt2_post + ge65_pre + ge65_post,
           adj = if_else(lt2_pre == 0 | lt2_post == 0 | ge65_pre == 0 | ge65_post == 0, 0.5, 0),
           yr_scale = length(pre_yrs) / length(post_yrs),   # convert to per-year rates
           irr_lt2  = (lt2_post + adj) / (lt2_pre + adj) * yr_scale,
           irr_ge65 = (ge65_post + adj) / (ge65_pre + adj) * yr_scale,
           ratio_of_ratios = irr_ge65 / irr_lt2,
           se = sqrt(1/(lt2_pre + adj) + 1/(lt2_post + adj) + 1/(ge65_pre + adj) + 1/(ge65_post + adj)),
           lower95 = ratio_of_ratios * exp(-1.96 * se),
           upper95 = ratio_of_ratios * exp( 1.96 * se))
}
ror_st <- cells %>% ror() %>% mutate(vaccine_group = vg(serotype)) %>%
  arrange(vaccine_group, desc(ratio_of_ratios))
print(as.data.frame(ror_st %>% select(-adj, -se, -yr_scale)), digits = 3)
write.csv(ror_st, "abcs_us_ratio_of_ratios_65plus_vs_lt2.csv", row.names = FALSE)

ror_grp <- cells %>% mutate(vaccine_group = vg(serotype)) %>% group_by(vaccine_group) %>%
  summarise(across(c(lt2_pre, lt2_post, ge65_pre, ge65_post), sum), .groups = "drop") %>% ror()
print(as.data.frame(ror_grp %>% select(-adj, -se, -yr_scale)), digits = 3)

plot_df <- ror_st %>% filter(total_cases >= min_cases) %>%
  mutate(serotype = factor(serotype, levels = serotype[order(ratio_of_ratios)]))
p_ror <- ggplot(plot_df, aes(y = serotype, x = ratio_of_ratios)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey60") +
  geom_linerange(aes(xmin = lower95, xmax = upper95, colour = vaccine_group), linewidth = 0.9) +
  geom_point(aes(colour = vaccine_group), size = 2) +
  scale_x_log10() + scale_colour_manual(values = cols, guide = "none") +
  facet_grid(vaccine_group ~ ., scales = "free_y", space = "free_y") +
  labs(x = "Ratio of ratios, 65+ vs <2 (log scale)", y = NULL,
       title = "US ABCs IPD: (2015-19 / 2007-09) in 65+ relative to <2",
       subtitle = "Case counts; 95% CI from Poisson log-ratio SE; serotypes with >=20 cases") +
  theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank(), plot.title.position = "plot",
        strip.text.y = element_text(angle = 0, hjust = 0))
ggsave("abcs_us_ratio_of_ratios.png", p_ror, width = 8, height = 9, dpi = 200, bg = "white")

# ---- 3. X-Y plot ---------------------------------------------------------------------------------
xy_df  <- ror_st %>% filter(total_cases >= min_cases)
lab_df <- xy_df %>% filter(total_cases >= 60 | vaccine_group != "Non-PCV13 serotypes")
brk <- c(0.01, 0.03, 0.1, 0.3, 1, 3, 10, 30, 100)
p_xy <- ggplot(xy_df, aes(x = irr_lt2, y = irr_ge65, colour = vaccine_group)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey60") +
  geom_hline(yintercept = 1, colour = "grey85") + geom_vline(xintercept = 1, colour = "grey85") +
  geom_point(aes(size = total_cases), alpha = 0.8) +
  geom_text(data = lab_df, aes(label = serotype), size = 3, vjust = -0.9, show.legend = FALSE) +
  scale_x_log10(breaks = brk, labels = brk) + scale_y_log10(breaks = brk, labels = brk) +
  scale_size_area(max_size = 8, name = "Total cases") +
  scale_colour_manual(values = cols, name = NULL) + coord_equal() +
  labs(x = "Ratio in <2 (2015-19 / 2007-09)", y = "Ratio in 65+ (2015-19 / 2007-09)",
       title = "US ABCs IPD: change by serotype, 65+ vs <2",
       subtitle = "Dashed line: equal change in both groups; points above it fell less (or rose more) in 65+") +
  theme_minimal(base_size = 11) + theme(panel.grid.minor = element_blank(), plot.title.position = "plot")
ggsave("abcs_us_xy_ratio.png", p_xy, width = 8, height = 7.5, dpi = 200, bg = "white")
