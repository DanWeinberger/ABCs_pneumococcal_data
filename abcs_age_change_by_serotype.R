# IPD ratio of ratios (65+ vs <2) on incidence, CDC ABCs, fitted with rjags, for two eras:
#   after PCV7:   2005-09 vs 1998-99
#   after PCV13:  2015-19 vs 2007-09
# Same Poisson model as the case-based version, with log person-years (per 100,000) as each cell's offset.
# For site i and serotype s (pair k), with the four cells <2 / 65+ x baseline / follow-up:
#   y[<2,  baseline ] ~ Poisson(PY[<2,  baseline ] * exp(a_lt2[k]))
#   y[<2,  follow-up] ~ Poisson(PY[<2,  follow-up] * exp(a_lt2[k] + b[k]))
#   y[65+, baseline ] ~ Poisson(PY[65+, baseline ] * exp(a_ge65[k]))
#   y[65+, follow-up] ~ Poisson(PY[65+, follow-up] * exp(a_ge65[k] + b[k] + theta[k]))
# so exp(a) are baseline incidence rates per 100,000, exp(b) is the <2 incidence rate ratio, and
# exp(theta) is the site's incidence ratio of ratios (65+ vs <2).
# Fixed effect:          theta[k] = mu[s]   (states differ only by Poisson sampling)
# Random state effects:  theta[k] ~ Normal(mu[s], sigma)
# Serotype means:        mu[s] ~ Normal(0, mu_prior_sd) independently
#                        (global_mean = TRUE gives mu[s] ~ Normal(mu0, tau) instead)
# Note: JAGS's dnorm() takes a precision (1 / SD^2), not an SD.
#
# Denominators: PopHIVE's ABCs site populations (data/abcs/standard/data.csv.gz).
#   <2 population  = 2/5 of the <5 population
#   65+ population = the 50+ population (the file has no 65+ group), i.e. 65+ assumed to grow like 50+
#   California excluded: its age-specific populations are internally inconsistent.
# Comparison ("site-stratified"): the ratio of the two Mantel-Haenszel incidence rate ratios (65+ and <2),
# stratified by site, with zero totals set to 0.5.
#
# Data: CDC ABCs "1998-2024 Serotype Data for Invasive Pneumococcal Disease Cases
# by Age Group and Site" (data.cdc.gov dataset qvzb-qs6p)

library(dplyr)
library(tidyr)
library(ggplot2)
library(rjags)
load.module("glm")  # block samplers for the Poisson log-linear terms; improves mixing

eras <- list(
  "After PCV7 (2005-09 vs 1998-99)"  = list(pre = 1998:1999, post = 2005:2009),
  "After PCV13 (2015-19 vs 2007-09)" = list(pre = 2007:2009, post = 2015:2019)
)
pcv7_types     <- c("4", "6B", "9V", "14", "18C", "19F", "23F")
pcv13_added    <- c("1", "3", "5", "6A", "7F", "19A")
#excluded_sites <- c("CA")
mu_prior_sd    <- 2.5  # prior SD of each serotype's log ratio of ratios
min_cases      <- 20   # chart only: hide serotypes with fewer cases (4 cells summed) in an era

# MCMC settings (per chain)
n_chains <- 4
n_adapt  <- 1000
n_burn   <- 5000
n_iter   <- 50000
n_thin   <- 10

# ---- 1. Case counts and population denominators ----------------------------------------------
cdc_url    <- "https://data.cdc.gov/api/views/qvzb-qs6p/rows.csv?accessType=DOWNLOAD"
mirror_url <- "https://raw.githubusercontent.com/PopHIVE/Ingest/main/data/abcs/raw/qvzb-qs6p.csv.xz"
pop_url    <- "https://raw.githubusercontent.com/PopHIVE/Ingest/main/data/abcs/standard/data.csv.gz"

read_csv_url <- function(url, ext, ...) {
  f <- tempfile(fileext = ext)
  download.file(url, f, mode = "wb", quiet = TRUE)
  read.csv(f, check.names = FALSE, stringsAsFactors = FALSE, ...)  # read.csv decompresses .gz/.xz itself
}
raw <- tryCatch(read_csv_url(cdc_url, ".csv"), error = function(e) {
  message("data.cdc.gov download failed; using the PopHIVE mirror of the same file")
  read_csv_url(mirror_url, ".csv.xz")
})

col <- function(pattern) grep(pattern, names(raw), ignore.case = TRUE, value = TRUE)[1]
abcs <- tibble(
  age  = trimws(raw[[col("^age")]]),
  site = trimws(raw[[col("^site")]]),
  year = as.integer(raw[[col("^year$")]]),
  st   = trimws(as.character(raw[[col("serotype")]])),
  n    = as.numeric(raw[[col("count")]])
) %>%
  filter(site != "All_Sites", !site %in% excluded_sites, !st %in% c("MISS", "NT"),
         age %in% c("Age <2", "Age 65+")) %>%
  mutate(age = ifelse(age == "Age <2", "lt2", "ge65"))

fips <- c("06" = "CA", "08" = "CO", "09" = "CT", "13" = "GA", "24" = "MD",
          "27" = "MN", "35" = "NM", "36" = "NY", "41" = "OR", "47" = "TN")
pop <- read_csv_url(pop_url, ".csv.gz", colClasses = c(geography = "character")) %>%
  filter(geography %in% names(fips), age %in% c("<5 years", "50+ years")) %>%
  mutate(site = unname(fips[geography]), year = as.integer(substr(time, 1, 4))) %>%
  group_by(site, age, year) %>%
  summarise(pop = first(pop), .groups = "drop") %>%
  mutate(pop = ifelse(age == "<5 years", pop * 2 / 5, pop),
         age = ifelse(age == "<5 years", "lt2", "ge65"))

# Cases and person-years by site, serotype, age group and period, for one era
era_cells <- function(pre, post) {
  sites <- setdiff(unique(abcs$site[abcs$year %in% pre]), excluded_sites)
  py <- pop %>%
    filter(site %in% sites, year %in% c(pre, post)) %>%
    mutate(period = ifelse(year %in% pre, "pre", "post")) %>%
    group_by(site, age, period) %>%
    summarise(py = sum(pop), .groups = "drop") %>%
    pivot_wider(names_from = period, values_from = py, names_prefix = "py_")
  abcs %>%
    filter(site %in% sites, year %in% c(pre, post)) %>%
    mutate(period = ifelse(year %in% pre, "pre", "post")) %>%
    group_by(site, st, age, period) %>%
    summarise(n = sum(n), .groups = "drop") %>%
    complete(site = sites, st, age, period, fill = list(n = 0)) %>%
    pivot_wider(names_from = period, values_from = n, names_prefix = "n_") %>%
    left_join(py, by = c("site", "age"))
}

# ---- 2. Site-stratified comparison: ratio of the two Mantel-Haenszel incidence rate ratios -------------
half_if_zero <- function(x) ifelse(x == 0, 0.5, x)
site_stratified <- function(cells) {
  cells %>%
    group_by(st, age) %>%
    summarise(cases_pre = sum(n_pre), cases_post = sum(n_post),
              R = sum(n_post * py_pre / (py_pre + py_post)),
              S = sum(n_pre * py_post / (py_pre + py_post)),
              V = sum((n_pre + n_post) * py_pre * py_post / (py_pre + py_post)^2),
              crude = (half_if_zero(sum(n_post)) / sum(py_post)) / (half_if_zero(sum(n_pre)) / sum(py_pre)),
              .groups = "drop") %>%
    mutate(zero = cases_pre == 0 | cases_post == 0,
           irr = ifelse(zero, crude, R / S),
           se_log = ifelse(zero, sqrt(1 / half_if_zero(cases_pre) + 1 / half_if_zero(cases_post)), sqrt(V / (R * S)))) %>%
    select(st, age, cases_pre, cases_post, irr, se_log) %>%
    pivot_wider(names_from = age, values_from = c(cases_pre, cases_post, irr, se_log)) %>%
    filter(!is.na(irr_lt2), !is.na(irr_ge65)) %>%
    mutate(total_cases  = cases_pre_lt2 + cases_post_lt2 + cases_pre_ge65 + cases_post_ge65,
           site_strat   = irr_ge65 / irr_lt2,
           site_strat_lower95 = site_strat * exp(-1.96 * sqrt(se_log_lt2^2 + se_log_ge65^2)),
           site_strat_upper95 = site_strat * exp( 1.96 * sqrt(se_log_lt2^2 + se_log_ge65^2))) %>%
    select(st, total_cases, site_strat, site_strat_lower95, site_strat_upper95)
}

# ---- 3. JAGS model ------------------------------------------------------------------------------
jags_code <- function(random_states, global_mean) {
  # Random state effects use a non-centered form (theta = mu + sigma * z), which mixes better in JAGS
  # when the between-state SD is small
  theta_line <- if (random_states) c("theta[k] <- mu[st[k]] + sigma * z[k]", "z[k] ~ dnorm(0, 1)") else "theta[k] <- mu[st[k]]"
  mu_line    <- if (global_mean) "mu[s] ~ dnorm(mu0, 1 / tau^2)" else sprintf("mu[s] ~ dnorm(0, 1 / %g^2)", mu_prior_sd)
  hyper      <- c(if (random_states) "sigma ~ dnorm(0, 1) T(0, )",
                  if (global_mean) c("mu0 ~ dnorm(0, 1 / 2.5^2)", "tau ~ dnorm(0, 1 / 2^2) T(0, )"))
  paste(c(
    "model {",
    "  for (k in 1:K) {",
    "    log(lam[k, 1]) <- log_py[k, 1] + a_lt2[k]",
    "    log(lam[k, 2]) <- log_py[k, 2] + a_lt2[k] + b[k]",
    "    log(lam[k, 3]) <- log_py[k, 3] + a_ge65[k]",
    "    log(lam[k, 4]) <- log_py[k, 4] + a_ge65[k] + b[k] + theta[k]",
    "    for (j in 1:4) {",
    "      y[k, j] ~ dpois(lam[k, j])",
    "    }",
    "    a_lt2[k]  ~ dnorm(0, 1 / 5^2)",
    "    a_ge65[k] ~ dnorm(0, 1 / 5^2)",
    "    b[k]      ~ dnorm(0, 1 / 5^2)",
    paste0("    ", theta_line),
    "  }",
    "  for (s in 1:S) {",
    paste0("    ", mu_line),
    "  }",
    paste0("  ", hyper),
    "}"), collapse = "\n")
}
cat(jags_code(random_states = FALSE, global_mean = FALSE), "\n")  # print the plain JAGS model text

fit_jags <- function(jags_data, random_states, global_mean = FALSE) {
  K <- jags_data$K; S <- jags_data$S
  inits <- lapply(seq_len(n_chains), function(ch) c(
    list(a_lt2 = rnorm(K, 0, 1), a_ge65 = rnorm(K, 0, 1), b = rnorm(K, 0, 1), mu = rnorm(S, 0, 1),
         .RNG.name = "base::Mersenne-Twister", .RNG.seed = 2026 + ch),
    if (random_states) list(z = rnorm(K, 0, 1), sigma = runif(1, 0.1, 1)),
    if (global_mean) list(mu0 = rnorm(1, 0, 1), tau = runif(1, 0.5, 2))
  ))
  monitor <- c("mu", if (random_states) "sigma", if (global_mean) c("mu0", "tau"))
  m <- jags.model(textConnection(jags_code(random_states, global_mean)), data = jags_data,
                  inits = inits, n.chains = n_chains, n.adapt = n_adapt, quiet = TRUE)
  update(m, n_burn)
  samples <- coda.samples(m, monitor, n.iter = n_iter, thin = n_thin)
  cat("Max Gelman-Rubin R-hat:", round(max(gelman.diag(samples, multivariate = FALSE)$psrf[, 1]), 3),
      "| min effective sample size:", round(min(effectiveSize(samples))), "\n")
  samples
}

serotype_summary <- function(samples, serotypes, prefix) {
  m <- exp(as.matrix(samples)[, paste0("mu[", seq_along(serotypes), "]")])
  out <- data.frame(st = serotypes,
                    median  = apply(m, 2, median),
                    lower95 = apply(m, 2, quantile, 0.025),
                    upper95 = apply(m, 2, quantile, 0.975))
  names(out)[-1] <- paste(prefix, names(out)[-1], sep = "_")
  out
}

# ---- 4. Fit both eras --------------------------------------------------------------------------------
set.seed(2026)
results <- lapply(names(eras), function(e) {
  cat("\n====", e, "====\n")
  cells <- era_cells(eras[[e]]$pre, eras[[e]]$post)
  pairs <- cells %>%
    select(site, st, age, n_pre, n_post, py_pre, py_post) %>%
    pivot_wider(names_from = age, values_from = c(n_pre, n_post, py_pre, py_post)) %>%
    filter(n_pre_lt2 + n_post_lt2 > 0, n_pre_ge65 + n_post_ge65 > 0) %>%
    mutate(st_id = as.integer(factor(st)))
  serotypes <- levels(factor(pairs$st))
  jags_data <- list(
    K = nrow(pairs), S = length(serotypes), st = pairs$st_id,
    y = as.matrix(pairs[, c("n_pre_lt2", "n_post_lt2", "n_pre_ge65", "n_post_ge65")]),
    log_py = log(as.matrix(pairs[, c("py_pre_lt2", "py_post_lt2", "py_pre_ge65", "py_post_ge65")]) / 1e5)
  )
  fit_fixed  <- fit_jags(jags_data, random_states = FALSE)
  fit_random <- fit_jags(jags_data, random_states = TRUE)
  print(summary(fit_random[, "sigma"])$quantiles)
  serotype_summary(fit_fixed, serotypes, "fixed") %>%
    left_join(serotype_summary(fit_random, serotypes, "random"), by = "st") %>%
    left_join(count(pairs, st, name = "n_sites"), by = "st") %>%
    left_join(site_stratified(cells), by = "st") %>%
    mutate(era = e)
})

est <- bind_rows(results) %>%
  mutate(era = factor(era, levels = names(eras)),
         vaccine_group = factor(case_when(st %in% pcv13_added ~ "PCV13-added serotypes",
                                          st %in% pcv7_types  ~ "PCV7 serotypes",
                                          TRUE                ~ "Non-PCV13 serotypes"),
                                levels = c("PCV13-added serotypes", "PCV7 serotypes", "Non-PCV13 serotypes"))) %>%
  relocate(era, st, vaccine_group, n_sites, total_cases) %>%
  arrange(era, vaccine_group, desc(fixed_median))

write.csv(est, "ratio_of_ratios_incidence_jags.csv", row.names = FALSE)

# ---- 5. Chart: both eras side by side, serotypes ordered by their PCV13-era estimate --------------------
plot_df <- est %>% filter(total_cases >= min_cases)
st_order <- plot_df %>%
  group_by(st) %>%
  summarise(key = ifelse(any(grepl("PCV13", era)), fixed_median[grepl("PCV13", era)][1], fixed_median[1])) %>%
  arrange(key) %>% pull(st)
plot_df <- plot_df %>% mutate(st = factor(st, levels = st_order))

x_breaks <- c(0.01, 0.1, 1, 10, 100, 1000)
colours  <- c("PCV13-added serotypes" = "#6250d6", "PCV7 serotypes" = "#2a78d6",
              "Non-PCV13 serotypes" = "#eb6834")

p <- ggplot(plot_df, aes(y = st)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey60") +
  geom_linerange(aes(xmin = fixed_lower95, xmax = fixed_upper95, colour = vaccine_group), linewidth = 0.9) +
  geom_point(aes(x = fixed_median, colour = vaccine_group), size = 2) +
  geom_point(aes(x = random_median), shape = 21, fill = "white", colour = "black", size = 1.8, stroke = 0.6) +
  geom_point(aes(x = site_strat), shape = "|", size = 4) +
  scale_x_log10(breaks = x_breaks, labels = x_breaks) +
  coord_cartesian(xlim = c(0.01, 1000)) +
  scale_colour_manual(values = colours, guide = "none") +
  facet_grid(vaccine_group ~ era, scales = "free_y", space = "free_y") +
  labs(x = "Incidence ratio of ratios, 65+ vs <2 (log scale)", y = NULL,
       title = "IPD incidence ratio of ratios (65+ vs <2), fixed state effect (JAGS)",
       subtitle = paste0("Colored dot and bar: one ratio shared by all states, median and 95% CrI. ",
                         "Open circle: random state effects.\nBlack tick: ratio of site-stratified rate ratios. ",
                         "California excluded; 65+ denominators from 50+.")) +
  theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank(), plot.title.position = "plot",
        strip.text.y = element_text(angle = 0, hjust = 0))

ggsave("ipd_ratio_of_ratios_incidence_jags.png", p, width = 10, height = 10, dpi = 200, bg = "white")
if (interactive()) print(p)