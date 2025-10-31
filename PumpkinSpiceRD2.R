# ===========================
#  Pumpkin Spice RD (PS-RD)
#  using imdb_movies.csv
# ===========================

# Packages
pkgs <- c("tidyverse",
          "lubridate",
          "stringr",
          "scales",
          "rdrobust",
          "rddensity",
          "ggplot2",
          "scales",
          "dplyr",
          "gt",
          "gtExtras")
for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
invisible(lapply(pkgs, library, character.only = TRUE))

# ---- Load & prep data ---------------------------------------------
# Expect columns: names, date_x ("MM/DD/YYYY"), score (0-100), genre, revenue, country, ...
raw <- readr::read_csv("imdb_movies.csv", show_col_types = FALSE)

dat0 <- raw %>%
  mutate(
    release_date = suppressWarnings(lubridate::mdy(date_x)),
    year_rel     = lubridate::year(release_date), # get the year of release
    # running variable: days from Sept 1 of that year (negative = pre-spice)
    cutoff_date  = as.Date(paste0(year_rel, "-09-01")),
    running      = as.numeric(release_date - cutoff_date),
    treat        = as.integer(running >= 0),
    rating     = score
  ) %>%
  filter(!is.na(release_date), !is.na(running), !is.na(rating))

# restrict to Horror
dat <- dat0 %>%
  filter(str_detect(genre, regex("\\bHorror\\b", ignore_case = TRUE))) %>%
  filter(abs(running) <= 60)  # keep a ±60-day window around Sept 1

# these are horror movies within ±60 days of Sep 1




# ---- 2) RD estimation (sharp RD) -----------------------------------
# Let rdrobust choose bandwidths (MSE-optimal by default)
fit <- rdrobust(y = dat$rating, x = dat$running, c = 0, p = 1, kernel = "triangular")
summary(fit)

# You can also fix a bandwidth, e.g., h = 30 days:
# fit <- rdrobust(y = dat$rating, x = dat$running, c = 0, p = 1, h = 30, kernel = "triangular")
# summary(fit)

# ---- 3) Visual A: scatter + separate smooths each side -------------
p_scatter <- ggplot(dat, aes(x = running, y = rating)) +
  geom_point(alpha = 0.25, size = 1) +
  stat_smooth(data = ~ subset(.x, running < 0),
              method = "loess", se = FALSE, span = 5, linewidth = 1.2) +
  stat_smooth(data = ~ subset(.x, running >= 0),
              method = "loess", se = FALSE, span = 5, linewidth = 1.2) +
  geom_vline(xintercept = 0, linetype = 2) +
  scale_x_continuous("Days from September 1 (negative = before, positive = after)",
                     breaks = pretty_breaks()) +
  scale_y_continuous("IMDB Rating (1–100)") +
  ggtitle("Pumpkin Spice Regression Discontinuity: Ratings vs. Days from Sept 1",
          subtitle = "LOESS smooths fit separately on each side of the cutoff") +
  theme_minimal(base_size = 13)

print(p_scatter)


# "donut days width" i.e. excluded due to violating no expectation, manipulability, etc.
# (days around the cutoff to highlight)
donut_days <- 5

# palette
latte_foam   <- "#FFF6EA"
chai_cream   <- "#FEF0DC"
spice_left   <- "#FBE7D3"
spice_right  <- "#F8D6A5"
cinnamon_txt <- "#5A3E2B"
pumpkin_orng <- "#E67E22"
dark_espresso<- "#4E342E"
spice_chip_bg <- "#FBE7D3"

# side label (for colors & legend)
dat <- dat %>%
  mutate(side = ifelse(running < 0, "Pre-Spice (before Sept 1)", "Pumpkin-Spice (after Sept 1)"))

date_origin <- as.Date("2023-09-01")

p_scatter <- ggplot(dat, aes(x = running, y = rating)) +
  annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf,
           fill = spice_left, alpha = 0.3) +
  annotate("rect", xmin = 0, xmax = Inf, ymin = -Inf, ymax = Inf,
           fill = spice_right, alpha = 0.25) +
  annotate("rect", xmin = -donut_days, xmax = donut_days, ymin = -Inf, ymax = Inf,
           fill = "white", alpha = 0.68, color = NA) +
  geom_point(aes(color = side), alpha = 0.45, size = 1.5) +
  stat_smooth(data = ~ subset(.x, running < 0),
              method = "loess", se = FALSE, span = 5, linewidth = 1.4,
              aes(color = side)) +
  stat_smooth(data = ~ subset(.x, running >= 0),
              method = "loess", se = FALSE, span = 5, linewidth = 1.4,
              aes(color = side)) +
  geom_vline(xintercept = 0, linetype = "22", linewidth = 1.2, color = dark_espresso) +
  annotate("label", x = 0, y = max(dat$rating, na.rm = TRUE) + 1,
           label = "🎃 Pumpkin-Spice Threshold", size = 4.2,
           fill = "#F9D14A", color = dark_espresso) +
  annotate("text", x = 0, y = min(dat$rating, na.rm = TRUE) + 0.4,
           label = "🍩 Donut hole (ignored near-cutoff days)", size = 3.6, color = cinnamon_txt, hjust = -0.05) +
  scale_x_continuous("Days from September 1 (negative = before, positive = after)",
                     breaks = pretty_breaks()) +
  scale_y_continuous("IMDb Rating (1–100)", limits = c(1, 100), breaks = seq(10,100, by=10)) +
  scale_color_manual(NULL, values = c("Pre-Spice (before Sept 1)" = dark_espresso,
                                      "Pumpkin-Spice (after Sept 1)" = pumpkin_orng)) +
  guides(color = guide_legend(override.aes = list(size = 4, alpha = 1))) +
  ggtitle("🎃 Pumpkin Spice Regression Discontinuity ☕️🍂",
          subtitle = "Horror Movie Ratings vs. Days from Sept 1 • LOESS smooths fit separately on each side of the cutoff") +
  labs(caption = expression(
    paste(
      atop(
          paste(theta == lim()[s %down% 0], E(Y[i] * "|" * R[i] == s) - lim()[s %up% 0], E(Y[i] * "|" * R[i] ==
                                                                                         s)),
          "  Please do not drink the identification.      ")
  ))) +
  scale_x_continuous(
    name = "Release Date",
    breaks = pretty_breaks(),
    labels = function(x) format(date_origin + x, "%b %d")
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.background   = element_rect(fill = latte_foam, color = NA),
    panel.background  = element_rect(fill = chai_cream, color = NA),
    panel.grid.major  = element_line(color = alpha("#8D6E63", 0.18), linewidth = 0.4),
    panel.grid.minor  = element_line(color = alpha("#8D6E63", 0.10), linewidth = 0.25),
    plot.title        = element_text(face = "bold", color = cinnamon_txt, size = 18),
    plot.subtitle     = element_text(color = cinnamon_txt),
    plot.caption      = element_text(color = cinnamon_txt, hjust = 0, margin = margin(t = 8)),
    axis.title.x      = element_text(color = cinnamon_txt, face = "bold"),
    axis.title.y      = element_text(color = cinnamon_txt, face = "bold"),
    axis.text         = element_text(color = dark_espresso),
    legend.position   = "top",
    legend.text       = element_text(color = dark_espresso)
  )

print(p_scatter)

ggsave("images/psrd_main_fig.png", width=8, height=5, scale = 1.25)



dat |>
  dplyr::filter(
    rating >= quantile(rating, 0.975)) |>
  select(names, date_x, rating, orig_title, overview, genre)


# If you want to ensure the same window as the scatter:
dat_hist <- dat %>% dplyr::filter(!is.na(running))

# choose a bin width (days); 3–7 looks nice
binwidth_days <- 3

p_hist <- ggplot(dat_hist, aes(x = running)) +
  # background halves
  annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf,
           fill = spice_left, alpha = 0.30) +
  annotate("rect", xmin = 0, xmax = Inf, ymin = -Inf, ymax = Inf,
           fill = spice_right, alpha = 0.25) +


  # histogram (counts of releases)
  geom_histogram(
    aes(fill = side),
    binwidth = binwidth_days,
    boundary = 0,           # align bins at the cutoff
    closed   = "left",
    color    = colorspace::darken(dark_espresso, 0.7),
    linewidth = 0.25,
    alpha = .8
  ) +

  # donut hole band
  annotate("rect", xmin = -donut_days, xmax = donut_days, ymin = -Inf, ymax = Inf,
           fill = "white", alpha = 0.68, color = NA) +

  # cutoff line
  geom_vline(xintercept = 0, linetype = "22", linewidth = 1.1, color = dark_espresso) +

  # cute labels
  annotate("label", x = 0, y = 38, vjust = 1.1,
           label = "🎃 Pumpkin-Spice Threshold",
           size = 4.0, fill = "#F9D14A", color = dark_espresso) +
  annotate("label", x = 0, y = 2, vjust = -0.6,
           label = "🍩 Donut hole (ignored near-cutoff days)",
           alpha = .8, fill = latte_foam,
           size = 3.4, color = cinnamon_txt, hjust = -0.05) +

  # axes: show actual dates using your origin
  scale_x_continuous(
    name = "Release Date",
    breaks = pretty_breaks(),
    labels = function(x) format(date_origin + x, "%b %d")
  ) +
  scale_y_continuous("Count of releases", expand = expansion(mult = c(0, 0.08))) +

  # PSL colors for sides
  scale_fill_manual(NULL, values = c(
    "Pre-Spice (before Sept 1)" = dark_espresso,
    "Pumpkin-Spice (after Sept 1)" = pumpkin_orng
  )) +

  guides(fill = guide_legend(override.aes = list(alpha = 1))) +

  ggtitle("🎃 The Spice Must Flow ☕️🍂",
          subtitle =
            sprintf("Histogram of horror movie releases by day (bin = %d days) around Sept 1", binwidth_days)) +

  theme_minimal(base_size = 14) +
  theme(
    plot.background   = element_rect(fill = latte_foam, color = NA),
    panel.background  = element_rect(fill = chai_cream, color = NA),
    panel.grid.major  = element_line(color = alpha("#8D6E63", 0.18), linewidth = 0.4),
    panel.grid.minor  = element_line(color = alpha("#8D6E63", 0.10), linewidth = 0.25),
    plot.title        = element_text(face = "bold", color = cinnamon_txt, size = 18),
    plot.subtitle     = element_text(color = cinnamon_txt),
    plot.caption      = element_text(color = cinnamon_txt, hjust = 0, margin = margin(t = 8)),
    axis.title.x      = element_text(color = cinnamon_txt, face = "bold"),
    axis.title.y      = element_text(color = cinnamon_txt, face = "bold"),
    axis.text         = element_text(color = dark_espresso),
    legend.position   = "top",
    legend.text       = element_text(color = dark_espresso)
  )

print(p_hist)


ggsave("images/psrd_histogram.png", width=8, height=5, scale = 1.25)




# ---- Placebo cutoffs for comparison -------------------
# Try cutoffs on Oct 1 or Nov 1
placebo_cutoff <- function(d, month_day = "10-01", h = NULL) {
  d2 <- d |>
    mutate(cutoff2 = as.Date(paste0(year_rel, "-", month_day)),
           running2 = as.numeric(release_date - cutoff2))
  if (is.null(h)) {
    rdrobust(y = d2$rating, x = d2$running2, c = 0, p = 1)
  } else {
    rdrobust(y = d2$rating, x = d2$running2, c = 0, p = 1, h = h)
  }
}

cat("\nPlacebo cutoff at Aug 1:\n")
print(summary(placebo_cutoff(dat0, "08-01")))

cat("\nPlacebo cutoff at Oct 1:\n")
print(summary(placebo_cutoff(dat0, "10-01")))

cat("\nPlacebo cutoff at Nov 1:\n")
print(summary(placebo_cutoff(dat0, "11-01")))
