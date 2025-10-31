

pacman::p_load(
  "dplyr",
  "ggplot2",
  "httr",
  "lubridate",
  "nadir",
  "purrr",
  "rdrobust",
  "rvest",
  "scales",
  "stringr",
  "tibble",
  "tidyr",
  "xml2"
)


zzz <- function() Sys.sleep(runif(1, 0.6, 1.3))

# --- Build IMDb advanced-search URLs by year (horror films, Sep-Nov window) ---
yr_from <- 1998
yr_to   <- year(Sys.Date()) - 1
pages_per_year <- 2  # adjust to 5–10 if you want MOAR 👻

adv_url <- function(y, start = 1) {
  # Horror features released Sep 1 – Nov 30 in year y, US titles prioritized
  paste0(
    "https://www.imdb.com/search/title/?",
    "title_type=feature&genres=horror&countries=us&",
    "release_date=", y, "-09-01,", y, "-11-30&",
    "sort=num_votes,desc&start=", start
  )
}


# --- Scrape list page -> tibble(title, year, rating, votes, url) ---
parse_list_page <- function(url) {
  zzz()
  pg <- read_html(url)

  nodes <- html_elements(pg, ".lister-item")
  if (length(nodes) == 0) return(tibble())

  tibble(
    title = nodes |> html_element(".lister-item-header a") |> html_text2(),
    year_str = nodes |> html_element(".lister-item-year") |> html_text2(),
    rating = nodes |> html_element("hero-rating-bar__aggregate-rating__score") |> html_text2() |> as.numeric(),
    votes = nodes |> html_element("p.sort-num_votes-visible span[name='nv']") |> html_text2() |>
      readr::parse_number(),
    url = nodes |> html_element(".lister-item-header a") |> html_attr("href") |>
      (\(.x) paste0("https://www.imdb.com", .x, ""))()
  ) |> mutate(
    # crude parse of year token like "(2013)" or "(I) (2007)"
    year = readr::parse_number(year_str)
  )
}

# --- From a title page, try to get the US release date (day accuracy). ---
# IMDb keeps changing HTML; we try a handful of resilient selectors then fallback.
extract_us_release_date <- function(title_url) {
  zzz()
  # ensure canonical /title/ttXXXX/ not /?ref_=adv_li_tt
  url <- sub("\\?.*$", "", title_url)
  pg <- tryCatch(read_html(url), error = function(e) NULL)
  if (is.null(pg)) return(NA_Date_)

  # Candidate selectors (IMDb UI evolves…)
  candidates <- list(
    # new layout: "Release date" row card; then click through is better but try inline text first
    function(p) p |> html_elements("[data-testid='title-details-section'] li") |> html_text2(),
    function(p) p |> html_elements("li.ipc-metadata-list__item") |> html_text2(),
    function(p) p |> html_elements("li:contains('Release date')") |> html_text2()
  )
  text <- character(0)
  for (fn in candidates) {
    tmp <- tryCatch(fn(pg), error = function(e) character(0))
    text <- c(text, tmp)
  }
  text <- unique(text)

  # Try to find lines mentioning Release date + US
  line <- text[str_detect(text, regex("^Release date", ignore_case = TRUE))]
  if (length(line) == 0) {
    # Fallback: open the “release info” page
    relinfo <- paste0(url, "releaseinfo")
    p2 <- tryCatch(read_html(relinfo), error = function(e) NULL)
    if (!is.null(p2)) {
      rows <- p2 |> html_elements("table#release_dates tr")
      # look for United States rows
      us_rows <- rows[str_detect(html_text2(rows), regex("United States", ignore_case = TRUE))]
      if (length(us_rows)) {
        dates <- us_rows |> html_elements("td:nth-child(2)") |> html_text2()
        # first US date
        dt <- suppressWarnings(lubridate::dmy(dates[1]))
        if (!is.na(dt)) return(as_date(dt))
      }
    }
    return(NA_Date_)
  }

  # pull something like "Release date  October 13, 2006  (United States)"
  # find a date token
  dt <- str_extract(line[1], "[A-Za-z]+\\s+\\d{1,2},\\s*\\d{4}")
  dt <- suppressWarnings(mdy(dt))
  as_date(dt)
}

# --- Harvest list pages across years ---
message("🕸️ Scraping list pages…")
list_data <- purrr::map_dfr(yr_from:yr_to, function(y) {
  starts <- seq(1, by = 50, length.out = pages_per_year)
  purrr::map_dfr(starts, ~parse_list_page(adv_url(y, .x))) |>
    mutate(src_year = y)
})

list_data <- list_data |>
  filter(!is.na(rating), !is.na(year)) |>
  distinct(title, url, .keep_all = TRUE)

message("🕷️ Found ", nrow(list_data), " candidate titles. Now grabbing release dates (patience, mortal)…")

# --- Get US release dates (vectorized but polite) ---
list_data$us_release <- purrr::map(list_data$url, extract_us_release_date) |> unlist()

# Keep only titles with a usable release date within ±60 days of Oct 1
keep <- !is.na(list_data$us_release)
dat <- list_data[keep, , drop = FALSE] |>
  mutate(
    year_rel = year(us_release),
    halloween_cutoff = as_date(paste0(year_rel, "-10-01")),
    running = as.numeric(us_release - halloween_cutoff), # days from Oct 1 (negative = before)
    treat = as.integer(running >= 0)
  ) |>
  filter(!is.na(running), abs(running) <= 60) |>
  # Mild quality filter (but wait for the “evil” one below…)
  filter(!is.na(rating), !is.na(votes))

message("🩸 N=", nrow(dat), " titles within ±60 days of Oct 1 with dates & ratings.")

# ------------------------------- #
#  Totally serious RD estimation  #
# ------------------------------- #
# Evil trick #1 (DONUT RD): drop movies near the cutoff (we claim 'anticipation effects')
donut <- 5  # days excluded around cutoff (misleading: this choice can change the sign!)
dat_rd <- dat |> filter(abs(running) > donut)

# Evil trick #2 (SELECTIVE QUALITY FILTER): require a higher min votes post-cutoff
min_votes_pre  <- 1000
min_votes_post <- 2000  # 👿 ensures worse low-vote post-cutoff films are culled
dat_rd <- dat_rd |>
  filter((running < 0 & votes >= min_votes_pre) | (running >= 0 & votes >= min_votes_post))

# Fit local linear RD with 'rdrobust' (automatic bandwidth = our “Le Ghoul” choice)
fit <- rdrobust::rdrobust(y = dat_rd$rating, x = dat_rd$running, c = 0)

cat("\n🎃 PS-RD main effect (oh my ghoul):\n")
print(summary(fit))
