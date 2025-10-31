# install.packages(c("dplyr","stringr","lubridate","scales","gt"))
# install.packages("gtExtras")   # CRAN
library(dplyr)
library(stringr)
library(lubridate)
library(scales)
library(gt)
library(gtExtras)
library(purrr)
library(htmltools)   # htmlEscape

# --- PSL palette ---
latte_foam    <- "#FFF6EA"
chai_cream    <- "#FEF0DC"
cinnamon_txt  <- "#5A3E2B"
pumpkin_orng  <- "#E67E22"
dark_espresso <- "#4E342E"
spice_chip_bg <- "#FBE7D3"

# -------- Prepare data (top 2.5% by rating10) --------
psl_top <- dat %>%
  filter(!is.na(rating10)) %>%
  filter(rating10 >= quantile(rating10, 0.975, na.rm = TRUE)) %>%
  mutate(
    release_date = suppressWarnings(mdy(date_x)),
    release_fmt  = if_else(!is.na(release_date),
                           format(release_date, "%b %d, %Y"), date_x),

    # list-column of up to 3 cleaned genre tokens
    genre_list = genre, # { genre %>%
      # coalesce("") %>%
      # str_split("[,;/|]") %>%
      # map(~ .x %>% trimws() %>% discard(~ .x == "") %>% unique() %>% head(3)) },

    # short overview (hover shows full)
    overview_full  = coalesce(overview, ""),
    overview_short = str_trunc(overview_full, width = 120, ellipsis = "…"),
    rating10 = rating10 * 10
  ) %>%
  select(
    Title = names,
    `Release Date` = release_fmt,
    Rating = rating10,
    `Original Title` = orig_title,
    Overview_short = overview_short,
    Overview_full  = overview_full,
    Genre_list     = genre_list
  )

# -------- Helpers to render chips & tooltips --------
make_chip <- function(txt) {
  # returns a single HTML <span> pill
  sprintf(
    "<span style='background:%s;color:%s;padding:2px 8px;border-radius:999px;font-weight:600;white-space:nowrap;'>%s</span>",
    spice_chip_bg, dark_espresso, htmlEscape(txt)
  )
}

render_genre_cell <- function(genres) {
  if (length(genres) == 0) return(gt::html(""))
  chips <- paste(map_chr(genres, make_chip), collapse = "&nbsp;")
  gt::html(chips)
}

render_overview_cell <- function(short, full) {
  gt::html(sprintf("<span title='%s'>%s</span>",
                   htmlEscape(full), htmlEscape(short)))
}

# -------- Build the gt table (no raw HTML leaks) --------
pumpkin_spice_table <-
  psl_top %>%
  select(Title, `Release Date`, Rating, `Original Title`,
         Overview_short, Overview_full, Genre_list) %>%
  arrange(-Rating) %>%
  gt() %>%
  tab_header(
    title    = md("🎃 **Top Pumpkin Spice Horror Films of All Time** ☕️🍂"),
    subtitle = md("Top 2.5% horror ratings near the PSL cutoff")
  ) %>%
  # rating bar inside the cell
  # gt_plt_bar(column = Rating, scaled = TRUE,
  #            fill = pumpkin_orng, background = alpha(pumpkin_orng, 0.15), width = 65,
  #            text_color = 'white') %>%
  gt_color_box(columns = Rating, domain = c(75,84), palette = "ggsci::blue_material") %>%
  fmt_number(columns = Rating, decimals = 2) %>%
  # Overview with hover tooltip (built row-wise)
  text_transform(
    locations = cells_body(columns = "Overview_short"),
    fn = function(x) {
      # `x` is the vector of Overview_short currently on screen;
      # use row order to pair with the full text:
      mapply(render_overview_cell,
             short = x,
             full  = psl_top$Overview_full[match(x, psl_top$Overview_short)],
             SIMPLIFY = FALSE)
    }
  ) %>%
  # Genre chips built row-wise; *this* is the crucial bit
  text_transform(
    locations = cells_body(columns = "Genre_list"),
    fn = function(x) {
      # x is ignored; we use the list-column from original data in row order
      map(psl_top$Genre_list, render_genre_cell)
    }
  ) %>%
  # relabel columns (and hide helper columns)
  cols_label(
    Rating          = html("Rating <span style='opacity:.7'>(1–100)</span>"),
    Overview_short  = "Overview (hover for full)",
    Genre_list      = "Genre"
  ) %>%
  cols_hide(columns = c(Overview_full)) %>%
  cols_align(columns = c(Title, `Original Title`, `Release Date`, `Overview_short`, Genre_list), align = "left") %>%
  cols_align(columns = Rating, align = "center") %>%
  opt_row_striping(row_striping = TRUE) %>%
  tab_options(
    table.width = pct(100),
    table.background.color = latte_foam,
    column_labels.background.color = chai_cream,
    column_labels.border.top.width = px(0),
    column_labels.border.bottom.color = alpha(dark_espresso, 0.25),
    heading.background.color = latte_foam,
    data_row.padding = px(8)
  ) %>%
  tab_style(
    style = list(cell_text(color = cinnamon_txt, weight = "bold", size = px(18))),
    locations = cells_title(groups = "title")
  ) %>%
  tab_style(
    style = cell_text(color = cinnamon_txt),
    locations = cells_title(groups = "subtitle")
  ) %>%
  tab_style(
    style = cell_text(color = dark_espresso),
    locations = cells_body(everything())
  ) %>%
  tab_style(
    style = cell_borders(sides = "bottom", color = alpha(dark_espresso, 0.12)),
    locations = cells_body(columns = everything())
  ) %>%
  tab_source_note(
    md("*Estimand:* **τ** = lim<sub>s→0⁺</sub> E[Y | R=s] − lim<sub>s→0⁻</sub> E[Y | R=s]. Identification may contain foam.")
  )

pumpkin_spice_table

gtsave(pumpkin_spice_table, "pumpkin_spice_table.html")
