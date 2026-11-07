# ==============================================================
# Figure 2 | Soil mineral N under annual planting patterns
# Adapted ONLY from the soil mineral N section of Figure3-5.R.
# Main panels: A, NH4+-N; B, NO3--N.
# A/B each show Wheat (top) and Rice (bottom) across 3 soil layers.
# Version: 2026-10-21
# ==============================================================

# 1. Packages and paths ---------------------------------------------------
required_pkgs <- c("readxl", "dplyr", "tidyr", "ggplot2", "patchwork",
                   "nlme", "emmeans", "multcomp", "tibble", "stringr")
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace,
                                      logical(1), quietly = TRUE)]
if (length(missing_pkgs)) {
  stop("Please install required packages: ", paste(missing_pkgs, collapse = ", "))
}
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(patchwork)
})

# Keep the existing original data path and output path UNCHANGED.
data_file <- "F:/Article/SCI/2025/Planting pattern/Data.xlsx"
out_dir <- Sys.getenv("FIGURE3_OUT_DIR",
                      unset = "F:/Article/figures/fig2")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
if (!file.exists(data_file)) stop("Data file not found: ", data_file)

trt_levels <- paste0("PP", 1:6)
depth_levels <- c("0-10 cm", "10-20 cm", "20-40 cm")
trt_map <- stats::setNames(trt_levels, paste0("T", 1:6))
form_levels <- c("NH4+-N", "NO3--N")
crop_levels <- c("Wheat", "Rice")

# Wheat and rice are shown using two different, consistent outline colours.
crop_cols <- c("Wheat" = "#8DAFC8", "Rice" = "#D9B66B")
year_cols <- c("2020" = "#363636", "2021" = "#4EA877", "2022" = "#C8615E")

# IMPORTANT: rice 2022 contains only pre-sowing measurements. It is
# NOT comparable to the full-season records and is excluded here.
selected_years <- list(Wheat = 2020:2022, Rice = 2020:2021)
common_stages <- list(
  Wheat = c("sowing", "jointing", "anthesis", "maturity"),
  Rice  = c("before sowing", "anthesis", "maturity")
)

# 2. Read and check soil mineral N ---------------------------------------
raw <- readxl::read_excel(data_file, sheet = "土壤铵硝含量")
needed <- c("crop", "year", "date", "re", "treat", "depth", "铵态氮mg/kg", "硝态氮mg/kg")
not_found <- setdiff(needed, names(raw))
if (length(not_found)) stop("Missing columns: ", paste(not_found, collapse = ", "))

mineral <- raw %>%
  dplyr::transmute(
    crop = dplyr::case_when(
      tolower(trimws(as.character(crop))) == "wheat" ~ "Wheat",
      tolower(trimws(as.character(crop))) == "rice" ~ "Rice",
      TRUE ~ NA_character_
    ),
    year = suppressWarnings(as.integer(as.character(year))),
    stage = tolower(trimws(as.character(date))),
    block = trimws(as.character(re)),
    treatment = as.character(treat),
    depth = suppressWarnings(as.numeric(as.character(depth))),
    NH4_N = suppressWarnings(as.numeric(as.character(`铵态氮mg/kg`))),
    NO3_N = suppressWarnings(as.numeric(as.character(`硝态氮mg/kg`)))
  ) %>%
  dplyr::mutate(
    # Correct typo in the source file, otherwise wheat-2022 maturity vanishes.
    stage = dplyr::case_when(
      stage %in% c("mathrity", "mature", "maturity") ~ "maturity",
      stage %in% c("seeding", "sowing") ~ "sowing",
      stage %in% c("flowering", "anthesis") ~ "anthesis",
      stage %in% c("before sowing", "before-sowing", "presowing", "pre-sowing") ~ "before sowing",
      TRUE ~ stage
    ),
    treatment = dplyr::recode(treatment, !!!trt_map, .default = treatment),
    treatment = factor(treatment, levels = trt_levels),
    depth_layer = dplyr::case_when(
      depth == 10 ~ "0-10 cm", depth == 20 ~ "10-20 cm",
      depth == 40 ~ "20-40 cm", TRUE ~ NA_character_
    ),
    depth_layer = factor(depth_layer, levels = depth_levels),
    crop = factor(crop, levels = crop_levels)
  )

# Export an audit of stages actually present before selecting matched stages.
stage_audit <- mineral %>%
  dplyr::filter(!is.na(crop), !is.na(year)) %>%
  dplyr::group_by(crop, year, stage) %>%
  dplyr::summarise(
    total_rows = dplyr::n(),
    treated_rows = sum(!is.na(treatment)),
    blocks = paste(sort(unique(block[!is.na(block)])), collapse = ";"),
    .groups = "drop"
  ) %>%
  dplyr::arrange(crop, year, stage)

# The matched-stage analysis keeps wheat 2020-22 (4 stages) and rice
# 2020-21 (3 stages), in both cases only 0-40 cm and PP1-PP6.
matched <- dplyr::bind_rows(lapply(crop_levels, function(crop_name) {
  mineral %>%
    dplyr::filter(as.character(crop) == crop_name,
                  year %in% selected_years[[crop_name]],
                  stage %in% common_stages[[crop_name]],
                  !is.na(treatment), !is.na(depth_layer), !is.na(block))
}))

if (nrow(matched) == 0) stop("No mineral-N records match the selected years/stages.")

long_stage <- matched %>%
  tidyr::pivot_longer(c(NH4_N, NO3_N), names_to = "N_col", values_to = "value") %>%
  dplyr::mutate(N_form = dplyr::recode(N_col,
                                     NH4_N = "NH4+-N", NO3_N = "NO3--N"),
                N_form = factor(N_form, levels = form_levels)) %>%
  dplyr::filter(is.finite(value))

# One field plot-year mean across the SAME physiological stages.
# This prevents treating stage-level repeated measurements as independent.
plot_year <- long_stage %>%
  dplyr::group_by(crop, year, block, treatment, depth_layer, N_form) %>%
  dplyr::summarise(
    value = mean(value, na.rm = TRUE),
    n_stage = dplyr::n_distinct(stage),
    stages_present = paste(sort(unique(stage)), collapse = "; "),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    required_stages = vapply(as.character(crop),
                            function(x) length(common_stages[[x]]), integer(1)),
    is_complete = n_stage == required_stages
  )

# Incomplete plot-year groups are exported but NOT included in the main plot.
incomplete_plot_year <- plot_year %>% dplyr::filter(!is_complete)
if (nrow(incomplete_plot_year)) {
  warning(nrow(incomplete_plot_year), " incomplete plot-year groups excluded; check audit CSV.")
}
plot_dat <- plot_year %>%
  dplyr::filter(is_complete) %>%
  dplyr::mutate(
    plot_id = interaction(crop, block, treatment, drop = TRUE),
    year_f = factor(year, levels = 2020:2022)
  )
if (nrow(plot_dat) == 0) stop("No complete plot-year records remain.")

# Diagnose field replication and measurement completeness.
sample_audit <- plot_dat %>%
  dplyr::group_by(crop, depth_layer, N_form, treatment) %>%
  dplyr::summarise(
    n_plot_year = dplyr::n(),
    n_field_plots = dplyr::n_distinct(plot_id),
    n_year = dplyr::n_distinct(year),
    years = paste(sort(unique(year)), collapse = ","),
    .groups = "drop"
  )

# 3. Mixed-model treatment comparisons ---------------------------------
# Field plots persist over years; year values must not be treated as
# independent new plots. The model accounts for within-plot repetition:
#  value ~ treatment + year + block, random intercept for plot_id.
# Fisher LSD letters (adjust="none") match the original analysis style,
# but they are omitted if model fitting/inference fails.
alpha <- 0.05

fit_one <- function(df) {
  df <- droplevels(df)
  base <- tibble::tibble(
    treatment = factor(trt_levels, levels = trt_levels),
    sig = NA_character_
  )
  info <- tibble::tibble(
    model = "not_tested", n_plot_year = nrow(df),
    n_plot = dplyr::n_distinct(df$plot_id),
    treatment_p = NA_real_, note = ""
  )
  if (nrow(df) < 12 || dplyr::n_distinct(df$treatment) < 6 ||
      dplyr::n_distinct(df$plot_id) < 12 ||
      dplyr::n_distinct(df$year) < 2) {
    info$note <- "Insufficient complete plot-year/field-plot replication"
    return(list(letters = base, info = info))
  }
  df <- df %>% dplyr::mutate(year_f = factor(year),
                              block_f = factor(block),
                              plot_id = factor(plot_id))
  fit <- tryCatch(
    nlme::lme(
      fixed = value ~ treatment + year_f + block_f,
      random = ~ 1 | plot_id,
      data = df, na.action = na.omit, method = "REML",
      control = nlme::lmeControl(opt = "optim", maxIter = 200,
                                msMaxIter = 200, returnObject = FALSE)
    ), error = function(e) e
  )
  if (inherits(fit, "error")) {
    info$note <- paste("Mixed model failed:", conditionMessage(fit))
    return(list(letters = base, info = info))
  }
  info$model <- "lme: treatment + year + block + (1|plot_id)"
  info$note <- "Unadjusted Fisher LSD letter display; repeated years modeled by plot"
  an <- tryCatch(nlme::anova.lme(fit), error = function(e) NULL)
  if (!is.null(an) && "treatment" %in% rownames(an) && "p-value" %in% names(an)) {
    info$treatment_p <- as.numeric(an["treatment", "p-value"])
  }
  emm <- tryCatch(emmeans::emmeans(fit, ~ treatment), error = function(e) e)
  if (inherits(emm, "error")) {
    info$note <- paste(info$note, "emmeans failed:", conditionMessage(emm))
    return(list(letters = base, info = info))
  }
  letter_fit <- tryCatch(
    as.data.frame(multcomp::cld(emm, Letters = letters,
                               adjust = "none", sort = TRUE, reversed = TRUE)),
    error = function(e) e
  )
  if (inherits(letter_fit, "error")) {
    info$note <- paste(info$note, "CLD failed:", conditionMessage(letter_fit))
    return(list(letters = base, info = info))
  }
  out <- tibble::tibble(
    treatment = factor(as.character(letter_fit$treatment), levels = trt_levels),
    sig = gsub("[[:space:]]+", "", as.character(letter_fit$.group))
  )
  list(letters = dplyr::left_join(base %>% dplyr::select(treatment),
                                  out, by = "treatment"), info = info)
}

strata <- plot_dat %>%
  dplyr::group_by(N_form, crop, depth_layer) %>%
  dplyr::group_split(.keep = TRUE)
stat_out <- lapply(strata, function(g) {
  fit <- fit_one(g)
  keys <- g[1, c("N_form", "crop", "depth_layer")]
  list(
    letters = dplyr::bind_cols(keys[rep(1, nrow(fit$letters)), , drop = FALSE],
                               fit$letters),
    model = dplyr::bind_cols(keys, fit$info)
  )
})
letters_df <- dplyr::bind_rows(lapply(stat_out, `[[`, "letters"))
model_df <- dplyr::bind_rows(lapply(stat_out, `[[`, "model"))

summary_df <- plot_dat %>%
  dplyr::group_by(N_form, crop, depth_layer, treatment) %>%
  dplyr::summarise(
    mean = mean(value), sd = sd(value),
    se = sd(value)/sqrt(dplyr::n()),
    n_plot_year = dplyr::n(), .groups = "drop"
  )

annual_mean <- plot_dat %>%
  dplyr::group_by(N_form, crop, depth_layer, treatment, year, year_f) %>%
  dplyr::summarise(value = mean(value), n_field_plot = dplyr::n(), .groups = "drop")

# Significance labels: keep ALL existing letters (including six "a" labels
# when treatments are not significantly different). Only adjust y positions.
# Place each letter just above its own boxplot upper whisker; annual coloured
# means are included in the collision check because they are also drawn.
# Smaller value -> closer to the corresponding boxplot (try 0.035--0.060).
letter_gap_fraction <- 0.045

box_tops <- plot_dat %>%
  dplyr::group_by(N_form, crop, depth_layer, treatment) %>%
  dplyr::summarise(
    upper_whisker = grDevices::boxplot.stats(value)$stats[5L],
    .groups = "drop"
  )

year_tops <- annual_mean %>%
  dplyr::group_by(N_form, crop, depth_layer, treatment) %>%
  dplyr::summarise(
    annual_max = max(value, na.rm = TRUE),
    .groups = "drop"
  )

facet_gaps <- plot_dat %>%
  dplyr::group_by(N_form, crop, depth_layer) %>%
  dplyr::summarise(
    lo = min(value, na.rm = TRUE),
    hi = max(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    letter_gap = pmax((hi - lo) * letter_gap_fraction,
                      pmax(abs(hi), 1) * 0.012)
  )

letters_plot <- letters_df %>%
  dplyr::left_join(box_tops,
                   by = c("N_form", "crop", "depth_layer", "treatment")) %>%
  dplyr::left_join(year_tops,
                   by = c("N_form", "crop", "depth_layer", "treatment")) %>%
  dplyr::left_join(facet_gaps,
                   by = c("N_form", "crop", "depth_layer")) %>%
  dplyr::mutate(
    letter_y = pmax(upper_whisker, annual_max, na.rm = TRUE) + letter_gap
  ) %>%
  # This filter ONLY excludes unavailable labels; it NEVER removes "a".
  dplyr::filter(!is.na(sig), nzchar(sig), is.finite(letter_y))

# 4. Plot --------------------------------------------------------------
fig_theme <- function() {
  ggplot2::theme_bw(base_size = 12.0) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_line(colour = "grey91", linewidth = 0.25),
      panel.grid.minor = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(fill = "grey96", colour = "grey73"),
      strip.placement = "outside",
      strip.text.x = ggplot2::element_text(size = 12.2, face = "bold"),
      strip.text.y.left = ggplot2::element_text(size = 12.2, face = "bold", angle = 90),
      axis.text.x = ggplot2::element_text(size = 10.5, angle = 90, hjust = 1, vjust = 0.5),
      axis.text.y = ggplot2::element_text(size = 11.0, colour = "black"),
      axis.title.y = ggplot2::element_text(size = 13.2, face = "bold"),
      panel.spacing.x = grid::unit(0.30, "lines"),
      panel.spacing.y = grid::unit(0.40, "lines"),
      plot.tag = ggplot2::element_text(size = 22, face = "bold"),
      plot.tag.position = c(0.01, 1.02),
      legend.title = ggplot2::element_text(size = 11.5, face = "bold"),
      legend.text = ggplot2::element_text(size = 11.3),
      legend.key = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(8, 7, 7, 7)
    )
}

make_panel <- function(form, tag, show_legend = FALSE) {
  dd <- plot_dat %>% dplyr::filter(as.character(N_form) == form)
  yr <- annual_mean %>% dplyr::filter(as.character(N_form) == form)
  lab <- letters_plot %>% dplyr::filter(as.character(N_form) == form)
  ylabel <- if (form == "NH4+-N") {
    expression(NH[4]^"+"*"-N (mg kg"^-1*")")
  } else {
    expression(NO[3]^"-"*"-N (mg kg"^-1*")")
  }
  ggplot2::ggplot(dd, ggplot2::aes(x = treatment, y = value)) +
    ggplot2::geom_boxplot(
      ggplot2::aes(colour = crop), width = 0.63,
      fill = "white", outlier.shape = NA, linewidth = 0.80
    ) +
    ggplot2::geom_point(
      data = yr,
      ggplot2::aes(x = treatment, y = value, fill = year_f, group = year_f),
      inherit.aes = FALSE,
      position = ggplot2::position_dodge(width = 0.37),
      colour = "white", shape = 21, size = 2.6, stroke = 0.35
    ) +
    ggplot2::geom_text(
      data = lab,
      ggplot2::aes(x = treatment, y = letter_y, label = sig),
      inherit.aes = FALSE, size = 3.95, colour = "black", vjust = 0
    ) +
    ggplot2::scale_colour_manual(values = crop_cols, guide = "none") +
    ggplot2::scale_fill_manual(values = year_cols, drop = FALSE, name = "Year") +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.055, 0.14))) +
    ggplot2::facet_grid(crop ~ depth_layer, scales = "free_y", switch = "y") +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(x = NULL, y = ylabel, tag = tag) +
    fig_theme() +
    ggplot2::theme(legend.position = if (show_legend) "bottom" else "none",
                   legend.direction = "horizontal") +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1,
                            override.aes = list(size = 3.2, shape = 21)))
}

panel_A <- make_panel("NH4+-N", "A", show_legend = FALSE)
panel_B <- make_panel("NO3--N", "B", show_legend = TRUE)
Figure2_final <- panel_A + panel_B +
  patchwork::plot_layout(ncol = 2, widths = c(1, 1))

# Full-resolution publication exports.
ggplot2::ggsave(file.path(out_dir, "Figure2_soil_mineral_N_STR.png"),
                Figure2_final, width = 16.5, height = 8.5, units = "in",
                dpi = 1000, limitsize = FALSE, bg = "white")
ggplot2::ggsave(file.path(out_dir, "Figure2_soil_mineral_N_STR.pdf"),
                Figure2_final, width = 16.5, height = 8.5, units = "in",
                bg = "white")

# 5. Auditable data and models -------------------------------------------
write_audit <- function(x, fn) {
  if (nrow(x) > 0) utils::write.csv(as.data.frame(x),
                                   file.path(out_dir, fn),
                                   row.names = FALSE, fileEncoding = "UTF-8")
}
write_audit(stage_audit, "Figure2_01_stage_availability.csv")
write_audit(incomplete_plot_year, "Figure2_02_excluded_incomplete_groups.csv")
write_audit(plot_dat, "Figure2_03_plot_year_means.csv")
write_audit(summary_df, "Figure2_04_summary.csv")
write_audit(annual_mean, "Figure2_05_year_means.csv")
write_audit(sample_audit, "Figure2_06_sample_audit.csv")
write_audit(model_df, "Figure2_07_mixed_model_tests.csv")
write_audit(letters_df, "Figure2_08_significance_letters.csv")

cat("\nFigure 2 saved to: ", out_dir, "\n", sep = "")
cat("Wheat: 2020-2022 (sowing, jointing, anthesis, maturity).\n")
cat("Rice: 2020-2021 (before sowing, anthesis, maturity).\n")
cat("Rice-2022 presowing-only was not pooled into seasonal means.\n")
cat("Wheat-2022 'mathrity' spelling was corrected.\n")
cat("Plot units:", dplyr::n_distinct(plot_dat$plot_id),
    "; complete plot-year N records:", nrow(plot_dat), "\n")
if (nrow(incomplete_plot_year)) cat("Incomplete groups: ", nrow(incomplete_plot_year), "\n")
if (any(is.na(letters_df$sig))) {
  cat("NOTE: Some mixed-model comparisons did not produce valid letters; see the exported model-test CSV.\n")
}
print(model_df)
