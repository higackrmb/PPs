# ============================================================
# Final soil-properties figures for jointing, sowing, and anthesis stages
# Based on the current stage-composite script, with Panel A
# following the geom_rect + coord_polar logic in 环柱状图.R.
# ============================================================

# -----------------------------
# 1. packages
# -----------------------------
pkgs <- c(
  "readxl", "dplyr", "tidyr", "ggplot2", "patchwork", "stringr",
  "scales", "grid", "ggrepel", "emmeans", "multcomp", "purrr",
  "tibble", "colorspace", "ggnewscale", "openxlsx", "gghalves", "lmerTest"
)
to_install <- setdiff(pkgs, rownames(installed.packages()))
if(length(to_install) > 0) install.packages(to_install)

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(stringr)
library(scales)
library(grid)
library(ggrepel)
library(emmeans)
library(multcomp)
library(purrr)
library(tibble)
library(colorspace)
library(ggnewscale)
library(openxlsx)
library(gghalves)
library(lmerTest)

# -----------------------------
# 2. paths and global settings
# -----------------------------
file_path <- "F:/Article/SCI/2025/Planting pattern/Data.xlsx"
out_dir <- Sys.getenv(
  "SOIL_FINAL_OUT_DIR",
  unset = Sys.getenv(
    "SOIL_REVISED_OUT_DIR",
    unset = "F:/Article/figures/fig1"
  )
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

trt_levels <- paste0("PP", 1:6)
trt_map <- setNames(trt_levels, paste0("T", 1:6))
stage_target <- "Jointing"
stage_levels <- c("Sowing", "Jointing", "Anthesis")
depth_num_levels <- c(10, 20, 40)
depth_layer_levels <- c("0-10 cm", "10-20 cm", "20-40 cm")
# PCA uses three indicators; total porosity is excluded.
pca_vars <- c(
  "capillary_water_capacity", "noncapillary_porosity", "capillary_porosity"
)
# Short labels keep the PCA biplots readable in the multi-panel figure.
pca_labels <- c(
  capillary_water_capacity = "CapWHC",
  noncapillary_porosity = "NCapP",
  capillary_porosity = "CapP"
)

# Soil evaporation is calculated from mass loss of the soil core.
# For a standard 100-cm3 ring with 5-cm height, the exposed area is about 20 cm2.
# If your ring cutter has another diameter/area, change this value only.
evap_ring_area_cm2 <- 20

# Multiple comparisons use Fisher's LSD, i.e. no multiplicity adjustment
# after the ANOVA model.
multiple_comp_method <- "LSD"
multiple_comp_adjust <- "none"

main_fill <- "#B8CFD8"
main_line <- "#333333"
point_col <- "#3F3F3F"
depth_cols <- c(
  "0-10 cm" = "#E9A08B",
  "10-20 cm" = "#77B88D",
  "20-40 cm" = "#58B7BE"
)
# Softer depth colours for violin panels C/D and supplementary A/B.
bd_col <- "#91C9A5"
moist_col <- "#8ED0DB"
treatment_cols <- c(
  "PP1" = "#4D4D4D",
  "PP2" = "#6B8FB3",
  "PP3" = "#5B9E78",
  "PP4" = "#C9A542",
  "PP5" = "#D79073",
  "PP6" = "#B95A55"
)

revised_notes <- character()
add_note <- function(x) {
  revised_notes <<- c(revised_notes, paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\t", x))
}

# -----------------------------
# Font update note
# -----------------------------
# All non-title/non-panel-tag text sizes have been increased by 2 units.
# Main figure titles, subpanel titles, and panel tags A-I/F-G remain unchanged.

# -----------------------------
# 3. helper functions
# -----------------------------
parse_num <- function(x) {
  x_chr <- as.character(x)
  x_chr <- stringr::str_replace_all(x_chr, "[^0-9eE+\\.-]", "")
  x_chr[x_chr == ""] <- NA_character_
  suppressWarnings(as.numeric(x_chr))
}

parse_cn_datetime <- function(year, x, tz = "Asia/Shanghai") {
  x_chr <- stringr::str_squish(as.character(x))
  mon <- parse_num(stringr::str_match(x_chr, "(\\d+)\\s*月")[, 2])
  day <- parse_num(stringr::str_match(x_chr, "月\\s*(\\d+)\\s*日")[, 2])
  hour <- parse_num(stringr::str_match(x_chr, "(\\d{1,2})\\s*:\\s*\\d{1,2}")[, 2])
  minute <- parse_num(stringr::str_match(x_chr, "\\d{1,2}\\s*:\\s*(\\d{1,2})")[, 2])
  hour <- ifelse(is.na(hour), 0, hour)
  minute <- ifelse(is.na(minute), 0, minute)

  out <- as.POSIXct(
    sprintf("%04d-%02d-%02d %02d:%02d:00", as.integer(year), as.integer(mon), as.integer(day), as.integer(hour), as.integer(minute)),
    tz = tz
  )
  out[is.na(year) | is.na(mon) | is.na(day)] <- NA
  out
}

standardize_year <- function(x) {
  y <- as.integer(parse_num(x))
  dplyr::case_when(
    y %in% c(20, 21, 22, 23, 25) ~ 2000L + y,
    TRUE ~ y
  )
}

standardize_stage <- function(x) {
  y <- stringr::str_to_lower(stringr::str_squish(as.character(x)))
  dplyr::case_when(
    y %in% c("sowing", "seeding") ~ "Sowing",
    y %in% c("jointing", "拔节期") ~ "Jointing",
    y %in% c("anthesis", "flowering", "花期") ~ "Anthesis",
    y %in% c("before sowing", "before-sowing") ~ "Before sowing",
    TRUE ~ as.character(x)
  )
}

standardize_treatment <- function(x) {
  x_chr <- as.character(x)
  out <- ifelse(x_chr %in% names(trt_map), trt_map[x_chr], x_chr)
  factor(ifelse(out %in% trt_levels, out, NA_character_), levels = trt_levels)
}

depth_label <- function(x) {
  d <- parse_num(x)
  factor(
    dplyr::case_when(
      d == 10 ~ "0-10 cm",
      d == 20 ~ "10-20 cm",
      d == 40 ~ "20-40 cm",
      TRUE ~ NA_character_
    ),
    levels = depth_layer_levels
  )
}

clean_common <- function(dat, source_sheet, default_crop = TRUE) {
  if(!"crop" %in% names(dat) && default_crop) dat$crop <- "wheat"
  if(!"date" %in% names(dat) && "stage" %in% names(dat)) dat$date <- dat$stage
  if(!"re" %in% names(dat) && "block" %in% names(dat)) dat$re <- dat$block
  if(!"treat" %in% names(dat) && "treatment" %in% names(dat)) dat$treat <- dat$treatment
  if(!"depth" %in% names(dat)) dat$depth <- NA_real_

  dat %>%
    dplyr::mutate(
      crop = as.character(crop),
      year = standardize_year(year),
      stage = standardize_stage(date),
      block = as.character(re),
      treatment = standardize_treatment(treat),
      depth = parse_num(depth),
      depth_layer = depth_label(depth),
      source_sheet = source_sheet
    ) %>%
    dplyr::filter(is.na(crop) | crop == "wheat")
}

mean_se <- function(x) {
  n_ok <- sum(!is.na(x))
  tibble::tibble(
    mean = mean(x, na.rm = TRUE),
    se = ifelse(n_ok > 1, stats::sd(x, na.rm = TRUE) / sqrt(n_ok), NA_real_),
    n = n_ok
  )
}

theme_revised <- function(base_size = 15.2) {
  ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_line(colour = "grey90", linewidth = 0.26),
      panel.grid.minor = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(fill = "grey96", colour = "grey65", linewidth = 0.45),
      strip.text = ggplot2::element_text(face = "bold", colour = "black", size = 14.2),
      axis.title = ggplot2::element_text(face = "bold", colour = "black", size = 15.6),
      axis.text = ggplot2::element_text(colour = "black", size = 13.5),
      legend.title = ggplot2::element_text(face = "bold", size = 14.3),
      legend.text = ggplot2::element_text(size = 13.2),
      legend.key = ggplot2::element_blank(),
      plot.tag = ggplot2::element_text(face = "bold", size = 23, colour = "black"),
      plot.tag.position = c(0.01, 0.99),
      plot.margin = ggplot2::margin(6, 6, 6, 6)
    )
}

empty_letters <- function(group_col = "depth_layer") {
  tibble::tibble(
    !!group_col := character(),
    treatment = factor(levels = trt_levels),
    sig = character()
  )
}

get_letters <- function(dat, value_col, group_col = "depth_layer") {
  empty <- empty_letters(group_col)
  if(nrow(dat) == 0) return(empty)
  dat %>%
    dplyr::filter(!is.na(.data[[value_col]]), !is.na(treatment), !is.na(block)) %>%
    dplyr::group_by(.data[[group_col]]) %>%
    dplyr::group_split() %>%
    purrr::map_dfr(function(tmp) {
      tmp <- droplevels(tmp)
      grp <- as.character(tmp[[group_col]][1])
      # Do not assign significance letters to incomplete 2-block contrasts.
      grid <- tidyr::expand_grid(
        treatment = factor(trt_levels, levels = trt_levels),
        block = sort(unique(tmp$block))
      )
      observed <- tmp %>% dplyr::count(treatment, block, name = "count")
      missing_cells <- grid %>% dplyr::left_join(observed, by = c("treatment", "block")) %>%
        dplyr::filter(is.na(count))
      if(nrow(tmp) < 4 || dplyr::n_distinct(tmp$treatment) != 6 ||
         dplyr::n_distinct(tmp$block) < 2 || nrow(missing_cells) > 0) {
        warning("No LSD letters: incomplete independent field-block data in ", grp, " / ", value_col, call. = FALSE)
        return(empty)
      }
      residual_df <- nrow(tmp) - (dplyr::n_distinct(tmp$treatment) + max(dplyr::n_distinct(tmp$block) - 1, 0))
      if(residual_df <= 0) return(empty)
      tryCatch({
        fit <- stats::aov(stats::reformulate(c("treatment", "block"), response = value_col), data = tmp)
        multcomp::cld(
          emmeans::emmeans(fit, ~ treatment),
          Letters = base::letters,
          adjust = multiple_comp_adjust,
          sort = TRUE,
          reversed = TRUE
        ) %>%
          as.data.frame() %>%
          dplyr::mutate(
            !!group_col := grp,
            sig = stringr::str_remove_all(.group, "\\s+")
          ) %>%
          dplyr::select(dplyr::all_of(group_col), treatment, sig)
      }, error = function(e) empty)
    })
}

warn_if_missing <- function(values, expected, label) {
  missing <- setdiff(expected, values)
  if(length(missing) > 0) {
    warning(label, " missing: ", paste(missing, collapse = ", "), call. = FALSE)
    add_note(paste(label, "missing:", paste(missing, collapse = ", ")))
  }
}

summarise_by_depth_with_letters <- function(dat, value_col) {
  letters_df <- get_letters(dat, value_col, "depth_layer")
  dat %>%
    dplyr::filter(depth_layer %in% depth_layer_levels) %>%
    dplyr::group_by(depth_layer, treatment) %>%
    dplyr::summarise(mean_se(.data[[value_col]]), .groups = "drop") %>%
    dplyr::left_join(letters_df, by = c("depth_layer", "treatment")) %>%
    dplyr::group_by(depth_layer) %>%
    dplyr::mutate(
      y_top = max(mean + se, na.rm = TRUE),
      letter_offset = dplyr::if_else(is.finite(y_top) & abs(y_top) > 0, abs(y_top) * 0.03, 0.03),
      letter_y = mean + se + letter_offset
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(depth_layer, treatment)
}

summarise_single_with_letters <- function(dat, value_col) {
  letters_df <- tibble::tibble(treatment = factor(levels = trt_levels), sig = character())
  block_table <- if(nrow(dat) > 0) table(dat$treatment, dat$block) else matrix(0, nrow=0, ncol=0)
  complete_blocks <- nrow(block_table) == 6 && ncol(block_table) >= 2 && all(block_table >= 1)
  if(complete_blocks) {
    letters_df <- tryCatch({
      fit <- stats::aov(stats::reformulate(c("treatment", "block"), response = value_col), data = dat)
      multcomp::cld(
        emmeans::emmeans(fit, ~ treatment),
        Letters = base::letters,
        adjust = multiple_comp_adjust,
        sort = TRUE,
        reversed = TRUE
      ) %>%
        as.data.frame() %>%
        dplyr::mutate(sig = stringr::str_remove_all(.group, "\\s+")) %>%
        dplyr::select(treatment, sig)
    }, error = function(e) tibble::tibble(treatment = factor(levels = trt_levels), sig = character()))
  }

  dat %>%
    dplyr::group_by(treatment) %>%
    dplyr::summarise(mean_se(.data[[value_col]]), .groups = "drop") %>%
    dplyr::left_join(letters_df, by = "treatment") %>%
    dplyr::mutate(
      y_top = max(mean + se, na.rm = TRUE),
      letter_offset = dplyr::if_else(is.finite(y_top) & abs(y_top) > 0, abs(y_top) * 0.035, 0.03),
      letter_y = mean + se + letter_offset
    ) %>%
    dplyr::arrange(treatment)
}

get_letters_for_violin <- function(dat, value_col, group_col = "depth_layer") {
  # Six management patterns are compared within each depth.
  # Repeated-year records come from the same field plots; use a mixed model
  # with year as a fixed effect and plot (block:treatment) as a random effect.
  # For one year, retain the original field-blocked ANOVA + Fisher LSD.
  # If tests fail or replication is incomplete, show NO letters (not a fake 'a').
  empty <- tibble::tibble(
    !!group_col := character(),
    treatment = factor(levels = trt_levels),
    sig = character(), letter_y = numeric()
  )
  dat <- dat %>% dplyr::filter(
    !is.na(.data[[value_col]]), !is.na(treatment), !is.na(block),
    !is.na(.data[[group_col]])
  )
  if(nrow(dat) == 0) return(empty)
  dat %>%
    dplyr::group_by(.data[[group_col]]) %>%
    dplyr::group_split() %>%
    purrr::map_dfr(function(tmp) {
      tmp <- droplevels(tmp)
      grp <- as.character(tmp[[group_col]][1])
      tab <- table(tmp$treatment, tmp$block)
      # 2 field blocks x six treatments are expected in at least one year.
      if(dplyr::n_distinct(tmp$treatment) != 6 || dplyr::n_distinct(tmp$block) < 2 ||
         any(rowSums(tab) < 2)) {
        warning("Missing field-level replication for ", grp, " / ", value_col, "; letters omitted.", call. = FALSE)
        return(empty)
      }
      actual_years <- dplyr::n_distinct(tmp$year[!is.na(tmp$year)])
      out <- tryCatch({
        if(actual_years > 1) {
          # A plot is the persistent block:treatment combination across years.
          fit <- lmerTest::lmer(
            stats::as.formula(paste0(value_col, " ~ treatment + factor(year) + (1|block:treatment)")),
            data = tmp, REML = TRUE
          )
        } else {
          fit <- stats::aov(stats::reformulate(c("treatment", "block"), response = value_col), data=tmp)
        }
        comp <- multcomp::cld(
          emmeans::emmeans(fit, ~ treatment), Letters=base::letters,
          adjust=multiple_comp_adjust, sort=TRUE, reversed=TRUE
        ) %>% as.data.frame() %>%
          dplyr::mutate(sig=stringr::str_remove_all(.group, "\\s+")) %>%
          dplyr::select(treatment, sig)
        pos <- tmp %>% dplyr::group_by(treatment) %>%
          dplyr::summarise(max_val=max(.data[[value_col]],na.rm=TRUE),.groups="drop")
        y_range <- range(tmp[[value_col]],na.rm=TRUE)
        offset <- if(diff(y_range)>0) diff(y_range)*0.06 else max(abs(y_range))*0.06
        pos %>% dplyr::left_join(comp,by="treatment") %>%
          dplyr::mutate(!!group_col := grp,letter_y=max_val+offset) %>%
          dplyr::select(dplyr::all_of(group_col),treatment,sig,letter_y)
      }, error=function(e) {
        warning("Letter model failed for ", grp, " / ",value_col, ": ", conditionMessage(e),
                ". No letters plotted.",call.=FALSE)
        empty
      })
      out
    })
}

make_pca_export <- function(por_dat) {
  pdat <- por_dat %>% tidyr::drop_na(dplyr::all_of(pca_vars))
  fit <- stats::prcomp(pdat[, pca_vars], center = TRUE, scale. = TRUE)
  scores <- dplyr::bind_cols(
    pdat %>% dplyr::select(stage, year, block, treatment, depth, depth_layer),
    as.data.frame(fit$x[, 1:2])
  )
  loadings <- as.data.frame(fit$rotation[, 1:2]) %>%
    tibble::rownames_to_column("variable")
  variance <- tibble::tibble(
    PC = paste0("PC", seq_along(fit$sdev)),
    variance = fit$sdev^2,
    proportion = summary(fit)$importance[2, ],
    cumulative = summary(fit)$importance[3, ]
  )
  list(scores = scores, loadings = loadings, variance = variance)
}

excel_ready <- function(dat) {
  dat <- as.data.frame(dat)
  dat[] <- lapply(dat, function(x) {
    if(is.factor(x)) return(as.character(x))
    x
  })
  dat
}

# -----------------------------
# 4. data cleaning
# -----------------------------
soc_base <- readxl::read_excel(file_path, sheet = "土壤基础养分") %>%
  clean_common("土壤基础养分", default_crop = TRUE) %>%
  dplyr::filter(depth %in% depth_num_levels, year %in% c(2021, 2023, 2025)) %>%
  dplyr::mutate(
    SOC_value = as.numeric(SOC),
    cc = factor(as.character(depth_layer), levels = depth_layer_levels),
    PP = treatment
  )

soc_A <- soc_base %>%
  dplyr::filter(!is.na(PP), !is.na(cc), !is.na(SOC_value)) %>%
  dplyr::group_by(year, PP, cc) %>%
  dplyr::summarise(SOC = mean(SOC_value, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(
    year = factor(year, levels = c(2021, 2023, 2025)),
    PP = factor(PP, levels = trt_levels),
    cc = factor(cc, levels = depth_layer_levels)
  )

cat("\nPanel A SOC data checks\n")
print(unique(soc_A$year))
print(table(soc_A$year))
print(table(soc_A$cc))
print(table(soc_A$year, soc_A$cc))
warn_if_missing(as.character(unique(soc_A$year)), as.character(c(2021, 2023, 2025)), "Panel A SOC years")
warn_if_missing(as.character(unique(soc_A$cc)), depth_layer_levels, "Panel A SOC depth layers")
warn_if_missing(as.character(unique(soc_A$PP)), trt_levels, "Panel A SOC treatments")

agg0 <- readxl::read_excel(file_path, sheet = "土壤团粒结构")
agg_cols <- names(agg0)
agg <- agg0 %>%
  clean_common("土壤团粒结构", default_crop = TRUE) %>%
  dplyr::mutate(
    total_weight_g = parse_num(.data[[agg_cols[6]]]),
    agg_ge_5mm_g = parse_num(.data[[agg_cols[7]]]),
    agg_3_5mm_g = parse_num(.data[[agg_cols[8]]]),
    agg_2_3mm_g = parse_num(.data[[agg_cols[9]]]),
    agg_1_2mm_g = parse_num(.data[[agg_cols[10]]]),
    agg_0_5_1mm_g = parse_num(.data[[agg_cols[11]]]),
    agg_0_25_0_5mm_g = parse_num(.data[[agg_cols[12]]]),
    agg_lt_0_25mm_g = parse_num(.data[[agg_cols[13]]]),
    MWD = (
      5 * agg_ge_5mm_g + 4 * agg_3_5mm_g + 2.5 * agg_2_3mm_g +
        1.5 * agg_1_2mm_g + 0.75 * agg_0_5_1mm_g +
        0.375 * agg_0_25_0_5mm_g + 0.125 * agg_lt_0_25mm_g
    ) / total_weight_g
  ) %>%
  dplyr::filter(depth %in% depth_num_levels)

bd <- readxl::read_excel(file_path, sheet = "土壤容重") %>%
  clean_common("土壤容重") %>%
  dplyr::rename(bulk_density = bulk_density) %>%
  dplyr::filter(stage %in% stage_levels, depth %in% depth_num_levels)

moist <- readxl::read_excel(file_path, sheet = "小麦季土壤水分") %>%
  clean_common("小麦季土壤水分") %>%
  dplyr::rename(soil_moisture = water_content) %>%
  dplyr::filter(stage %in% stage_levels, depth %in% depth_num_levels)

comp <- readxl::read_excel(file_path, sheet = "土壤紧实度") %>%
  clean_common("土壤紧实度") %>%
  dplyr::rename(compaction = `soil compaction`) %>%
  dplyr::filter(stage %in% c(stage_levels, "Before sowing"), depth <= 45) %>%
  dplyr::mutate(stage = ifelse(stage == "Before sowing", "Sowing", stage))
add_note("Before-sowing penetration resistance data were used as the sowing-stage proxy for the sowing supplement.")

por <- readxl::read_excel(file_path, sheet = "土壤持水性和孔隙度") %>%
  clean_common("土壤持水性和孔隙度") %>%
  dplyr::mutate(total_porosity = ifelse(is.na(`总孔隙度%`), `非毛管孔隙%` + `毛管孔隙%`, `总孔隙度%`)) %>%
  dplyr::transmute(
    crop, year, stage, block, treatment, depth, depth_layer,
    capillary_water_capacity = `毛管持水量/%`,
    maximum_water_capacity = `最大持水量/%`,
    minimum_water_capacity = `最小持水量/%`,
    noncapillary_porosity = `非毛管孔隙%`,
    capillary_porosity = `毛管孔隙%`,
    total_porosity
  ) %>%
  dplyr::filter(stage %in% stage_levels, depth %in% depth_num_levels)

ksat0 <- readxl::read_excel(file_path, sheet = "土壤饱和导水率")
ksat_cols <- names(ksat0)
ksat_value_col <- grep("饱和导水率|Ksat|Ks|hydraulic", ksat_cols, ignore.case = TRUE, value = TRUE)[1]
ksat_sample_col <- grep("样点|sample", ksat_cols, ignore.case = TRUE, value = TRUE)[1]
ksat_time_col <- grep("测试时间/min|测试时间|time", ksat_cols, ignore.case = TRUE, value = TRUE)[1]

ksat <- ksat0 %>%
  clean_common("土壤饱和导水率") %>%
  dplyr::mutate(
    sample_point = if(!is.na(ksat_sample_col)) as.character(.data[[ksat_sample_col]]) else "1",
    elapsed_time_min = if(!is.na(ksat_time_col)) parse_num(.data[[ksat_time_col]]) else dplyr::row_number(),
    Ks = parse_num(.data[[ksat_value_col]])
  ) %>%
  dplyr::filter(stage %in% stage_levels, depth %in% depth_num_levels, !is.na(Ks)) %>%
  dplyr::arrange(year, stage, block, treatment, depth, sample_point, elapsed_time_min) %>%
  dplyr::group_by(crop, year, stage, block, treatment, depth, depth_layer, sample_point) %>%
  dplyr::summarise(
    n_obs = dplyr::n(),
    Ks = mean(utils::tail(Ks, max(1, ceiling(dplyr::n() * 0.30))), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::group_by(crop, year, stage, block, treatment, depth, depth_layer) %>%
  dplyr::summarise(Ks = mean(Ks, na.rm = TRUE), .groups = "drop")

kunsat0 <- readxl::read_excel(file_path, sheet = "土壤非饱和导水率")
kunsat_cols <- names(kunsat0)
kunsat_value_col <- grep("非饱和导水率|Kunsat|K_unsat|unsat|hydraulic", kunsat_cols, ignore.case = TRUE, value = TRUE)[1]
kunsat_time_col <- grep("测试时间/min|测试时间|time", kunsat_cols, ignore.case = TRUE, value = TRUE)[1]

kunsat <- kunsat0 %>%
  clean_common("土壤非饱和导水率") %>%
  dplyr::mutate(
    elapsed_time_min = if(!is.na(kunsat_time_col)) parse_num(.data[[kunsat_time_col]]) else dplyr::row_number(),
    Kunsat = parse_num(.data[[kunsat_value_col]])
  ) %>%
  dplyr::filter(stage %in% stage_levels, !is.na(Kunsat)) %>%
  dplyr::arrange(year, stage, block, treatment, elapsed_time_min) %>%
  dplyr::group_by(crop, year, stage, block, treatment) %>%
  dplyr::summarise(
    n_obs = dplyr::n(),
    Kunsat = mean(utils::tail(Kunsat, max(1, ceiling(dplyr::n() * 0.30))), na.rm = TRUE),
    .groups = "drop"
  )

# Soil evaporation: calculated from the mass loss of ring + soil between
# the first and last weighing within each year x stage x block x treatment.
# Soil evaporation (mm) = mass loss (g) / exposed ring area (cm2) * 10.
evap0 <- readxl::read_excel(file_path, sheet = "土壤蒸发")
evap_cols <- names(evap0)
evap_time_col <- grep("^时间$|time", evap_cols, ignore.case = TRUE, value = TRUE)[1]
evap_weight_col <- grep("环刀\\+土|环刀.*土|重量|weight", evap_cols, ignore.case = TRUE, value = TRUE)[1]

evap <- evap0 %>%
  clean_common("土壤蒸发") %>%
  dplyr::mutate(
    measure_time = if(!is.na(evap_time_col)) parse_cn_datetime(year, .data[[evap_time_col]]) else as.POSIXct(NA_real_, origin = "1970-01-01", tz = "Asia/Shanghai"),
    ring_soil_weight_g = parse_num(.data[[evap_weight_col]])
  ) %>%
  dplyr::filter(stage %in% stage_levels, !is.na(ring_soil_weight_g), !is.na(measure_time)) %>%
  dplyr::arrange(year, stage, block, treatment, measure_time) %>%
  dplyr::group_by(crop, year, stage, block, treatment) %>%
  dplyr::summarise(
    n_obs = dplyr::n(),
    initial_time = dplyr::first(measure_time),
    final_time = dplyr::last(measure_time),
    interval_days = as.numeric(difftime(final_time, initial_time, units = "days")),
    initial_weight_g = dplyr::first(ring_soil_weight_g),
    final_weight_g = dplyr::last(ring_soil_weight_g),
    weight_loss_g = initial_weight_g - final_weight_g,
    soil_evaporation_mm = weight_loss_g / evap_ring_area_cm2 * 10,
    soil_evaporation_rate_mm_d = dplyr::if_else(interval_days > 0, soil_evaporation_mm / interval_days, NA_real_),
    .groups = "drop"
  ) %>%
  dplyr::filter(n_obs >= 2, !is.na(soil_evaporation_mm))
add_note(paste0("Soil evaporation was calculated from ring mass loss with exposed area = ", evap_ring_area_cm2, " cm2."))
if(!"depth" %in% names(kunsat0)) {
  add_note("Unsaturated hydraulic conductivity has no depth column; Panel I is drawn as a single panel.")
}

jointing_check <- dplyr::bind_rows(
  bd %>% dplyr::filter(stage == stage_target) %>% dplyr::transmute(source = "Bulk density", stage, treat = treatment, depth),
  moist %>% dplyr::filter(stage == stage_target) %>% dplyr::transmute(source = "Soil moisture", stage, treat = treatment, depth),
  comp %>% dplyr::filter(stage == stage_target) %>% dplyr::transmute(source = "Penetration resistance", stage, treat = treatment, depth),
  evap %>% dplyr::filter(stage == stage_target) %>% dplyr::transmute(source = "Soil evaporation", stage, treat = treatment, depth = NA_real_),
  ksat %>% dplyr::filter(stage == stage_target) %>% dplyr::transmute(source = "Ks", stage, treat = treatment, depth)
)

cat("\nJointing data checks\n")
print(table(jointing_check$source, jointing_check$stage))
print(table(jointing_check$source, jointing_check$treat))
print(table(jointing_check$source, jointing_check$depth))
cat("\nG/H/I summaries\n")
print(summary(evap$soil_evaporation_mm))
print(summary(ksat$Ks))
print(summary(kunsat$Kunsat))
warn_if_missing(as.character(unique(jointing_check$treat)), trt_levels, "Jointing treatments")
warn_if_missing(as.character(unique(jointing_check$depth[jointing_check$source != "Penetration resistance"])), as.character(depth_num_levels), "Jointing depths for faceted panels")

stage_check_all <- dplyr::bind_rows(
  bd %>% dplyr::transmute(source = "Bulk density", stage, treat = treatment, depth),
  moist %>% dplyr::transmute(source = "Soil moisture", stage, treat = treatment, depth),
  comp %>% dplyr::transmute(source = "Penetration resistance", stage, treat = treatment, depth),
  por %>% dplyr::transmute(source = "PCA input", stage, treat = treatment, depth),
  evap %>% dplyr::transmute(source = "Soil evaporation", stage, treat = treatment, depth = NA_real_),
  ksat %>% dplyr::transmute(source = "Ks", stage, treat = treatment, depth),
  kunsat %>% dplyr::transmute(source = "Kunsat", stage, treat = treatment, depth = NA_real_)
)

print_stage_checks <- function(stage_name) {
  tmp <- stage_check_all %>% dplyr::filter(stage == stage_name)
  cat("\n", stage_name, " data checks\n", sep = "")
  print(table(tmp$stage, useNA = "ifany"))
  print(table(tmp$treat, useNA = "ifany"))
  print(table(tmp$depth, useNA = "ifany"))
  warn_if_missing(as.character(unique(tmp$treat[!is.na(tmp$treat)])), trt_levels, paste(stage_name, "treatments"))
  warn_if_missing(as.character(unique(tmp$depth[!is.na(tmp$depth) & tmp$source != "Penetration resistance"])), as.character(depth_num_levels), paste(stage_name, "depths for faceted panels"))
}
purrr::walk(stage_levels, print_stage_checks)

cat("\nPCA input variables\n")
print(pca_vars)

cat("\nC panel jointing violin-letter checks\n")
bd_jointing_letter_check <- get_letters_for_violin(
  bd %>% dplyr::filter(stage == "Jointing"),
  "bulk_density"
)
print(bd_jointing_letter_check)

cat("\nD panel jointing violin-letter checks\n")
moist_jointing_letter_check <- get_letters_for_violin(
  moist %>% dplyr::filter(stage == "Jointing"),
  "soil_moisture"
)
print(moist_jointing_letter_check)

cat("\nG panel jointing soil evaporation mean, SE, and letter positions\n")
g_evap_jointing_letter_check <- summarise_single_with_letters(
  evap %>% dplyr::filter(stage == "Jointing"),
  "soil_evaporation_mm"
)
print(g_evap_jointing_letter_check %>% dplyr::select(treatment, mean, se, sig, letter_y, letter_offset))

cat("\nH panel jointing Ks mean, SE, and letter positions\n")
h_ks_jointing_letter_check <- summarise_by_depth_with_letters(
  ksat %>% dplyr::filter(stage == "Jointing"),
  "Ks"
)
print(h_ks_jointing_letter_check %>% dplyr::select(depth_layer, treatment, mean, se, sig, letter_y, letter_offset))

cat("\nI panel jointing Kunsat mean, SE, and letter positions\n")
i_kunsat_jointing_letter_check <- summarise_single_with_letters(
  kunsat %>% dplyr::filter(stage == "Jointing"),
  "Kunsat"
)
print(i_kunsat_jointing_letter_check %>% dplyr::select(treatment, mean, se, sig, letter_y, letter_offset))

# -----------------------------
# 5. panel A: SOC ring-bar based on 环柱状图.R
# -----------------------------
make_soc_ring_panel <- function(tag = "A") {
  df <- soc_A %>%
    dplyr::mutate(
      year_num = as.integer(as.character(year)),
      year_f = factor(year_num, levels = c(2021, 2023, 2025)),
      year_idx = as.integer(year_f),
      PP = factor(PP, levels = trt_levels),
      cc = factor(cc, levels = depth_layer_levels)
    )

  # Distinct but harmonious year colours for clearer visual separation.
  year_cols <- c(
    "2021" = "#E69F8D",  # soft coral
    "2023" = "#6FB7B0",  # muted teal
    "2025" = "#7C88C5"   # soft indigo
  )

  # A small helper for depth-specific radial axes.
  # This keeps the 10/20/40 cm panels visually balanced instead of forcing
  # the shallow-layer scale onto the much smaller 40 cm values.
  nice_radial_axis <- function(x, n_breaks = 5) {
    xmax <- max(x, na.rm = TRUE)
    if(!is.finite(xmax) || xmax <= 0) xmax <- 1
    raw_step <- xmax / n_breaks
    step <- 10 ^ floor(log10(raw_step))
    step <- step * c(1, 2, 5, 10)[which(c(1, 2, 5, 10) * step >= raw_step)[1]]
    upper <- ceiling(xmax / step) * step
    list(upper = upper, breaks = seq(0, upper, by = step))
  }

  make_one_depth <- function(depth_name, local_tag = NULL) {
    df_d <- df %>%
      dplyr::filter(cc == depth_name) %>%
      dplyr::arrange(PP, year_idx) %>%
      dplyr::mutate(
        x_group = as.integer(PP),
        x_center = x_group + c(-0.23, 0, 0.23)[year_idx],
        bar_w = 0.18,
        xmin = x_center - bar_w/2,
        xmax = x_center + bar_w/2,
        ymin = 0,
        ymax = SOC,
        col = year_cols[as.character(year_num)]
      )

    axis_info <- nice_radial_axis(df_d$SOC, n_breaks = 5)
    local_upper <- axis_info$upper
    y_breaks <- axis_info$breaks
    max_r <- local_upper * 1.20

    # Keep the angular mapping fixed so the PP labels and bars are aligned.
    # The right-side x space is retained for radial tick labels, matching the
    # previous panel structure.
    x_lim <- c(0.55, 6.65)

    # Tangential PP labels: the text angle is calculated from the same x-scale
    # used by coord_polar(), then flipped only when needed to keep text readable.
    pp_ticks <- tibble::tibble(
      PP = factor(trt_levels, levels = trt_levels),
      x = seq_along(trt_levels),
      y_lab = max_r * 0.985,
      theta = 360 * (x - x_lim[1]) / diff(x_lim),
      angle_raw = -theta,
      angle = dplyr::case_when(
        angle_raw < -90 ~ angle_raw + 180,
        angle_raw > 90 ~ angle_raw - 180,
        TRUE ~ angle_raw
      )
    )

    radial_labels <- tibble::tibble(
      x = 6.52,
      y = y_breaks,
      lab = y_breaks
    )

    ggplot2::ggplot() +
      ggplot2::geom_rect(
        data = df_d,
        ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = factor(year_num)),
        colour = "grey28",
        linewidth = 0.16
      ) +
      ggplot2::scale_fill_manual(values = year_cols, name = "Year", drop = FALSE) +
      ggplot2::geom_hline(yintercept = y_breaks, color = "grey87", linewidth = 0.24) +
      ggplot2::geom_text(
        data = pp_ticks,
        ggplot2::aes(x = x, y = y_lab, label = PP, angle = angle),
        hjust = 0.5,
        vjust = 0.5,
        size = 5.7,
        fontface = "bold",
        colour = "black"
      ) +
      ggplot2::geom_text(
        data = radial_labels,
        ggplot2::aes(x = x, y = y, label = lab),
        size = 4.95,
        color = "grey35"
      ) +
      ggplot2::coord_polar(theta = "x", start = 0, clip = "off") +
      ggplot2::scale_x_continuous(limits = x_lim, expand = c(0, 0)) +
      ggplot2::scale_y_continuous(limits = c(0, max_r), expand = c(0, 0)) +
      ggplot2::ggtitle(depth_name) +
      ggplot2::labs(tag = local_tag) +
      ggplot2::theme_void(base_size = 14.2) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", size = 13.2, hjust = 0.5, margin = ggplot2::margin(b = -1)),
        plot.margin = ggplot2::margin(1, 3, 1, 3),
        plot.tag = ggplot2::element_text(face = "bold", size = 23),
        plot.tag.position = c(0.01, 0.99),
        legend.position = "bottom",
        legend.box = "horizontal",
        legend.margin = ggplot2::margin(0, 0, 0, 0),
        legend.key.size = grid::unit(0.36, "cm")
      ) +
      ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(shape = 22, size = 4), nrow = 1))
  }

  make_one_depth("0-10 cm", tag) + make_one_depth("10-20 cm") + make_one_depth("20-40 cm") +
    patchwork::plot_layout(ncol = 3, guides = "collect") &
    ggplot2::theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.margin = ggplot2::margin(0, 0, 0, 0),
      legend.title = ggplot2::element_text(face = "bold", size = 13.8),
      legend.text = ggplot2::element_text(size = 12.8)
    )
}


# -----------------------------
# 6. other panel constructors
# -----------------------------
empty_panel <- function(tag, label = "Data not available") {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0, y = 0, label = label, size = 5.2, colour = "grey35") +
    ggplot2::labs(tag = tag) +
    ggplot2::theme_void(base_size = 12.5) +
    ggplot2::theme(
      plot.tag = ggplot2::element_text(face = "bold", size = 18.5, colour = "black"),
      plot.tag.position = c(0.01, 0.99),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA)
    )
}

bar_with_letters <- function(dat, value_col, y_lab, tag, free_y = FALSE) {
  if(nrow(dat) == 0) {
    add_note(paste("Panel", tag, "has no available data."))
    return(empty_panel(tag))
  }
  pdat <- summarise_by_depth_with_letters(dat, value_col) %>%
    dplyr::mutate(treatment = factor(treatment, levels = trt_levels))

  p <- ggplot2::ggplot(pdat, ggplot2::aes(treatment, mean)) +
    ggplot2::geom_col(fill = main_fill, colour = main_line, linewidth = 0.32, width = 0.72) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = mean - se, ymax = mean + se), width = 0.16, linewidth = 0.32) +
    ggplot2::geom_text(ggplot2::aes(y = letter_y, label = sig), size = 6.15, fontface = "plain", na.rm = TRUE) +
    ggplot2::facet_wrap(~ depth_layer, nrow = 1, scales = if(free_y) "free_y" else "fixed") +
    ggplot2::labs(x = NULL, y = y_lab, tag = tag) +
    theme_revised(13.6) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1),
      legend.position = "none"
    )
  p
}

box_panel <- function(dat, value_col, y_lab, tag, box_col = "#AFCED7", add_letters = TRUE) {
  if(nrow(dat) == 0) {
    add_note(paste("Panel", tag, "has no available data."))
    return(empty_panel(tag))
  }
  letters_df <- if(add_letters) get_letters_for_violin(dat, value_col) else NULL

  p <- ggplot2::ggplot(dat, ggplot2::aes(treatment, .data[[value_col]])) +
    ggplot2::geom_boxplot(width = 0.58, outlier.shape = NA, fill = "white", colour = box_col, linewidth = 0.82) +
    ggplot2::geom_point(position = ggplot2::position_jitter(width = 0.10, height = 0), size = 1.35, alpha = 0.58, colour = "#7A7A7A") +
    ggplot2::facet_wrap(~ depth_layer, nrow = 1) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.06, 0.14))) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(x = NULL, y = y_lab, tag = tag) +
    theme_revised(14.6) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1), panel.spacing.x = grid::unit(0.78, "lines"), legend.position = "none")
  if(add_letters && !is.null(letters_df) && nrow(letters_df) > 0) {
    p <- p + ggplot2::geom_text(data = letters_df, ggplot2::aes(x = treatment, y = letter_y, label = sig), inherit.aes = FALSE, size = 6.45, fontface = "plain", vjust = 0, na.rm = TRUE)
  }
  p
}
profile_panel <- function(dat, tag = "E") {
  if(nrow(dat) == 0) {
    add_note(paste("Panel", tag, "has no available penetration-resistance data."))
    return(empty_panel(tag))
  }
  sumdat <- dat %>%
    dplyr::group_by(treatment, depth) %>%
    dplyr::summarise(mean_se(compaction), .groups = "drop") %>%
    dplyr::mutate(treatment = factor(treatment, levels = trt_levels))

  max_depth <- max(sumdat$depth, na.rm = TRUE)
  y_breaks <- seq(0, ceiling(max_depth / 5) * 5, by = 5)

  ggplot2::ggplot(sumdat, ggplot2::aes(mean, depth, colour = treatment, group = treatment)) +
    ggplot2::geom_path(linewidth = 0.82, alpha = 0.95) +
    ggplot2::geom_point(size = 2.05, alpha = 0.95) +
    ggplot2::geom_segment(
      ggplot2::aes(x = mean - se, xend = mean + se, y = depth, yend = depth),
      linewidth = 0.34,
      alpha = 0.72
    ) +
    ggplot2::scale_colour_manual(values = treatment_cols, drop = FALSE, name = "Treatment") +
    ggplot2::scale_y_reverse(breaks = y_breaks, expand = ggplot2::expansion(mult = c(0.02, 0.03))) +
    ggplot2::scale_x_continuous(labels = scales::label_number(accuracy = 1)) +
    ggplot2::labs(x = "Penetration resistance (kPa)", y = "Depth (cm)", tag = tag) +
    theme_revised(13.7) +
    ggplot2::theme(
      legend.position = "inside",
      legend.position.inside = c(0.98, 0.98),
      legend.justification = c(1, 1),
      legend.direction = "horizontal",
      legend.key.size = grid::unit(0.42, "cm"),
      legend.margin = ggplot2::margin(1, 1, 1, 1),
      legend.background = ggplot2::element_rect(fill = scales::alpha("white", 0.82), colour = "grey82", linewidth = 0.25)
    ) +
    ggplot2::guides(colour = ggplot2::guide_legend(nrow = 2, byrow = TRUE))
}

# -----------------------------
# New panel F: measured values (not PCA score) at jointing in 2024.
# Put NCapP and original PCA into Supplementary Information.
# -----------------------------
cap_pore_panel <- function(stage_name="Jointing", tag="F", include_ncap=FALSE) {
  inds <- c(capillary_water_capacity="CapWHC (%)", capillary_porosity="CapP (%)")
  if(include_ncap) inds <- c(inds, noncapillary_porosity="NCapP (%)")
  raw <- por %>%
    dplyr::filter(stage == stage_name, !is.na(depth_layer), !is.na(treatment)) %>%
    dplyr::select(stage, year, block, treatment, depth_layer, dplyr::all_of(names(inds))) %>%
    tidyr::pivot_longer(cols = dplyr::all_of(names(inds)), names_to = "variable", values_to = "value") %>%
    dplyr::mutate(indicator = factor(inds[variable], levels = unname(inds)), depth_layer = factor(depth_layer, levels = depth_layer_levels), treatment = factor(treatment, levels = trt_levels), group_id = paste(variable, depth_layer, sep = "__")) %>%
    dplyr::filter(!is.na(value))
  if(nrow(raw) == 0) return(empty_panel(tag, "Capillary/porosity data missing"))

  summ <- raw %>%
    dplyr::group_by(indicator, depth_layer, treatment, group_id) %>%
    dplyr::summarise(mean_se(value), .groups = "drop")
  letters <- get_letters(raw, "value", group_col = "group_id")
  summ <- summ %>%
    dplyr::left_join(letters, by = c("group_id", "treatment")) %>%
    dplyr::group_by(indicator, depth_layer) %>%
    dplyr::mutate(ymin = mean - dplyr::coalesce(se, 0), ymax = mean + dplyr::coalesce(se, 0), yspan = max(ymax, na.rm = TRUE) - min(ymin, na.rm = TRUE), text_y = ymax + pmax(0.10 * yspan, 0.90)) %>%
    dplyr::ungroup()

  ggplot2::ggplot(summ, ggplot2::aes(treatment, mean)) +
    ggplot2::geom_pointrange(ggplot2::aes(ymin = ifelse(n >= 2, mean - se, mean), ymax = ifelse(n >= 2, mean + se, mean)), colour = "#4F8B9E", linewidth = 0.42, size = 1.15, fatten = 1.75, na.rm = TRUE) +
    ggplot2::geom_text(ggplot2::aes(y = text_y, label = sig), size = 4.2, vjust = 0, na.rm = TRUE) +
    ggplot2::facet_grid(indicator ~ depth_layer, scales = "free_y") +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.08, 0.24))) +
    ggplot2::labs(x = NULL, y = NULL, tag = tag) +
    theme_revised(base_size = 15.0) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5, size = 12.2), axis.text.y = ggplot2::element_text(size = 11.8), strip.text = ggplot2::element_text(size = 12.2, face = "bold"), panel.spacing = grid::unit(0.45, "lines"), legend.position = "none")
}
pca_display_data <- function(stage_name) {
  por %>%
    dplyr::filter(stage == stage_name, depth_layer %in% depth_layer_levels) %>%
    dplyr::group_by(stage, treatment, depth, depth_layer) %>%
    dplyr::summarise(
      dplyr::across(dplyr::all_of(pca_vars), ~ mean(.x, na.rm = TRUE)),
      n_raw = dplyr::n(),
      n_year = dplyr::n_distinct(year),
      n_block = dplyr::n_distinct(block),
      .groups = "drop"
    ) %>%
    tidyr::drop_na(dplyr::all_of(pca_vars))
}

pca_split_panel <- function(stage_name, tag = "F") {
  # Data are summarized at treatment + depth; values are means across years and blocks/re.
  # A separate standardized PCA is then fitted within each depth layer.
  pdat <- pca_display_data(stage_name)
  cat("\nPCA display data summarized at: treatment + depth; values are means across years and blocks/re\n")
  cat("\n", stage_name, " PCA display point counts by depth x treatment\n", sep = "")
  print(pdat %>% dplyr::count(depth_layer, treatment))
  cat("\n", stage_name, " PCA display point counts by depth\n", sep = "")
  print(pdat %>% dplyr::count(depth_layer))
  missing_pca <- pdat %>%
    tidyr::complete(depth_layer = depth_layer_levels, treatment = trt_levels) %>%
    dplyr::filter(is.na(n_raw))
  if(nrow(missing_pca) > 0) {
    warning(stage_name, " PCA treatment-depth means missing: ",
            paste(paste(missing_pca$depth_layer, missing_pca$treatment, sep = "/"), collapse = ", "),
            call. = FALSE)
  }

  if(nrow(pdat) < 6 || dplyr::n_distinct(pdat$depth_layer) < 1) {
    add_note(paste(stage_name, "PCA panel has insufficient data."))
    return(empty_panel(tag, paste(stage_name, "PCA data not available")))
  }

  make_one <- function(depth_name, local_tag = NULL) {
    ddat <- pdat %>% dplyr::filter(depth_layer == depth_name)
    if(nrow(ddat) < 4 || dplyr::n_distinct(ddat$treatment) < 2) {
      return(empty_panel(if(is.null(local_tag)) "" else local_tag, paste(depth_name, "PCA missing")) +
               ggplot2::ggtitle(depth_name))
    }
    fit <- stats::prcomp(ddat[, pca_vars], center = TRUE, scale. = TRUE)
    pct <- summary(fit)$importance[2, 1:2] * 100
    sdat <- dplyr::bind_cols(
      ddat %>% dplyr::select(stage, treatment, depth, depth_layer, n_raw, n_year, n_block),
      as.data.frame(fit$x[, 1:2])
    )
    loads <- as.data.frame(fit$rotation[, 1:2]) %>%
      tibble::rownames_to_column("variable") %>%
      dplyr::mutate(label = dplyr::recode(variable, !!!pca_labels))
    score_dx <- diff(range(sdat$PC1, na.rm = TRUE))
    score_dy <- diff(range(sdat$PC2, na.rm = TRUE))
    load_dx <- diff(range(loads$PC1, na.rm = TRUE))
    load_dy <- diff(range(loads$PC2, na.rm = TRUE))
    ratios <- c(score_dx / load_dx, score_dy / load_dy)
    ratios <- ratios[is.finite(ratios) & ratios > 0]
    scale_arrow <- if(length(ratios) > 0) min(ratios) * 0.63 else 1
    loads <- loads %>% dplyr::mutate(PC1a = PC1 * scale_arrow, PC2a = PC2 * scale_arrow)
    x_rng <- range(c(sdat$PC1, loads$PC1a), na.rm = TRUE)
    y_rng <- range(c(sdat$PC2, loads$PC2a), na.rm = TRUE)
    x_pad <- max(0.2, diff(x_rng) * 0.08)
    y_pad <- max(0.2, diff(y_rng) * 0.08)

    ggplot2::ggplot(sdat, ggplot2::aes(PC1, PC2, colour = treatment)) +
      ggplot2::geom_hline(yintercept = 0, colour = "grey84", linewidth = 0.25) +
      ggplot2::geom_vline(xintercept = 0, colour = "grey84", linewidth = 0.25) +
      ggplot2::geom_point(size = 3.05, alpha = 0.95) +
      ggplot2::geom_segment(
        data = loads,
        ggplot2::aes(x = 0, y = 0, xend = PC1a, yend = PC2a),
        inherit.aes = FALSE,
        arrow = grid::arrow(length = grid::unit(0.16, "cm")),
        colour = "grey22",
        linewidth = 0.55
      ) +
      ggrepel::geom_label_repel(
        data = loads,
        ggplot2::aes(x = PC1a, y = PC2a, label = label),
        inherit.aes = FALSE,
        size = 4,
        colour = "black",
        fill = NA,
        label.size = 0,
        label.padding = grid::unit(0.2, "lines"),
        segment.color = "red",
        segment.size = 0.18,
        box.padding = 0.16,
        point.padding = 0.05,
        min.segment.length = 0,
        max.overlaps = Inf
      ) +
      ggplot2::scale_colour_manual(values = treatment_cols, drop = FALSE, name = "Treatment") +
      ggplot2::coord_cartesian(xlim = x_rng + c(-x_pad, x_pad), ylim = y_rng + c(-y_pad, y_pad)) +
      ggplot2::labs(
        title = depth_name,
        x = paste0("PC1 (", sprintf("%.1f", pct[1]), "%)"),
        y = paste0("PC2 (", sprintf("%.1f", pct[2]), "%)"),
        tag = local_tag
      ) +
      theme_revised(13.4) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", size = 12.2, hjust = 0.5, margin = ggplot2::margin(b = 2)),
        legend.position = "bottom",
        legend.key.size = grid::unit(0.30, "cm"),
        legend.margin = ggplot2::margin(0, 0, 0, 0),
        axis.title = ggplot2::element_text(face = "bold", size = 12.5),
        axis.text = ggplot2::element_text(size = 11.0)
      ) +
      ggplot2::guides(colour = ggplot2::guide_legend(nrow = 1, byrow = TRUE))
  }

  make_one("0-10 cm", tag) + make_one("10-20 cm") + make_one("20-40 cm") +
    patchwork::plot_layout(ncol = 3, guides = "collect") &
    ggplot2::theme(legend.position = "bottom")
}

bar_single_panel <- function(dat, value_col, y_lab, tag) {
  if(nrow(dat) == 0) {
    add_note(paste("Panel", tag, "has no available data."))
    return(empty_panel(tag))
  }
  pdat <- summarise_single_with_letters(dat, value_col) %>%
    dplyr::mutate(treatment = factor(treatment, levels = trt_levels))

  ggplot2::ggplot(pdat, ggplot2::aes(treatment, mean)) +
    ggplot2::geom_col(fill = main_fill, colour = main_line, linewidth = 0.32, width = 0.72) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = mean - se, ymax = mean + se), width = 0.16, linewidth = 0.32) +
    ggplot2::geom_text(ggplot2::aes(y = letter_y, label = sig), size = 6.15, fontface = "plain", na.rm = TRUE) +
    ggplot2::labs(x = NULL, y = y_lab, tag = tag) +
    theme_revised(13.6) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1),
      legend.position = "none"
    )
}

# -----------------------------
# 7. final figure builders
# -----------------------------
stage_slice <- function(dat, stage_name) {
  dat %>% dplyr::filter(stage == stage_name)
}

make_evap_panel <- function(dat, tag = "G") {
  bar_single_panel(dat, "soil_evaporation_mm", "Soil evaporation (mm)", tag)
}

make_kunsat_panel <- function(dat, tag = "I") {
  if("depth_layer" %in% names(dat) && any(!is.na(dat$depth_layer)) &&
     dplyr::n_distinct(dat$depth_layer[!is.na(dat$depth_layer)]) > 1) {
    bar_with_letters(dat, "Kunsat", expression("SIR (cm min"^-1*")"), tag, free_y = TRUE)
  } else {
    bar_single_panel(dat, "Kunsat", expression("SIR (cm min"^-1*")"), tag)
  }
}

save_figure <- function(plot, file_prefix, width, height) {
  ggplot2::ggsave(
    filename = file.path(out_dir, paste0(file_prefix, ".png")),
    plot = plot,
    width = width,
    height = height,
    dpi = 1000,
    bg = "white"
  )
  ggplot2::ggsave(
    filename = file.path(out_dir, paste0(file_prefix, ".pdf")),
    plot = plot,
    width = width,
    height = height,
    bg = "white"
  )
}

make_main_figure <- function() {
  bd_j <- stage_slice(bd, "Jointing")
  moist_j <- stage_slice(moist, "Jointing")
  comp_j <- stage_slice(comp, "Jointing")
  evap_j <- stage_slice(evap, "Jointing")
  ksat_j <- stage_slice(ksat, "Jointing")
  kunsat_j <- stage_slice(kunsat, "Jointing")

  pA <- make_soc_ring_panel("A")
  pB <- bar_with_letters(agg, "MWD", "MWD (mm)", "B", free_y = FALSE)
  pC <- box_panel(bd_j, "bulk_density", expression("Bulk density (g cm"^-3*")"), "C", box_col = bd_col)
  pD <- box_panel(moist_j, "soil_moisture", "Soil moisture (%)", "D", box_col = moist_col)
  pE <- profile_panel(comp_j, "E")
  pF <- cap_pore_panel("Jointing", "F", include_ncap=FALSE)
  pG <- make_evap_panel(evap_j, "G")
  pH <- bar_with_letters(ksat_j, "Ks", expression("Ks (cm min"^-1*")"), "H", free_y = TRUE)
  pI <- make_kunsat_panel(kunsat_j, "I")

  pA_box <- patchwork::wrap_elements(full = pA)
  pF_box <- patchwork::wrap_elements(full = pF)

  row1 <- pA_box + pB + patchwork::plot_layout(widths = c(1.58, 1))
  row2 <- pC + pD + patchwork::plot_layout(widths = c(1.18, 1))
  row3 <- pE + patchwork::plot_spacer() + pF_box +
    patchwork::plot_layout(widths = c(1, 0.01, 1.7))
  row4 <- pG + pH + pI + patchwork::plot_layout(widths = c(1, 2, 1))
  
  (row1 / row2 / row3 / row4) +
    patchwork::plot_layout(heights = c(1.65, 1.3, 1.8, 1.3)) +
    patchwork::plot_annotation(
     # title = "Soil properties at jointing stage",
      theme = ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", size = 18.5, hjust = 0.5, margin = ggplot2::margin(b = 5))
      )
    ) &
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", colour = NA))
}

make_supp_figure <- function(stage_name, title_text) {
  pA <- box_panel(stage_slice(bd, stage_name), "bulk_density", expression("Bulk density (g cm"^-3*")"), "A", box_col = bd_col)
  pB <- box_panel(stage_slice(moist, stage_name), "soil_moisture", "Soil moisture (%)", "B", box_col = moist_col)
  pC <- profile_panel(stage_slice(comp, stage_name), "C")
  pD <- cap_pore_panel(stage_name, "D", include_ncap = FALSE)
  pD_box <- patchwork::wrap_elements(full = pD)

  row1 <- pA + pB + patchwork::plot_layout(widths = c(1, 1))
  row2 <- pC + pD_box + patchwork::plot_layout(widths = c(1, 1.70))

  panel_list <- list()
  width_list <- c()
  current_tag <- "E"

  evap_s <- stage_slice(evap, stage_name)
  if(nrow(evap_s) > 0) {
    panel_list[[length(panel_list) + 1]] <- make_evap_panel(evap_s, current_tag)
    width_list <- c(width_list, 1)
    current_tag <- LETTERS[match(current_tag, LETTERS) + 1]
  }

  ksat_s <- stage_slice(ksat, stage_name)
  if(nrow(ksat_s) > 0) {
    panel_list[[length(panel_list) + 1]] <- bar_with_letters(ksat_s, "Ks", expression("Ks (cm min"^-1*")"), current_tag, free_y = TRUE)
    width_list <- c(width_list, 2)
    current_tag <- LETTERS[match(current_tag, LETTERS) + 1]
  }

  kunsat_s <- stage_slice(kunsat, stage_name)
  if(nrow(kunsat_s) > 0) {
    panel_list[[length(panel_list) + 1]] <- make_kunsat_panel(kunsat_s, current_tag)
    width_list <- c(width_list, 1)
  }

  if(length(panel_list) == 3) {
    row3 <- panel_list[[1]] + panel_list[[2]] + panel_list[[3]] + patchwork::plot_layout(widths = width_list)
    fig <- (row1 / row2 / row3) + patchwork::plot_layout(heights = c(1, 1.75, 0.95))
  } else if(length(panel_list) == 2) {
    row3 <- panel_list[[1]] + panel_list[[2]] + patchwork::plot_layout(widths = width_list)
    fig <- (row1 / row2 / row3) + patchwork::plot_layout(heights = c(1, 1.75, 0.95))
  } else if(length(panel_list) == 1) {
    row3 <- panel_list[[1]]
    fig <- (row1 / row2 / row3) + patchwork::plot_layout(heights = c(1, 1.75, 0.90))
  } else {
    fig <- (row1 / row2) + patchwork::plot_layout(heights = c(1, 1.75))
  }

  fig + patchwork::plot_annotation(title = title_text, theme = ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 17.5, hjust = 0.5, margin = ggplot2::margin(b = 5)))) &
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", colour = NA))
}
pca_raw_long <- por %>%
  tidyr::pivot_longer(
    cols = dplyr::all_of(pca_vars),
    names_to = "indicator",
    values_to = "value"
  ) %>%
  dplyr::mutate(
    indicator = dplyr::recode(indicator, !!!pca_labels),
    stage = factor(stage, levels = stage_levels),
    treatment = factor(treatment, levels = trt_levels),
    depth_layer = factor(depth_layer, levels = depth_layer_levels)
  )

pca_raw_summary <- pca_raw_long %>%
  dplyr::group_by(indicator, stage, depth_layer, treatment) %>%
  dplyr::summarise(mean_se(value), .groups = "drop")

pca_raw_fig <- ggplot2::ggplot(
  pca_raw_summary,
  ggplot2::aes(treatment, mean, colour = depth_layer, group = depth_layer)
) +
  ggplot2::geom_pointrange(
    ggplot2::aes(ymin = mean - se, ymax = mean + se),
    position = ggplot2::position_dodge(width = 0.55),
    linewidth = 0.34,
    size = 0.72,
    na.rm = TRUE
  ) +
  ggplot2::facet_grid(indicator ~ stage, scales = "free_y") +
  ggplot2::scale_colour_manual(values = depth_cols, drop = FALSE, name = "Depth") +
  ggplot2::labs(x = NULL, y = "Raw indicator value") +
  theme_revised(13.4) +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.position = "top",
    panel.spacing.y = grid::unit(0.35, "lines")
  )

# -----------------------------
# 8. build and export figures
# -----------------------------
figure_main_final <- make_main_figure()
figure_sowing <- make_supp_figure("Sowing", "Soil properties at sowing stage")
figure_anthesis <- make_supp_figure("Anthesis", "Soil properties at anthesis stage")
figure_jointing_porosity <- cap_pore_panel("Jointing", "A", include_ncap=TRUE)
figure_jointing_pca <- pca_split_panel("Jointing", "A")

save_figure(figure_main_final, "Figure1_STR_soil_physical_hydraulic", width = 15, height = 16)
save_figure(figure_jointing_porosity, "Supplementary_jointing_all_porosity", width=14,height=8)
save_figure(figure_jointing_pca, "Supplementary_jointing_original_PCA", width=12,height=4.8)
save_figure(figure_sowing, "Figure_S_soil_properties_sowing_final6_nature", width = 13, height = 13)
save_figure(figure_anthesis, "Figure_S_soil_properties_anthesis_final6_nature", width = 13, height = 13)

pca_export <- make_pca_export(por)
numeric_summary <- function(x, name) {
  q <- stats::quantile(x, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
  tibble::tibble(
    variable = name,
    n = sum(!is.na(x)),
    min = unname(q[1]),
    q1 = unname(q[2]),
    median = unname(q[3]),
    mean = mean(x, na.rm = TRUE),
    q3 = unname(q[4]),
    max = unname(q[5])
  )
}
table_export <- function(tab, section) {
  as.data.frame(tab) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(section = section, .before = 1)
}

checks_export <- dplyr::bind_rows(
  table_export(table(soc_A$year), "A_SOC_year"),
  table_export(table(soc_A$cc), "A_SOC_depth"),
  table_export(table(soc_A$year, soc_A$cc), "A_SOC_year_depth"),
  table_export(table(stage_check_all$source, stage_check_all$stage), "source_stage"),
  table_export(table(stage_check_all$source, stage_check_all$treat), "source_treat"),
  table_export(table(stage_check_all$source, stage_check_all$depth), "source_depth")
)

export_sheets <- list(
  A_SOC = soc_A,
  B_MWD_raw = agg,
  B_MWD_summary = summarise_by_depth_with_letters(agg, "MWD"),
  C_BD_raw = bd,
  C_BD_letters_jointing = bd_jointing_letter_check,
  C_BD_summary = bd %>%
    dplyr::group_by(stage, depth_layer, treatment) %>%
    dplyr::summarise(mean_se(bulk_density), .groups = "drop") %>%
    dplyr::arrange(stage, depth_layer, treatment),
  D_Moist_raw = moist,
  D_Moist_letters_jointing = moist_jointing_letter_check,
  D_Moist_summary = moist %>%
    dplyr::group_by(stage, depth_layer, treatment) %>%
    dplyr::summarise(mean_se(soil_moisture), .groups = "drop") %>%
    dplyr::arrange(stage, depth_layer, treatment),
  E_PR_raw = comp,
  E_PR_summary = comp %>%
    dplyr::group_by(stage, treatment, depth) %>%
    dplyr::summarise(mean_se(compaction), .groups = "drop") %>%
    dplyr::arrange(stage, treatment, depth),
  F_Porosity_raw = por,
  F_main_CapWHC_jointing = summarise_by_depth_with_letters(
    por %>% dplyr::filter(stage=="Jointing"),"capillary_water_capacity"),
  F_main_CapP_jointing = summarise_by_depth_with_letters(
    por %>% dplyr::filter(stage=="Jointing"),"capillary_porosity"),
  F_supp_NCapP_jointing = summarise_by_depth_with_letters(
    por %>% dplyr::filter(stage=="Jointing"),"noncapillary_porosity"),
  F_PCA_display_jointing = pca_display_data("Jointing"),
  F_PCA_display_sowing = pca_display_data("Sowing"),
  F_PCA_display_anthesis = pca_display_data("Anthesis"),
  F_PCA_scores = pca_export$scores,
  F_PCA_loadings = pca_export$loadings,
  F_PCA_variance = pca_export$variance,
  G_Evap_raw = evap,
  G_Evap_jointing_letters = g_evap_jointing_letter_check,
  G_Evap_summary = evap %>%
    dplyr::group_by(stage, treatment) %>%
    dplyr::summarise(mean_se(soil_evaporation_mm), .groups = "drop") %>%
    dplyr::arrange(stage, treatment),
  H_Ks_raw = ksat,
  H_Ks_jointing_letters = h_ks_jointing_letter_check,
  H_Ks_summary = ksat %>%
    dplyr::group_by(stage, depth_layer, treatment) %>%
    dplyr::summarise(mean_se(Ks), .groups = "drop") %>%
    dplyr::arrange(stage, depth_layer, treatment),
  I_Kunsat_raw = kunsat,
  I_Kunsat_jointing_letters = i_kunsat_jointing_letter_check,
  I_Kunsat_summary = kunsat %>%
    dplyr::group_by(stage, treatment) %>%
    dplyr::summarise(mean_se(Kunsat), .groups = "drop") %>%
    dplyr::arrange(stage, treatment),
  PCA_raw_indicator_summary = pca_raw_summary,
  Console_checks = checks_export,
  GHI_numeric_summary = dplyr::bind_rows(
    numeric_summary(evap$soil_evaporation_mm, "Soil evaporation"),
    numeric_summary(evap$soil_evaporation_rate_mm_d, "Soil evaporation rate"),
    numeric_summary(ksat$Ks, "Ks"),
    numeric_summary(kunsat$Kunsat, "Kunsat")
  )
)

integrated_workbook <- file.path(out_dir, "Figure1_STR_source_data.xlsx")
openxlsx::write.xlsx(
  x = lapply(export_sheets, excel_ready),
  file = integrated_workbook,
  overwrite = TRUE
)
add_note(paste("Integrated data workbook written to:", integrated_workbook))

writeLines(
  c(
    "Final4 Nature-polished soil-properties figure notes",
    "==================================",
    revised_notes,
    "",
    "Main Figure 1 retains panels A-E and G-I from old Figure 2; F now plots measured CapWHC/CapP.",
    "Panel A uses SOC years 2021, 2023, and 2025.",
    "Panel A was split into three compact ring-bar subplots for 10 cm, 20 cm, and 40 cm.",
    "Original jointing-stage PCA is now a supplementary figure; complete CapWHC/CapP/NCapP measured values are plotted in a separate supplementary figure.",
    "The main figure does not use a global bottom legend; legends are local to panels A and E.",
    "The sowing and anthesis supplementary figures remove SOC and MWD and relabel panels from A to F.",
    "Panel C/D multi-year comparisons now use treatment + fixed year + random field-plot term; do not interpret repeated years as independent plots.",
    "Panel G shows soil evaporation calculated from mass loss; panels H and I show Ks and Kunsat. All use mean +/- SE bar plots and LSD letters positioned as mean + SE + within-panel offset. No log10 version was exported.",
    "Old sowing and anthesis supplementary figures are retained. Do not count duplicated supplementary panels twice.",
    "NO automatic letter a is supplied when a statistical model fails or field-block replication is incomplete.",
    "Field infiltration is a descriptive rename of the old Kunsat column. Confirm measurement conditions in Methods before final publication."
  ),
  con = file.path(out_dir, "Figure1_STR_notes.txt"),
  useBytes = TRUE
)

message("Final4 Nature-polished output checklist:")
message("1. Panel A uses SOC years 2021, 2023, and 2025 only.")
message("2. Panels C and D include treatment comparison letters within each depth layer.")
message("3. New Panel F displays CapWHC/CapP measured values. Original PCA is supplementary.")
message("4. Multiple comparisons use LSD, implemented as emmeans CLD with adjust = 'none'.")
message("5. Panel G shows soil evaporation; H is Ks; I is field infiltration rate (old Kunsat column).")
message("6. Supplementary sowing and anthesis figures are labeled from A to F/G depending on whether soil evaporation data are available and exclude SOC and MWD.")
message("7. Revised Figure 1 and supplementary measured indicators/PCA written to: ", out_dir)


