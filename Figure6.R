# ============================================================================
# Soil & Tillage Research resubmission | Figure 6 (original Figure 1 panels A-C)
# A: Six-year wheat/rice grain yield (mirror bars)
# B: Six-year seasonal nitrogen-use-efficiency indicators (AEN, REN, PEN)
# C: Seasonal yield–NUE exploratory correlations (scatter)
#
# IMPORTANT
#   Data: current finalized yield and NUE workbook, with the SAME original file_path.
#   Replace workbook at file_path with the latest version if needed.
#   Comparisons are within each year and crop, with block included in ANOVA.
#   NUE computed against PP1 is a common zero-N reference with DIFFERENT
#   management practices; it is not a pure isolated fertilizer response.
#   Yield–AEN correlations are partly mathematical because AEN contains yield.
#   Do NOT interpret correlation significance as causal evidence.
# ============================================================================

required_pkgs <- c("readxl", "dplyr", "tidyr", "ggplot2", "agricolae", "stringr", 
                   "patchwork", "scales", "grid", "writexl", "tibble", "ggrepel")
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if(length(missing_pkgs)) stop(
  "Please install required packages: ", paste(missing_pkgs, collapse = ", "),
  "\nExample: install.packages(c(", paste(sprintf('"%s"', missing_pkgs), collapse = ", "), "))"
)
suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr); library(ggplot2)
  library(stringr); library(patchwork); library(scales); library(grid)
  library(writexl); library(ggrepel)
})

# Original Figure1(2).R paths: unchanged.
file_path <- "F:/Article/SCI/2025/Planting pattern/figure/3.1/yield and NUE.xlsx"
out_dir   <- "F:/Article/figures/fig6"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
if(!file.exists(file_path)) stop("The original file_path does not exist: ", file_path)

n_conc_unit <- "g_kg"
trt_all <- paste0("PP", 1:6)
trt_nue <- paste0("PP", 2:6)
crop_cols <- c("Rice" = "#D7B36A", "Wheat" = "#88A9C4")
shape_vals <- c("PP2"=21, "PP3"=22, "PP4"=23, "PP5"=24, "PP6"=25)
nue_indicator_labels <- c("AEN"="AEN (kg kg⁻¹)", "REN"="REN (%)", "PEN"="PEN (kg kg⁻¹)")

# 2. Helper functions
# -----------------------------
calc_n_uptake <- function(yield_kg_ha, n_conc, unit = "g_kg") {
  if(unit == "g_kg") {
    return(yield_kg_ha * n_conc / 1000)
  } else if(unit == "percent") {
    return(yield_kg_ha * n_conc / 100)
  } else {
    stop("n_conc_unit must be either 'g_kg' or 'percent'.")
  }
}

# 季节数据：逐年、逐季节显著性字母（高值 = a / A）
get_sig_letters_by_year_metric <- function(dat, response_var) {
  res_list <- list()
  year_levels   <- levels(dat$year)
  season_levels <- levels(dat$season)
  
  for(y in year_levels) {
    for(s in season_levels) {
      tmp <- dat %>% dplyr::filter(year == y, season == s)
      tmp <- tmp[!is.na(tmp[[response_var]]), ]
      
      if(nrow(tmp) == 0) next
      if(dplyr::n_distinct(tmp$treatment) < 2) next
      
      fit <- aov(
        stats::reformulate(c("block", "treatment"), response = response_var),
        data = tmp
      )
      
      lsd_out <- agricolae::LSD.test(
        fit,
        "treatment",
        p.adj = "none",
        group = TRUE
      )
      
      cld_out <- lsd_out$groups %>%
        tibble::rownames_to_column("treatment") %>%
        dplyr::rename(sig = groups) %>%
        dplyr::mutate(
          year = y,
          season = s
        ) %>%
        dplyr::select(year, treatment, season, sig)
      
      res_list[[paste(y, s, sep = "_")]] <- cld_out
    }
  }
  
  dplyr::bind_rows(res_list)
}
# 纵向镜像图布局（A 图）
make_vertical_layout <- function(year_levels, trt_levels, gap = 1) {
  n_trt  <- length(trt_levels)
  n_year <- length(year_levels)
  
  layout_df <- do.call(
    rbind,
    lapply(seq_along(year_levels), function(i) {
      yr <- year_levels[i]
      data.frame(
        year      = yr,
        treatment = trt_levels,
        trt_id    = seq_along(trt_levels),
        stringsAsFactors = FALSE
      ) %>%
        dplyr::mutate(
          year_id = i,
          y = (n_year - year_id) * (n_trt + gap) + (n_trt - trt_id + 1)
        )
    })
  ) %>%
    dplyr::mutate(
      year      = factor(year, levels = year_levels),
      treatment = factor(treatment, levels = trt_levels)
    )
  
  year_band <- layout_df %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(
      ymin = min(y) - 0.5,
      ymax = max(y) + 0.5,
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      fill_col = rep(c("grey97", "grey92"), length.out = dplyr::n())
    )
  
  layout_df <- layout_df %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(
      y_label = ifelse(
        treatment == trt_levels[1],
        paste0(as.character(year), "   ", as.character(treatment)),
        paste0("       ", as.character(treatment))
      )
    ) %>%
    dplyr::ungroup()
  
  list(layout_df = layout_df, year_band = year_band)
}

# 旋转后的季节 NUE 镜像柱图（Panel B）
make_nue_mirror_plot_rotated_final <- function(
    dat,
    response_var,
    ylab_text,
    show_x_text = FALSE,
    show_year_text = FALSE,
    show_crop_text = FALSE
) {
  sum_dat <- dat %>%
    dplyr::group_by(year, treatment, season) %>%
    dplyr::summarise(
      mean = mean(.data[[response_var]], na.rm = TRUE),
      se   = sd(.data[[response_var]], na.rm = TRUE) /
        sqrt(sum(!is.na(.data[[response_var]]))),
      .groups = "drop"
    )
  
  sig_letters <- get_sig_letters_by_year_metric(dat, response_var) %>%
    dplyr::mutate(
      year      = factor(year, levels = levels(dat$year)),
      treatment = factor(treatment, levels = levels(dat$treatment)),
      season    = factor(season, levels = c("wheat", "rice"))
    )
  
  year_levels <- levels(dat$year)
  trt_levels  <- levels(dat$treatment)
  gap   <- 0.8
  n_trt <- length(trt_levels)
  
  layout_df <- do.call(
    rbind,
    lapply(seq_along(year_levels), function(i) {
      yr <- year_levels[i]
      data.frame(
        year      = yr,
        treatment = trt_levels,
        trt_id    = seq_along(trt_levels),
        stringsAsFactors = FALSE
      ) %>%
        dplyr::mutate(
          x = (i - 1) * (n_trt + gap) + trt_id
        )
    })
  ) %>%
    dplyr::mutate(
      year      = factor(year, levels = year_levels),
      treatment = factor(treatment, levels = trt_levels)
    )
  
  year_band <- layout_df %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(
      xmin = min(x) - 0.5,
      xmax = max(x) + 0.5,
      xmid = mean(x),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      fill_col = rep(c("grey97", "grey92"), length.out = dplyr::n())
    )
  
  plot_dat <- sum_dat %>%
    dplyr::left_join(sig_letters, by = c("year", "treatment", "season")) %>%
    dplyr::left_join(layout_df, by = c("year", "treatment")) %>%
    dplyr::mutate(
      crop_lab = dplyr::recode(as.character(season), wheat = "Wheat", rice = "Rice"),
      bar_ymin = ifelse(season == "wheat", -mean, 0),
      bar_ymax = ifelse(season == "wheat", 0, mean),
      err_ymin = ifelse(season == "wheat", -(mean + se), mean - se),
      err_ymax = ifelse(season == "wheat", -(mean - se), mean + se),
      bar_xmin = x - 0.30,
      bar_xmax = x + 0.30
    )
  
  y_max  <- max(abs(c(plot_dat$err_ymin, plot_dat$err_ymax)), na.rm = TRUE)
  offset <- y_max * 0.04
  
  plot_dat <- plot_dat %>%
    dplyr::mutate(
      sig_y = ifelse(season == "wheat", err_ymin - offset, err_ymax + offset)
    )
  
  y_lim <- max(abs(c(plot_dat$err_ymin, plot_dat$err_ymax, plot_dat$sig_y)), na.rm = TRUE) * 1.08
  
  x_breaks <- layout_df$x
  x_labels <- if(show_x_text) as.character(layout_df$treatment) else rep("", nrow(layout_df))
  
  p <- ggplot() +
    geom_rect(
      data = year_band,
      aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
      fill = year_band$fill_col,
      colour = NA
    ) +
    geom_vline(
      xintercept = year_band$xmax[-nrow(year_band)] + 0.5,
      linewidth = 0.30,
      colour = "grey65"
    ) +
    geom_hline(
      yintercept = 0,
      linewidth = 0.45,
      colour = "black"
    ) +
    geom_rect(
      data = plot_dat,
      aes(
        xmin = bar_xmin, xmax = bar_xmax,
        ymin = bar_ymin, ymax = bar_ymax,
        fill = crop_lab
      ),
      colour = "black",
      linewidth = 0.28
    ) +
    geom_segment(
      data = plot_dat,
      aes(x = x, xend = x, y = err_ymin, yend = err_ymax),
      linewidth = 0.28
    ) +
    geom_segment(
      data = plot_dat,
      aes(x = x - 0.08, xend = x + 0.08, y = err_ymin, yend = err_ymin),
      linewidth = 0.28
    ) +
    geom_segment(
      data = plot_dat,
      aes(x = x - 0.08, xend = x + 0.08, y = err_ymax, yend = err_ymax),
      linewidth = 0.28
    ) +
    geom_text(
      data = plot_dat %>% dplyr::filter(season == "wheat"),
      aes(x = x, y = sig_y, label = sig),
      vjust = 1,
      size = 3.5
    ) +
    geom_text(
      data = plot_dat %>% dplyr::filter(season == "rice"),
      aes(x = x, y = sig_y, label = sig),
      vjust = 0,
      size = 3.5
    ) +
    scale_x_continuous(
      breaks = x_breaks,
      labels = x_labels,
      expand = expansion(mult = c(0.01, 0.01))
    ) +
    scale_y_continuous(
      limits = c(-y_lim, y_lim),
      labels = function(x) abs(round(x, 1)),
      breaks = pretty(c(-y_lim, y_lim), n = 5),
      expand = expansion(mult = c(0.01, 0.01))
    ) +
    scale_fill_manual(values = crop_cols) +
    labs(x = NULL, y = ylab_text, fill = NULL) +
    coord_cartesian(clip = "off") +
    theme_classic(base_size = 10.5) +
    theme(
      legend.position = "none",
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 9.5, face = "bold"),
      axis.text.x = if(show_x_text) element_text(size = 6.8, angle = 90, vjust = 0.5, hjust = 1) else element_blank(),
      axis.text.y = element_text(size = 7.8),
      axis.ticks.x = if(show_x_text) element_line(linewidth = 0.25) else element_blank(),
      axis.line.x  = element_line(linewidth = 0.35),
      axis.line.y  = element_line(linewidth = 0.35),
      axis.ticks.y = element_line(linewidth = 0.25),
      plot.margin = if(show_year_text) margin(2, 4, 16, 4) else margin(2, 4, 2, 4)
    )
  
  if(show_year_text) {
    for(i in seq_len(nrow(year_band))) {
      p <- p + annotate(
        "text",
        x = year_band$xmid[i],
        y = -y_lim * 1.07,
        label = as.character(year_band$year[i]),
        size = 2.7
      )
    }
  }
  
  if(show_crop_text) {
    p <- p +
      annotate(
        "text",
        x = mean(range(layout_df$x)),
        y = y_lim * 0.90,
        label = "Rice",
        colour = "#9A762E",
        size = 3.0,
        fontface = "bold"
      ) +
      annotate(
        "text",
        x = mean(range(layout_df$x)),
        y = -y_lim * 0.90,
        label = "Wheat",
        colour = "#4F738F",
        size = 3.0,
        fontface = "bold"
      )
  }
  
  return(p)
}

# Panel B 年份标签位置
make_nue_year_labels <- function(dat, trt_levels = trt_nue, gap = 0.8) {
  year_levels <- levels(dat$year)
  n_trt <- length(trt_levels)
  
  layout_df <- do.call(
    rbind,
    lapply(seq_along(year_levels), function(i) {
      yr <- year_levels[i]
      data.frame(
        year = yr,
        treatment = trt_levels,
        trt_id = seq_along(trt_levels),
        stringsAsFactors = FALSE
      ) %>%
        dplyr::mutate(
          x = (i - 1) * (n_trt + gap) + trt_id
        )
    })
  )
  
  layout_df %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(xmid = mean(x), .groups = "drop")
}

# -----------------------------
# Preliminary quality checks: independent plot IDs and shared zero-N controls.
expected_cols <- c("Year", "Season", "Crop", "Treatment", "Block", "Yield_kg_ha")
raw_check <- readxl::read_excel(file_path, sheet="yield_components", n_max=0)
if(!all(expected_cols %in% names(raw_check))) {
  stop("yield_components missing columns: ",
       paste(setdiff(expected_cols, names(raw_check)), collapse=", "))
}
expected_n_cols <- c("Year", "Season", "Treatment", "Block", "N_rate_kg_ha",
                     "Grain_yield_kg_ha", "Straw_biomass_kg_ha", "Grain_N_g_kg", "Straw_N_g_kg")
plant_check <- readxl::read_excel(file_path, sheet="plant_N", n_max=0)
if(!all(expected_n_cols %in% names(plant_check))) {
  stop("plant_N missing columns: ",
       paste(setdiff(expected_n_cols, names(plant_check)), collapse=", "))
}

# 3. Read data
# -----------------------------
yield_dat <- readxl::read_excel(file_path, sheet = "yield_components") %>%
  dplyr::rename(
    year              = Year,
    season            = Season,
    crop              = Crop,
    treatment         = Treatment,
    block             = Block,
    yield_kg_ha       = Yield_kg_ha,
    biomass_kg_ha     = Biomass_kg_ha,
    spike_m2          = Panicle_or_Spike_m2,
    grain_number      = Grain_number,
    tkw_g             = TKW_g
  ) %>%
  dplyr::mutate(
    year      = factor(year, levels = sort(unique(year))),
    season    = factor(season, levels = c("wheat", "rice")),
    crop      = factor(crop),
    treatment = factor(treatment, levels = trt_all),
    block     = factor(block)
  )

plant_dat <- readxl::read_excel(file_path, sheet = "plant_N") %>%
  dplyr::rename(
    year                 = Year,
    season               = Season,
    crop                 = Crop,
    treatment            = Treatment,
    block                = Block,
    n_rate_kg_ha         = N_rate_kg_ha,
    grain_yield_kg_ha    = Grain_yield_kg_ha,
    straw_biomass_kg_ha  = Straw_biomass_kg_ha,
    grain_n_conc         = Grain_N_g_kg,
    straw_n_conc         = Straw_N_g_kg,
    total_biomass_kg_ha  = Total_biomass_kg_ha,
    total_n_uptake_raw   = Total_N_uptake_kg_ha
  ) %>%
  dplyr::mutate(
    year      = factor(year, levels = sort(unique(year))),
    season    = factor(season, levels = c("wheat", "rice")),
    crop      = factor(crop),
    treatment = factor(treatment, levels = trt_all),
    block     = factor(block),
    
    grain_n_uptake_kg_ha = calc_n_uptake(grain_yield_kg_ha, grain_n_conc, n_conc_unit),
    straw_n_uptake_kg_ha = calc_n_uptake(straw_biomass_kg_ha, straw_n_conc, n_conc_unit),
    
    total_n_uptake_kg_ha = ifelse(
      is.na(total_n_uptake_raw),
      grain_n_uptake_kg_ha + straw_n_uptake_kg_ha,
      total_n_uptake_raw
    ),
    
    NHI = grain_n_uptake_kg_ha / total_n_uptake_kg_ha * 100
  )

# -----------------------------
# Confirm no row duplication and complete PP1 block-year controls.
key_cols <- c("year", "season", "treatment", "block")
if(any(duplicated(as.data.frame(yield_dat[key_cols])))) {
  stop("yield_components contains repeated year/season/treatment/block records.")
}
if(any(duplicated(as.data.frame(plant_dat[key_cols])))) {
  stop("plant_N contains repeated year/season/treatment/block records.")
}
if(any(!is.finite(yield_dat$yield_kg_ha)) || any(!is.finite(plant_dat$total_n_uptake_kg_ha))) {
  stop("Non-finite yield or calculated N uptake values. Check original workbook.")
}
controls <- plant_dat %>% dplyr::filter(treatment == "PP1") %>%
  dplyr::count(year,season,block)
if(nrow(controls)!=length(levels(plant_dat$year))*length(levels(plant_dat$season))*length(levels(plant_dat$block)) ||
   any(controls$n!=1)) stop("Missing or duplicate PP1 controls by year × season × block.")
if(any(plant_dat$n_rate_kg_ha[as.character(plant_dat$treatment)=="PP1"]!=0,na.rm=TRUE))
  stop("PP1 must be the zero-N reference for the defined NUE indicators.")
cat("Validated input records: yield=",nrow(yield_dat),
    "; plant_N=",nrow(plant_dat),"\n")

# 4. Panel A: grain yield mirror plot
# -----------------------------
yield_crop <- yield_dat %>%
  dplyr::filter(season %in% c("wheat", "rice")) %>%
  dplyr::group_by(year, treatment, block, season) %>%
  dplyr::summarise(yield = mean(yield_kg_ha, na.rm = TRUE), .groups = "drop")

yield_sum <- yield_crop %>%
  dplyr::group_by(year, treatment, season) %>%
  dplyr::summarise(
    mean = mean(yield, na.rm = TRUE),
    se   = sd(yield, na.rm = TRUE) / sqrt(dplyr::n()),
    .groups = "drop"
  )

yield_letters <- get_sig_letters_by_year_metric(yield_crop, "yield") %>%
  dplyr::mutate(
    year      = factor(year, levels = levels(yield_crop$year)),
    treatment = factor(treatment, levels = trt_all),
    season    = factor(season, levels = c("wheat", "rice"))
  )

layout_A   <- make_vertical_layout(levels(yield_sum$year), trt_all, gap = 1)
layout_dfA <- layout_A$layout_df
year_bandA <- layout_A$year_band

plot_A_dat <- yield_sum %>%
  dplyr::left_join(yield_letters, by = c("year", "treatment", "season")) %>%
  dplyr::left_join(layout_dfA %>% dplyr::select(year, treatment, y, y_label),
                   by = c("year", "treatment")) %>%
  dplyr::mutate(
    crop_lab = dplyr::recode(as.character(season), wheat = "Wheat", rice = "Rice"),
    bar_xmin = ifelse(season == "wheat", -mean, 0),
    bar_xmax = ifelse(season == "wheat", 0, mean),
    err_xmin = ifelse(season == "wheat", -(mean + se), mean - se),
    err_xmax = ifelse(season == "wheat", -(mean - se), mean + se),
    bar_ymin = y - 0.36,
    bar_ymax = y + 0.36
  )

x_max_A <- max(c(abs(plot_A_dat$err_xmin), abs(plot_A_dat$err_xmax)), na.rm = TRUE)
offsetA <- x_max_A * 0.030

plot_A_dat <- plot_A_dat %>%
  dplyr::mutate(sig_x = ifelse(season == "wheat", err_xmin - offsetA, err_xmax + offsetA))

x_lim_A <- max(abs(c(plot_A_dat$err_xmin, plot_A_dat$err_xmax, plot_A_dat$sig_x)), na.rm = TRUE) * 1.10

pA_final <- ggplot() +
  geom_rect(
    data = year_bandA,
    aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax),
    fill = year_bandA$fill_col,
    colour = NA
  ) +
  geom_hline(yintercept = year_bandA$ymin[-1], linewidth = 0.35, colour = "grey65") +
  geom_vline(xintercept = 0, linewidth = 0.55, colour = "black") +
  geom_rect(
    data = plot_A_dat,
    aes(xmin = bar_xmin, xmax = bar_xmax, ymin = bar_ymin, ymax = bar_ymax, fill = crop_lab),
    colour = "black",
    linewidth = 0.32
  ) +
  geom_segment(data = plot_A_dat, aes(x = err_xmin, xend = err_xmax, y = y, yend = y), linewidth = 0.32) +
  geom_segment(data = plot_A_dat, aes(x = err_xmin, xend = err_xmin, y = y - 0.12, yend = y + 0.12), linewidth = 0.32) +
  geom_segment(data = plot_A_dat, aes(x = err_xmax, xend = err_xmax, y = y - 0.12, yend = y + 0.12), linewidth = 0.32) +
  geom_text(
    data = plot_A_dat %>% dplyr::filter(season == "wheat"),
    aes(x = sig_x, y = y, label = sig),
    hjust = 1,
    size = 4.1,
    colour = "black"
  ) +
  geom_text(
    data = plot_A_dat %>% dplyr::filter(season == "rice"),
    aes(x = sig_x, y = y, label = sig),
    hjust = 0,
    size = 4.1,
    colour = "black"
  ) +
  annotate(
    "text",
    x = -x_lim_A * 0.55,
    y = max(layout_dfA$y) + 1.2,
    label = "Wheat",
    fontface = "bold",
    size = 3.8,
    colour = "#4F738F"
  ) +
  annotate(
    "text",
    x = x_lim_A * 0.55,
    y = max(layout_dfA$y) + 1.2,
    label = "Rice",
    fontface = "bold",
    size = 3.8,
    colour = "#9A762E"
  ) +
  scale_fill_manual(values = crop_cols) +
  scale_x_continuous(
    limits = c(-x_lim_A, x_lim_A),
    labels = function(x) abs(x),
    breaks = pretty(c(-x_lim_A, x_lim_A), n = 5),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_y_continuous(
    breaks = layout_dfA$y,
    labels = layout_dfA$y_label,
    expand = expansion(mult = c(0.01, 0.05))
  ) +
  labs(x = expression("Grain yield (kg ha"^-1*")"), y = NULL, fill = NULL) +
  theme_classic(base_size = 13) +
  theme(
    legend.position = "top",
    legend.justification = "center",
    legend.key.size = unit(0.38, "cm"),
    legend.text = element_text(size = 9.5),
    axis.title.x = element_text(size = 11, face = "bold"),
    axis.text.x  = element_text(size = 9),
    axis.text.y  = element_text(size = 8.5, family = "mono"),
    axis.line.y  = element_blank(),
    axis.ticks.y = element_blank(),
    plot.margin  = margin(4, 4, 4, 4)
  )

# -----------------------------
# 5. Seasonal NUE calculation and Panel B
# -----------------------------
control_dat <- plant_dat %>%
  dplyr::filter(treatment == "PP1") %>%
  dplyr::select(year, season, crop, block, yield_N0 = grain_yield_kg_ha, uptake_N0 = total_n_uptake_kg_ha)

nue_season <- plant_dat %>%
  dplyr::left_join(control_dat, by = c("year", "season", "crop", "block")) %>%
  dplyr::mutate(
    PFPN = ifelse(n_rate_kg_ha > 0, grain_yield_kg_ha / n_rate_kg_ha, NA_real_),
    AEN  = ifelse(n_rate_kg_ha > 0, (grain_yield_kg_ha - yield_N0) / n_rate_kg_ha, NA_real_),
    REN  = ifelse(n_rate_kg_ha > 0, (total_n_uptake_kg_ha - uptake_N0) / n_rate_kg_ha * 100, NA_real_),
    PEN  = ifelse((total_n_uptake_kg_ha - uptake_N0) > 0,
                  (grain_yield_kg_ha - yield_N0) / (total_n_uptake_kg_ha - uptake_N0),
                  NA_real_)
  ) %>%
  dplyr::filter(treatment != "PP1") %>%
  dplyr::mutate(treatment = factor(treatment, levels = trt_nue))

# B 图：PFPN 已删除，只保留 AEN、REN、PEN。
pB_AEN <- make_nue_mirror_plot_rotated_final(
  dat = nue_season,
  response_var = "AEN",
  ylab_text = expression("AEN (kg kg"^-1*")"),
  show_x_text = FALSE,
  show_year_text = FALSE,
  show_crop_text = FALSE
)

pB_REN <- make_nue_mirror_plot_rotated_final(
  dat = nue_season,
  response_var = "REN",
  ylab_text = "REN (%)",
  show_x_text = FALSE,
  show_year_text = FALSE,
  show_crop_text = FALSE
)

pB_PEN <- make_nue_mirror_plot_rotated_final(
  dat = nue_season,
  response_var = "PEN",
  ylab_text = expression("PEN (kg kg"^-1*")"),
  show_x_text = TRUE,
  show_year_text = FALSE,
  show_crop_text = FALSE
)

year_lab_B <- make_nue_year_labels(dat = nue_season, trt_levels = trt_nue, gap = 0.8)

# -----------------------------
# 7. Panel C: seasonal yield vs seasonal NUE relationship
# -----------------------------
scatter_season <- nue_season %>%
  dplyr::mutate(
    crop_lab = dplyr::case_when(
      season == "rice"  ~ "Rice",
      season == "wheat" ~ "Wheat",
      TRUE ~ as.character(season)
    ),
    crop_lab = factor(crop_lab, levels = c("Rice", "Wheat")),
    treatment = factor(treatment, levels = trt_nue)
  ) %>%
  dplyr::select(
    year, block, season, crop_lab, treatment,
    grain_yield_kg_ha, AEN, REN, PEN
  ) %>%
  tidyr::pivot_longer(
    cols = c(AEN, REN, PEN),
    names_to = "indicator",
    values_to = "value"
  ) %>%
  dplyr::filter(!is.na(grain_yield_kg_ha), !is.na(value)) %>%
  dplyr::mutate(indicator = factor(indicator, levels = c("AEN", "REN", "PEN")))

scatter_mean <- scatter_season %>%
  dplyr::group_by(crop_lab, treatment, indicator) %>%
  dplyr::summarise(
    yield_mean = mean(grain_yield_kg_ha, na.rm = TRUE),
    nue_mean   = mean(value, na.rm = TRUE),
    yield_se   = sd(grain_yield_kg_ha, na.rm = TRUE) / sqrt(sum(!is.na(grain_yield_kg_ha))),
    nue_se     = sd(value, na.rm = TRUE) / sqrt(sum(!is.na(value))),
    .groups = "drop"
  )

cor_lab <- scatter_season %>%
  dplyr::group_by(indicator, crop_lab) %>%
  dplyr::group_modify(~{
    tmp <- .x
    if(nrow(tmp) < 3 ||
       length(unique(tmp$grain_yield_kg_ha)) < 2 ||
       length(unique(tmp$value)) < 2) {
      return(tibble::tibble(r = NA_real_, p = NA_real_))
    }
    ct <- stats::cor.test(tmp$grain_yield_kg_ha, tmp$value, method = "pearson")
    tibble::tibble(r = unname(ct$estimate), p = ct$p.value)
  }) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    p_lab = dplyr::case_when(
      is.na(p) ~ "P = NA",
      p < 0.001 ~ "P < 0.001",
      TRUE ~ paste0("P = ", sprintf("%.3f", p))
    ),
    label = dplyr::case_when(
      is.na(r) ~ paste0(crop_lab, ": r = NA, ", p_lab),
      TRUE ~ paste0(crop_lab, ": r = ", sprintf("%.2f", r), ", ", p_lab)
    )
  )

facet_pos <- scatter_season %>%
  dplyr::group_by(indicator) %>%
  dplyr::summarise(
    xmin = min(grain_yield_kg_ha, na.rm = TRUE),
    xmax = max(grain_yield_kg_ha, na.rm = TRUE),
    ymin = min(value, na.rm = TRUE),
    ymax = max(value, na.rm = TRUE),
    .groups = "drop"
  )

cor_lab_pos <- cor_lab %>%
  dplyr::left_join(facet_pos, by = "indicator") %>%
  dplyr::mutate(
    x = dplyr::case_when(
      crop_lab == "Wheat" ~ xmin + 0.04 * (xmax - xmin),
      crop_lab == "Rice"  ~ xmax - 0.04 * (xmax - xmin),
      TRUE ~ xmin + 0.05 * (xmax - xmin)
    ),
    y = dplyr::case_when(
      crop_lab == "Wheat" ~ ymax - 0.05 * (ymax - ymin),
      crop_lab == "Rice"  ~ ymin + 0.12 * (ymax - ymin),
      TRUE ~ ymax - 0.05 * (ymax - ymin)
    ),
    hjust = dplyr::case_when(crop_lab == "Wheat" ~ 0, crop_lab == "Rice" ~ 1, TRUE ~ 0),
    vjust = dplyr::case_when(crop_lab == "Wheat" ~ 1, crop_lab == "Rice" ~ 0, TRUE ~ 1)
  )

p_synergy_season <- ggplot2::ggplot(scatter_season, ggplot2::aes(x = grain_yield_kg_ha, y = value)) +
  ggplot2::geom_point(
    ggplot2::aes(fill = crop_lab, shape = treatment),
    size = 3,
    alpha = 0.5,
    colour = "grey35",
    stroke = 0.5
  ) +
  ggplot2::geom_smooth(
    ggplot2::aes(colour = crop_lab, group = crop_lab),
    method = "lm",
    se = TRUE,
    linewidth = 0.75,
    alpha = 0.13,
    show.legend = FALSE
  ) +
  ggplot2::geom_segment(
    data = scatter_mean,
    ggplot2::aes(x = yield_mean - yield_se, xend = yield_mean + yield_se, y = nue_mean, yend = nue_mean),
    colour = "black",
    linewidth = 1,
    inherit.aes = FALSE,
    show.legend = FALSE
  ) +
  ggplot2::geom_errorbar(
    data = scatter_mean,
    ggplot2::aes(x = yield_mean, ymin = nue_mean - nue_se, ymax = nue_mean + nue_se),
    colour = "black",
    linewidth = 1,
    inherit.aes = FALSE,
    show.legend = FALSE
  ) +
  ggplot2::geom_point(
    data = scatter_mean,
    ggplot2::aes(x = yield_mean, y = nue_mean, fill = crop_lab, shape = treatment),
    size = 4,
    colour = "black",
    stroke = 1,
    inherit.aes = FALSE
  ) +
  ggplot2::geom_text(
    data = cor_lab_pos,
    aes(x = x, y = y, label = label, colour = crop_lab, hjust = hjust, vjust = vjust),
    size = 5.5,
    lineheight = 1.0,
    fontface = "plain",
    show.legend = FALSE,
    inherit.aes = FALSE
  ) +
  ggplot2::facet_wrap(
    ~ indicator,
    nrow = 1,
    scales = "free_y",
    labeller = ggplot2::as_labeller(nue_indicator_labels)
  ) +
  ggplot2::scale_fill_manual(values = crop_cols, name = NULL) +
  ggplot2::scale_colour_manual(values = crop_cols, guide = "none") +
  ggplot2::scale_shape_manual(values = shape_vals, name = NULL) +
  ggplot2::labs(
    x = expression("Seasonal grain yield (kg ha"^-1*")"),
    y = "NUE indicator"
  ) +
  ggplot2::theme_classic(base_size = 12.5) +
  ggplot2::theme(
    strip.background = ggplot2::element_rect(fill = "grey96", colour = "grey70", linewidth = 0.45),
    strip.text = ggplot2::element_text(face = "bold", size = 11),
    axis.title = ggplot2::element_text(face = "bold", size = 11.5),
    axis.text = ggplot2::element_text(size = 9.5, colour = "black"),
    axis.line = ggplot2::element_line(linewidth = 0.40),
    axis.ticks = ggplot2::element_line(linewidth = 0.35),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.text = ggplot2::element_text(size = 9.5),
    legend.key.size = grid::unit(0.45, "cm"),
    legend.margin = ggplot2::margin(0, 0, 0, 0),
    legend.spacing.x = grid::unit(4, "pt"),
    panel.spacing = grid::unit(0.75, "lines"),
    plot.margin = ggplot2::margin(4, 6, 4, 6)
  ) +
  ggplot2::guides(
    fill = ggplot2::guide_legend(
      order = 1,
      override.aes = list(shape = 21, size = 3.3, colour = "black", alpha = 1)
    ),
    shape = ggplot2::guide_legend(
      order = 2,
      override.aes = list(fill = "white", colour = "black", size = 3.3, alpha = 1)
    )
  )

# -----------------------------
# 8. Final assembly
# -----------------------------
tag_theme <- ggplot2::theme(
  plot.tag = ggplot2::element_text(face = "bold", size = 19),
  plot.tag.position = c(0.01, 0.99)
)

pA_tag <- pA_final +
  ggplot2::labs(tag = "A") +
  tag_theme +
  ggplot2::guides(fill = "none", colour = "none", color = "none") +
  ggplot2::theme(
    legend.position = "none",
    axis.title.x = ggplot2::element_text(size = 14, face = "bold", colour = "black"),
    axis.text.x = ggplot2::element_text(size = 11.5, colour = "black"),
    axis.text.y = ggplot2::element_text(size = 11.5, colour = "black", face = "bold"),
    plot.margin = ggplot2::margin(4, 5, 4, 5)
  )

pB_AEN_tag <- pB_AEN +
  ggplot2::labs(tag = "B") +
  tag_theme +
  ggplot2::theme(
    legend.position = "top",
    legend.direction = "horizontal",
    legend.title = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(size = 12),
    legend.key.size = grid::unit(0.45, "cm"),
    legend.margin = ggplot2::margin(0, 0, 0, 0),
    legend.spacing.x = grid::unit(5, "pt"),
    axis.title.y = ggplot2::element_text(size = 12.5, face = "bold", colour = "black"),
    axis.text.y  = ggplot2::element_text(size = 12, colour = "black"),
    axis.text.x  = ggplot2::element_blank()
  )

pB_REN_tag <- pB_REN +
  ggplot2::labs(tag = NULL) +
  ggplot2::guides(fill = "none", colour = "none", color = "none") +
  ggplot2::theme(
    plot.tag = ggplot2::element_blank(),
    legend.position = "none",
    axis.title.y = ggplot2::element_text(size = 12.5, face = "bold", colour = "black"),
    axis.text.y  = ggplot2::element_text(size = 12, colour = "black"),
    axis.text.x  = ggplot2::element_blank()
  )

pB_PEN_tag <- pB_PEN +
  ggplot2::labs(tag = NULL) +
  ggplot2::guides(fill = "none", colour = "none", color = "none") +
  ggplot2::geom_text(
    data = year_lab_B,
    ggplot2::aes(x = xmid, y = -Inf, label = year),
    inherit.aes = FALSE,
    vjust = -0.5,
    size = 3.8,
    colour = "black"
  ) +
  ggplot2::coord_cartesian(clip = "off") +
  ggplot2::theme(
    plot.tag = ggplot2::element_blank(),
    legend.position = "none",
    axis.title.y = ggplot2::element_text(size = 12.5, face = "bold", colour = "black"),
    axis.text.y  = ggplot2::element_text(size = 12, colour = "black"),
    axis.text.x  = ggplot2::element_text(
      size = 14,
      angle = 90,
      vjust = 0.5,
      hjust = 1,
      colour = "black"
    ),
    plot.margin = ggplot2::margin(2, 4, 24, 4)
  )

pB_block <- (pB_AEN_tag / pB_REN_tag / pB_PEN_tag) +
  patchwork::plot_layout(heights = c(1, 1, 1.10))

pC_synergy_tag <- p_synergy_season +
  ggplot2::labs(tag = "C") +
  tag_theme +
  ggplot2::theme(
    strip.background = ggplot2::element_rect(fill = "grey96", colour = "grey70", linewidth = 0.45),
    strip.text = ggplot2::element_text(face = "bold", size = 12),
    axis.title.x = ggplot2::element_text(face = "bold", size = 14, colour = "black"),
    axis.title.y = ggplot2::element_text(face = "bold", size = 13, colour = "black"),
    axis.text.x = ggplot2::element_text(size = 12, colour = "black"),
    axis.text.y = ggplot2::element_text(size = 12, colour = "black"),
    legend.position = "top",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.text = ggplot2::element_text(size = 12),
    legend.key.size = grid::unit(0.45, "cm"),
    legend.margin = ggplot2::margin(0, 0, 0, 0),
    legend.spacing.x = grid::unit(5, "pt"),
    panel.spacing.x = grid::unit(0.90, "lines"),
    panel.spacing.y = grid::unit(0.20, "lines"),
    plot.margin = ggplot2::margin(6, 6, 4, 6)
  ) +
  ggplot2::guides(
    fill = ggplot2::guide_legend(
      order = 1,
      override.aes = list(shape = 21, size = 3.6, colour = "black", alpha = 1)
    ),
    shape = ggplot2::guide_legend(
      order = 2,
      override.aes = list(fill = "white", colour = "black", size = 3.6, alpha = 1)
    )
  )

# -----------------------------
# 8. Final assembly: only ORIGINAL Fig. 1A–C
# -----------------------------
# Keep the author's recognizable style: A/B on top, C across the lower row.
# All six-year group letters are retained. They are generated from the original
# per-year Fisher LSD method, now with block explicitly accounted for.
top_row <- pA_tag + pB_block + patchwork::plot_layout(widths = c(1.1, 2.5))
fig6 <- (top_row / pC_synergy_tag) + patchwork::plot_layout(heights = c(1.42, 1))

print(fig6)

# Main manuscript figure, raster + vector
tryCatch(
  ggplot2::ggsave(file.path(out_dir,"Figure6_STR_longterm_yield_NUE.png"),
                 fig6,width=19.0,height=13.3,dpi=600,bg="white",limitsize=FALSE),
  error=function(e) stop("Could not export Figure 6 PNG: ", conditionMessage(e))
)
ggplot2::ggsave(file.path(out_dir,"Figure6_STR_longterm_yield_NUE.pdf"),
                fig6,width=19.0,height=13.3,bg="white",limitsize=FALSE)

# Export independent A/B/C panels to allow clean editorial layout changes later.
ggplot2::ggsave(file.path(out_dir,"Figure6_panel_A_yield.png"),
                pA_tag,width=6.9,height=9.1,dpi=450,bg="white")
ggplot2::ggsave(file.path(out_dir,"Figure6_panel_B_NUE.png"),
                pB_block,width=11,height=9.1,dpi=450,bg="white")
ggplot2::ggsave(file.path(out_dir,"Figure6_panel_C_yield_NUE_association.png"),
                pC_synergy_tag,width=18,height=5.7,dpi=450,bg="white")

# -----------------------------
# 9. Export audit/appendix tables (without changing the user's input workbook)
# -----------------------------
annual_sum <- yield_crop %>% dplyr::group_by(year,season,treatment) %>%
  dplyr::summarise(mean_yield=mean(yield), se_yield=stats::sd(yield)/sqrt(dplyr::n()),
                   n_blocks=dplyr::n(),.groups="drop")
annual_yield <- yield_crop %>% dplyr::group_by(year,treatment,block) %>%
  dplyr::summarise(annual_yield_kg_ha=sum(yield),.groups="drop")
annual_yield_summary <- annual_yield %>% dplyr::group_by(treatment) %>%
  dplyr::summarise(mean_annual_yield=mean(annual_yield_kg_ha),
                   se=stats::sd(annual_yield_kg_ha)/sqrt(dplyr::n()),n=dplyr::n(),
                   .groups="drop")
nue_summary <- nue_season %>% dplyr::group_by(year,season,treatment) %>%
  dplyr::summarise(dplyr::across(c(AEN,REN,PEN),
                                 list(mean=~mean(.x,na.rm=TRUE),
                                      se=~sd(.x,na.rm=TRUE)/sqrt(sum(!is.na(.x))))),
                   .groups="drop")
nue_sample_audit <- nue_season %>% dplyr::group_by(year,season,treatment) %>%
  dplyr::summarise(dplyr::across(c(AEN,REN,PEN),~sum(is.finite(.x)),.names="n_{.col}"),
                   .groups="drop")

# Letters in panel A and in all three B indicators, plus descriptive C correlation.
writexl::write_xlsx(
  list(
    yield_plot_level=yield_crop %>% dplyr::mutate(dplyr::across(where(is.factor),as.character)),
    yield_mean_se=annual_sum %>% dplyr::mutate(dplyr::across(where(is.factor),as.character)),
    yield_sig_letters=yield_letters %>% dplyr::mutate(dplyr::across(where(is.factor),as.character)),
    annual_yield=annual_yield_summary %>% dplyr::mutate(dplyr::across(where(is.factor),as.character)),
    NUE_plot_level=nue_season %>% dplyr::mutate(dplyr::across(where(is.factor),as.character)),
    NUE_mean_se=nue_summary %>% dplyr::mutate(dplyr::across(where(is.factor),as.character)),
    NUE_nonmissing_n=nue_sample_audit %>% dplyr::mutate(dplyr::across(where(is.factor),as.character)),
    NUE_AEN_letters=get_sig_letters_by_year_metric(nue_season,"AEN") %>%
      dplyr::mutate(dplyr::across(where(is.factor),as.character)),
    NUE_REN_letters=get_sig_letters_by_year_metric(nue_season,"REN") %>%
      dplyr::mutate(dplyr::across(where(is.factor),as.character)),
    NUE_PEN_letters=get_sig_letters_by_year_metric(nue_season,"PEN") %>%
      dplyr::mutate(dplyr::across(where(is.factor),as.character)),
    scatter_correlations=cor_lab %>% dplyr::mutate(dplyr::across(where(is.factor),as.character))
  ),file.path(out_dir,"Figure6_STR_data_statistics_audit.xlsx")
)

cat("Finished Figure 6 (only former Figure 1 A/B/C).\n",
    "Output directory: ",out_dir,"\n",
    "NOTE: AEN uses yield mathematically; panel C is exploratory only.\n",
    "NOTE: PP1 is a differently managed zero-N reference.\n",sep="")
