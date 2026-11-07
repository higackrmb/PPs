# =============================================================================
# Figure 7B: management, soil N accessibility, seasonal N acquisition and
# annual outcomes in a rice-wheat rotation
#
# This script is deliberately an association diagram, not a causal SEM.
# It uses only year x treatment x block records that can be matched directly.
# =============================================================================

options(stringsAsFactors = FALSE, width = 180)
set.seed(20261022)

required_pkgs <- c(
  "readxl", "dplyr", "tidyr", "stringr", "tibble", "ggplot2",
  "lme4", "lmerTest", "emmeans", "vegan", "ragg"
)
missing_pkgs <- required_pkgs[
  !vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_pkgs)) {
  stop("Install required packages first: ", paste(missing_pkgs, collapse = ", "))
}
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(tibble)
  library(ggplot2)
  library(lme4)
  library(lmerTest)
  library(emmeans)
  library(vegan)
})

# ---- 1. Paths ---------------------------------------------------------------
project_dir <- "F:/Article/SCI/2025/Planting pattern"

first_present <- function(paths) {
  found <- paths[file.exists(paths)]
  if (!length(found)) stop("Input file not found. Checked: ", paste(paths, collapse = " | "))
  found[1]
}

soil_file <- first_present(c(
  file.path(project_dir, "Data(20261021-093806).xlsx"),
  file.path(project_dir, "Data.xlsx")
))
crop_file <- first_present(c(
  file.path(project_dir, "figure", "3.4", "Figure5_clean_N_uptake_15N_data.xlsx"),
  file.path(project_dir, "figure", "3.1", "yield and NUE.xlsx")
))

# The environment override is useful for a protected or sandboxed run.
default_out <- file.path(
  project_dir,
  intToUtf8(c(35770, 25991, 25237, 31295, 45, 83, 84, 82)),
  "Figure", "figures", "Figure7B_statistically_revised"
)
out_dir <- "F:/Article/figures/fig7"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(out_dir)) stop("Could not create output directory: ", out_dir)

message("Soil/plant workbook: ", soil_file)
message("Crop/N workbook: ", crop_file)
message("Output directory: ", out_dir)

# ---- 2. Standardization helpers --------------------------------------------
num <- function(x) suppressWarnings(as.numeric(gsub(",", "", as.character(x))))

norm_treatment <- function(x) {
  z <- toupper(str_squish(as.character(x)))
  z <- str_replace(z, "^T([1-6])$", "PP\\1")
  z
}

norm_block <- function(x) {
  z <- toupper(str_squish(as.character(x)))
  case_when(
    z %in% c("4#", "4", "I", "1") ~ "I",
    z %in% c("5#", "5", "II", "2") ~ "II",
    z %in% c("6#", "6", "III", "3") ~ "III",
    TRUE ~ NA_character_
  )
}

norm_crop <- function(x) {
  z <- str_to_lower(str_squish(as.character(x)))
  case_when(
    str_detect(z, "wheat") ~ "wheat",
    str_detect(z, "rice") ~ "rice",
    TRUE ~ z
  )
}

jointing_cn <- intToUtf8(c(25300, 33410, 26399))
anthesis_cn <- intToUtf8(c(24320, 33457, 26399))

norm_stage <- function(x) {
  z <- str_to_lower(str_squish(as.character(x)))
  case_when(
    str_detect(z, "joint") | z == jointing_cn ~ "jointing",
    str_detect(z, "anthesis") | z == anthesis_cn ~ "anthesis",
    str_detect(z, "before sow|pre.?sow") ~ "before sowing",
    str_detect(z, "sowing") ~ "sowing",
    str_detect(z, "maturity|mathrity") ~ "maturity",
    TRUE ~ z
  )
}

pp_levels <- paste0("PP", 1:6)
block_levels <- c("I", "II", "III")

contrast_defs <- tribble(
  ~contrast, ~from, ~to, ~management_id,
  "N addition", "PP1", "PP2", "M1",
  "Straw return", "PP2", "PP3", "M2",
  "Wheat INtS + straw mode", "PP3", "PP4", "M3",
  "Rice rotary direct seeding", "PP4", "PP5", "M4",
  "Rice no-till dry direct seeding vs PP4", "PP4", "PP6", "M5"
)
contrast_methods <- list(
  "PP2 - PP1" = c(-1, 1, 0, 0, 0, 0),
  "PP3 - PP2" = c(0, -1, 1, 0, 0, 0),
  "PP4 - PP3" = c(0, 0, -1, 1, 0, 0),
  "PP5 - PP4" = c(0, 0, 0, -1, 1, 0),
  "PP6 - PP4" = c(0, 0, 0, -1, 0, 1)
)
contrast_name_map <- setNames(contrast_defs$contrast, names(contrast_methods))

# Chinese worksheet names are not used as lookup keys. The verified worksheet
# positions in Data.xlsx are used so the script remains robust under Windows
# locales that print Unicode worksheet names as <U+....>.
sheet_index <- c(
  SOC = 4,
  BulkDensity = 6,
  SWC = 8,
  MineralN = 9,
  Porosity = 10,
  Kunsat = 12,
  Root = 14
)

read_soil_sheet <- function(index) {
  d <- read_excel(soil_file, sheet = index)
  key_pos <- match(c("year", "re", "treat", "date"), names(d))
  if (anyNA(key_pos)) stop("Expected keys year/re/treat/date are missing in worksheet index ", index)
  d %>%
    mutate(
      year = as.integer(num(.data$year)),
      treatment = norm_treatment(.data$treat),
      block = norm_block(.data$re),
      stage = norm_stage(.data$date),
      crop_std = if ("crop" %in% names(.)) norm_crop(.data$crop) else NA_character_,
      depth_num = if ("depth" %in% names(.)) num(.data$depth) else NA_real_
    ) %>%
    filter(year %in% 2020:2025, treatment %in% pp_levels, block %in% block_levels)
}

make_metric <- function(d, value, metric, season, stage, depth, unit, source_sheet, notes = "") {
  d %>%
    transmute(
      year, treatment, block,
      value = num({{ value }}),
      metric = .env$metric,
      season = .env$season,
      stage = .env$stage,
      depth = .env$depth,
      unit = .env$unit,
      source_sheet = .env$source_sheet,
      notes = .env$notes
    ) %>%
    filter(is.finite(value)) %>%
    group_by(year, treatment, block, metric, season, stage, depth, unit, source_sheet, notes) %>%
    summarise(value = mean(value), .groups = "drop")
}

# ---- 3. Soil, hydraulic and root variables ---------------------------------
# Wheat jointing soil water content, strictly 0-10 cm.
swc0 <- read_soil_sheet(sheet_index[["SWC"]])
SWC <- swc0 %>%
  filter(crop_std == "wheat", stage == "jointing", depth_num == 10) %>%
  make_metric(water_content, "SWC", "wheat", "jointing", "0-10 cm", "%", "Data.xlsx sheet 8")

# Capillary porosity is verified as column 11 in worksheet 10.
por0 <- read_soil_sheet(sheet_index[["Porosity"]])
CapillaryPorosity <- por0 %>%
  filter(crop_std == "wheat", stage == "jointing", depth_num == 10) %>%
  mutate(capillary_porosity = num(.data[[names(por0)[11]]])) %>%
  make_metric(capillary_porosity, "CapillaryPorosity", "wheat", "jointing", "0-10 cm", "%", "Data.xlsx sheet 10")

# SOC is pre-sowing 0-10 cm. Baseline 2020 rows have no treatment and are not
# used in treatment contrasts or matched associations.
soc0 <- read_soil_sheet(sheet_index[["SOC"]])
SOC <- soc0 %>%
  filter(stage == "before sowing", depth_num == 10) %>%
  make_metric(SOC, "SOC", "wheat", "before sowing", "0-10 cm", "g kg-1", "Data.xlsx sheet 4")

# Bulk density is column 8 in worksheet 6.
bd0 <- read_soil_sheet(sheet_index[["BulkDensity"]])
BulkDensity <- bd0 %>%
  filter(crop_std == "wheat", stage == "jointing", depth_num == 10) %>%
  make_metric(bulk_density, "BulkDensity", "wheat", "jointing", "0-10 cm", "g cm-3", "Data.xlsx sheet 6")

# Mineral N: NH4-N + NO3-N. Wheat uses a thickness-weighted 0-40 cm
# concentration index at jointing; rice uses 0-10 cm at anthesis.
mn0 <- read_soil_sheet(sheet_index[["MineralN"]]) %>%
  mutate(
    nh4 = num(.data[[names(.)[7]]]),
    no3 = num(.data[[names(.)[8]]]),
    mineral_n = nh4 + no3,
    layer_width = case_when(depth_num == 10 ~ 10, depth_num == 20 ~ 10, depth_num == 40 ~ 20, TRUE ~ NA_real_)
  )

WheatMineralN <- mn0 %>%
  filter(crop_std == "wheat", stage == "jointing", depth_num %in% c(10, 20, 40), is.finite(mineral_n)) %>%
  group_by(year, treatment, block) %>%
  summarise(value = weighted.mean(mineral_n, layer_width), .groups = "drop") %>%
  mutate(
    metric = "WheatMineralN", season = "wheat", stage = "jointing", depth = "0-40 cm weighted",
    unit = "mg kg-1", source_sheet = "Data.xlsx sheet 9",
    notes = "Thickness-weighted concentration index; not a soil N stock"
  )

RiceMineralN <- mn0 %>%
  filter(crop_std == "rice", stage == "anthesis", depth_num == 10, is.finite(mineral_n)) %>%
  group_by(year, treatment, block) %>%
  summarise(value = mean(mineral_n), .groups = "drop") %>%
  mutate(
    metric = "RiceMineralN", season = "rice", stage = "anthesis", depth = "0-10 cm",
    unit = "mg kg-1", source_sheet = "Data.xlsx sheet 9", notes = "NH4-N + NO3-N"
  )

# Kunsat: 2024 wheat jointing. The stable late-time estimate is the mean of the
# final 30% of readings within each plot.
ku0 <- read_soil_sheet(sheet_index[["Kunsat"]]) %>%
  mutate(test_time = num(.data[[names(.)[6]]]), conductivity = num(.data[[names(.)[7]]])) %>%
  filter(crop_std == "wheat", stage == "jointing", year == 2024,
         is.finite(test_time), is.finite(conductivity))
Kunsat <- ku0 %>%
  arrange(year, treatment, block, test_time) %>%
  group_by(year, treatment, block) %>%
  summarise(
    value = {
      ord <- order(test_time)
      v <- conductivity[ord]
      mean(tail(v, max(1, ceiling(length(v) * 0.30))))
    },
    .groups = "drop"
  ) %>%
  mutate(
    metric = "Kunsat", season = "wheat", stage = "jointing", depth = NA_character_,
    unit = "cm min-1", source_sheet = "Data.xlsx sheet 12",
    notes = "Mean of final 30% of plot-specific readings"
  )

# Root dry weight is summed across 0-40 cm. Root activity is root-DW weighted.
root0 <- read_soil_sheet(sheet_index[["Root"]]) %>%
  mutate(root_activity = num(.data[[names(.)[7]]]), root_dw = num(.data[[names(.)[8]]])) %>%
  filter(crop_std == "wheat", stage == "jointing", depth_num %in% c(10, 20, 40),
         is.finite(root_activity), is.finite(root_dw))
root_plot <- root0 %>%
  group_by(year, treatment, block) %>%
  summarise(
    RootDryWeight = sum(root_dw),
    RootActivity = if_else(sum(root_dw) > 0, weighted.mean(root_activity, root_dw), mean(root_activity)),
    .groups = "drop"
  )
RootDryWeight <- root_plot %>%
  transmute(
    year, treatment, block, value = RootDryWeight, metric = "RootDryWeight",
    season = "wheat", stage = "jointing", depth = "0-40 cm", unit = "g",
    source_sheet = "Data.xlsx sheet 14", notes = "Sum across three measured layers"
  )
RootActivity <- root_plot %>%
  transmute(
    year, treatment, block, value = RootActivity, metric = "RootActivity",
    season = "wheat", stage = "jointing", depth = "0-40 cm", unit = "ug TPF g-1 FW h-1",
    source_sheet = "Data.xlsx sheet 14", notes = "Root-dry-weight-weighted mean"
  )

metric_list <- list(
  SWC = SWC,
  Kunsat = Kunsat,
  SOC = SOC,
  BulkDensity = BulkDensity,
  CapillaryPorosity = CapillaryPorosity,
  WheatMineralN = WheatMineralN,
  RiceMineralN = RiceMineralN,
  RootDryWeight = RootDryWeight,
  RootActivity = RootActivity
)

for (nm in names(metric_list)) {
  d <- metric_list[[nm]]
  if (!nrow(d)) stop("No usable records for metric: ", nm)
  if (anyDuplicated(d[c("year", "treatment", "block")])) stop("Duplicate plot-year records for metric: ", nm)
}

# ---- 4. Microbial community: 2024, 0-10 cm -------------------------------
u8 <- function(codepoints) intToUtf8(codepoints)
microbe_report_dir <- file.path(
  project_dir,
  u8(c(21407, 22987, 25968, 25454)),
  u8(c(24494, 29983, 29289, 27979, 35797)),
  u8(c(48, 45, 49, 48, 20998, 26512, 32467, 26524)),
  u8(c(36154, 26143, 45, 26446, 26126, 45, 49, 56, 115, 97, 109, 45, 51, 45, 20998, 26512, 25253, 21578))
)
pcoa_file <- file.path(microbe_report_dir, "05.Beta", "04.PCoA", "Bray_Curtis", "PCoA_score.xls")
dist_file <- file.path(microbe_report_dir, "05.Beta", "01.beta_diversity", "Bray_Curtis_distance.xls")

microbe_available <- file.exists(pcoa_file) && file.exists(dist_file)
microbe_note <- "Not available"
BacterialPCoA1 <- tibble()
bacterial_contrasts <- tibble()

if (microbe_available) {
  pcoa <- read.delim(pcoa_file, check.names = FALSE, row.names = 1)
  dist_mat <- as.matrix(read.delim(dist_file, check.names = FALSE, row.names = 1))
  # The verified metadata crosswalk maps the six 0-10 cm sample groups
  # W1, W4, W7, W10, W13 and W16 to PP1-PP6. The suffix -1/-2/-3 is the
  # field replicate. Parse the complete sample IDs directly so the workflow
  # is not blocked by Windows locale handling of the Chinese metadata path.
  samples <- rownames(pcoa)
  group_no <- as.integer(str_match(samples, "^W([0-9]+)-([1-3])$")[, 2])
  replicate <- as.integer(str_match(samples, "^W([0-9]+)-([1-3])$")[, 3])
  map <- tibble(
    sample = samples,
    treatment = paste0("PP", match(group_no, c(1, 4, 7, 10, 13, 16))),
    replicate = replicate,
    block = c("I", "II", "III")[replicate]
  ) %>%
    filter(treatment %in% pp_levels, !is.na(block))

  common <- Reduce(intersect, list(rownames(pcoa), rownames(dist_mat), colnames(dist_mat), map$sample))
  if (length(common) == 18) {
    map <- map %>% filter(sample %in% common) %>% arrange(match(sample, common))
    BacterialPCoA1 <- tibble(
      sample = common,
      value = pcoa[common, 1],
      year = 2024L
    ) %>%
      left_join(map %>% dplyr::select(sample, treatment, block), by = "sample") %>%
      transmute(
        year, treatment, block, value, metric = "BacterialPCoA1", season = "wheat",
        stage = "microbial sampling", depth = "0-10 cm", unit = "PCoA1 score",
        source_sheet = "0-10 cm Bray-Curtis PCoA_score.xls",
        notes = "Axis sign is arbitrary; used only for exploratory matched associations"
      )

    pairwise_permanova <- function(from, to, label) {
      samples <- map$sample[map$treatment %in% c(from, to)]
      meta <- map %>% filter(sample %in% samples) %>% arrange(match(sample, samples))
      dm <- as.dist(dist_mat[meta$sample, meta$sample, drop = FALSE])
      ctrl <- permute::how(nperm = 9999)
      permute::setBlocks(ctrl) <- factor(meta$block)
      fit <- vegan::adonis2(dm ~ treatment, data = meta, permutations = ctrl)
      tibble(
        metric = "BacterialCommunity",
        contrast = label,
        from = from,
        to = to,
        scope = "2024 pairwise PERMANOVA",
        year = 2024L,
        n_obs = nrow(meta),
        n_years = 1L,
        estimate = fit$R2[1],
        SE = NA_real_,
        df = fit$Df[1],
        lower95 = NA_real_,
        upper95 = NA_real_,
        statistic = fit$F[1],
        p = fit$`Pr(>F)`[1],
        interaction_p = NA_real_,
        singular_fit = NA,
        effect_type = "PERMANOVA R2 (unsigned)",
        model = "Bray-Curtis PERMANOVA; permutations restricted within block",
        plot_eligible = FALSE,
        note = "SE/CI and positive/negative direction are not applicable to multivariate community composition"
      )
    }
    bacterial_contrasts <- bind_rows(lapply(seq_len(nrow(contrast_defs)), function(i) {
      pairwise_permanova(contrast_defs$from[i], contrast_defs$to[i], contrast_defs$contrast[i])
    })) %>%
      mutate(q_within_metric = p.adjust(p, method = "BH"))
    metric_list$BacterialPCoA1 <- BacterialPCoA1
    microbe_note <- "Included: complete 18-sample 2024 0-10 cm mapping"
  } else {
    microbe_available <- FALSE
    microbe_note <- paste0("Excluded: common sample count was ", length(common), ", expected 18")
  }
}

# ---- 5. Crop N, source-specific N, yield, REN and PEN ----------------------
if (!all(c("plant_N_plot_all", "NUE_annual", "N15_source") %in% excel_sheets(crop_file))) {
  stop("The selected crop workbook lacks one or more required sheets: plant_N_plot_all, NUE_annual, N15_source")
}

crops <- read_excel(crop_file, sheet = "plant_N_plot_all") %>%
  transmute(
    year = as.integer(num(year)),
    season = norm_crop(season),
    treatment = norm_treatment(treatment),
    block = norm_block(block),
    N_rate_kg_ha = num(n_rate_kg_ha),
    yield_kg_ha = num(grain_yield_kg_ha),
    total_N_uptake_kg_ha = num(total_n_uptake_kg_ha)
  ) %>%
  filter(year %in% 2020:2025, season %in% c("wheat", "rice"),
         treatment %in% pp_levels, block %in% block_levels)

if (nrow(crops) != 216 || anyDuplicated(crops[c("year", "season", "treatment", "block")])) {
  stop("Expected 216 unique crop records (6 years x 2 seasons x 6 treatments x 3 blocks).")
}

controls <- crops %>%
  filter(treatment == "PP1") %>%
  dplyr::select(year, season, block, control_N = total_N_uptake_kg_ha, control_yield = yield_kg_ha)
crops <- crops %>%
  left_join(controls, by = c("year", "season", "block")) %>%
  mutate(
    delta_N = total_N_uptake_kg_ha - control_N,
    delta_yield = yield_kg_ha - control_yield,
    REN_pct = if_else(N_rate_kg_ha > 0, 100 * delta_N / N_rate_kg_ha, NA_real_),
    PEN_kg_grain_per_kg_N_uptake = if_else(N_rate_kg_ha > 0 & abs(delta_N) > 1e-10,
                                           delta_yield / delta_N, NA_real_)
  )

n15 <- read_excel(crop_file, sheet = "N15_source") %>%
  transmute(
    year = as.integer(num(year)),
    season = norm_crop(season),
    treatment = norm_treatment(treatment),
    block = norm_block(block),
    total_crop_N_kg_ha = num(total_crop_n_kg_ha),
    fertilizer_N_uptake_kg_ha = num(fertilizer_derived_N_uptake_kg_ha),
    soil_N_uptake_kg_ha = num(soil_derived_N_uptake_kg_ha)
  ) %>%
  filter(year == 2024, season %in% c("wheat", "rice"),
         treatment %in% paste0("PP", 2:6), block %in% c("I", "II"))

if (nrow(n15) != 20 || anyDuplicated(n15[c("year", "season", "treatment", "block")])) {
  stop("Expected 20 direct 2024 15N crop-source records (2 seasons x 5 treatments x 2 blocks).")
}

annual_calc <- crops %>%
  group_by(year, treatment, block) %>%
  summarise(
    annual_yield_kg_ha = sum(yield_kg_ha),
    annual_N_uptake_kg_ha = sum(total_N_uptake_kg_ha),
    annual_N_rate_kg_ha = sum(N_rate_kg_ha),
    .groups = "drop"
  )
annual_controls <- annual_calc %>%
  filter(treatment == "PP1") %>%
  dplyr::select(year, block, annual_control_N = annual_N_uptake_kg_ha,
         annual_control_yield = annual_yield_kg_ha)
annual_calc <- annual_calc %>%
  left_join(annual_controls, by = c("year", "block")) %>%
  mutate(
    annual_REN_pct = if_else(annual_N_rate_kg_ha > 0,
      100 * (annual_N_uptake_kg_ha - annual_control_N) / annual_N_rate_kg_ha, NA_real_),
    annual_PEN = if_else(
      annual_N_rate_kg_ha > 0 & abs(annual_N_uptake_kg_ha - annual_control_N) > 1e-10,
      (annual_yield_kg_ha - annual_control_yield) /
        (annual_N_uptake_kg_ha - annual_control_N), NA_real_)
  )

annual_workbook <- read_excel(crop_file, sheet = "NUE_annual") %>%
  transmute(
    year = as.integer(num(year)), treatment = norm_treatment(treatment), block = norm_block(block),
    workbook_annual_yield = num(annual_yield_kg_ha), workbook_annual_N = num(annual_n_uptake_kg_ha),
    workbook_annual_REN = num(ren_pct)
  )
annual_check <- annual_calc %>%
  left_join(annual_workbook, by = c("year", "treatment", "block")) %>%
  mutate(
    yield_difference = annual_yield_kg_ha - workbook_annual_yield,
    N_difference = annual_N_uptake_kg_ha - workbook_annual_N,
    REN_difference = annual_REN_pct - workbook_annual_REN
  )

# Crop-season and annual responses used for the five planned management
# contrasts as the soil variables.  REN/PEN and direct 15N source allocation
# are defined only for the N-fertilized patterns PP2-PP6, so the N-addition
# contrast (PP2-PP1) is not estimable for those responses and is not invented.
crop_metric_list <- list(
  WheatTotalN = crops %>% filter(season == "wheat") %>%
    transmute(year, treatment, block, value = total_N_uptake_kg_ha),
  RiceTotalN = crops %>% filter(season == "rice") %>%
    transmute(year, treatment, block, value = total_N_uptake_kg_ha),
  WheatYield = crops %>% filter(season == "wheat") %>%
    transmute(year, treatment, block, value = yield_kg_ha),
  RiceYield = crops %>% filter(season == "rice") %>%
    transmute(year, treatment, block, value = yield_kg_ha),
  WheatREN = crops %>% filter(season == "wheat", treatment != "PP1") %>%
    transmute(year, treatment, block, value = REN_pct),
  RiceREN = crops %>% filter(season == "rice", treatment != "PP1") %>%
    transmute(year, treatment, block, value = REN_pct),
  WheatPEN = crops %>% filter(season == "wheat", treatment != "PP1") %>%
    transmute(year, treatment, block, value = PEN_kg_grain_per_kg_N_uptake),
  RicePEN = crops %>% filter(season == "rice", treatment != "PP1") %>%
    transmute(year, treatment, block, value = PEN_kg_grain_per_kg_N_uptake),
  AnnualYield = annual_calc %>%
    transmute(year, treatment, block, value = annual_yield_kg_ha),
  AnnualREN = annual_calc %>% filter(treatment != "PP1") %>%
    transmute(year, treatment, block, value = annual_REN_pct),
  WheatFertN = n15 %>% filter(season == "wheat") %>%
    transmute(year, treatment, block, value = fertilizer_N_uptake_kg_ha),
  WheatSoilN = n15 %>% filter(season == "wheat") %>%
    transmute(year, treatment, block, value = soil_N_uptake_kg_ha),
  RiceFertN = n15 %>% filter(season == "rice") %>%
    transmute(year, treatment, block, value = fertilizer_N_uptake_kg_ha),
  RiceSoilN = n15 %>% filter(season == "rice") %>%
    transmute(year, treatment, block, value = soil_N_uptake_kg_ha)
)

write.csv(crops, file.path(out_dir, "Figure7B_seasonal_crop_metrics.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(n15, file.path(out_dir, "Figure7B_15N_source_data_direct.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(annual_check, file.path(out_dir, "Figure7B_annual_outcomes_check.csv"), row.names = FALSE, fileEncoding = "UTF-8")

# ---- 6. Five planned management contrasts ---------------------------------
extract_coef <- function(fit, term = "x_z") {
  co <- summary(fit)$coefficients
  if (!term %in% rownames(co)) return(c(beta = NA, SE = NA, df = NA, statistic = NA, p = NA))
  pcol <- grep("^Pr\\(", colnames(co), value = TRUE)
  dfcol <- grep("^df$", colnames(co), value = TRUE)
  statcol <- grep("t value|z value", colnames(co), value = TRUE)
  c(
    beta = co[term, "Estimate"],
    SE = co[term, "Std. Error"],
    df = if (length(dfcol)) co[term, dfcol[1]] else stats::df.residual(fit),
    statistic = if (length(statcol)) co[term, statcol[1]] else NA_real_,
    p = if (length(pcol)) co[term, pcol[1]] else NA_real_
  )
}

fit_management_metric <- function(dat, metric_name) {
  d <- dat %>%
    filter(is.finite(value)) %>%
    mutate(
      block = factor(block, levels = block_levels),
      year_f = factor(year),
      plot_id = interaction(treatment, block, drop = TRUE)
    )
  treatment_levels <- pp_levels[pp_levels %in% unique(d$treatment)]
  d <- d %>% mutate(treatment = factor(treatment, levels = treatment_levels))
  n_years <- n_distinct(d$year)
  if (!length(treatment_levels) || !all(diff(match(treatment_levels, pp_levels)) == 1)) {
    stop("Management contrasts require consecutive PP levels for ", metric_name)
  }
  contrast_rows <- contrast_defs %>%
    filter(from %in% treatment_levels, to %in% treatment_levels)
  contrast_methods_use <- lapply(seq_len(nrow(contrast_rows)), function(i) {
    v <- rep(0, length(treatment_levels))
    v[match(contrast_rows$from[i], treatment_levels)] <- -1
    v[match(contrast_rows$to[i], treatment_levels)] <- 1
    v
  })
  names(contrast_methods_use) <- paste(contrast_rows$to, "-", contrast_rows$from)
  contrast_name_map_use <- setNames(contrast_rows$contrast, names(contrast_methods_use))

  interaction_p <- NA_real_
  singular_fit <- FALSE
  model_label <- "blocked linear model (one year)"
  if (n_years >= 2) {
    fit_add <- suppressWarnings(lmerTest::lmer(
      value ~ treatment + year_f + block + (1 | plot_id), data = d, REML = FALSE
    ))
    fit_int <- suppressWarnings(lmerTest::lmer(
      value ~ treatment * year_f + block + (1 | plot_id), data = d, REML = FALSE
    ))
    interaction_p <- tryCatch(
      as.numeric(anova(fit_add, fit_int)[2, "Pr(>Chisq)"]),
      error = function(e) NA_real_
    )
    fit <- fit_add
    singular_fit <- lme4::isSingular(fit_add, tol = 1e-5)
    model_label <- "mixed model: treatment + year + block + random plot"
  } else {
    fit <- lm(value ~ treatment + block, data = d)
  }

  emm <- emmeans::emmeans(fit, ~ treatment)
  cc <- as.data.frame(emmeans::contrast(emm, method = contrast_methods_use, adjust = "none"))
  ci <- as.data.frame(confint(emmeans::contrast(emm, method = contrast_methods_use, adjust = "none")))
  out <- cc %>%
    transmute(
      metric = metric_name,
      contrast_code = as.character(contrast),
      contrast = unname(contrast_name_map_use[contrast_code]),
      from = contrast_defs$from[match(contrast, contrast_defs$contrast)],
      to = contrast_defs$to[match(contrast, contrast_defs$contrast)],
      scope = "pooled",
      year = NA_integer_,
      n_obs = nrow(d), n_years = n_years,
      estimate = estimate, SE = SE, df = df,
      lower95 = ci$lower.CL, upper95 = ci$upper.CL,
      statistic = t.ratio, p = p.value,
      interaction_p = interaction_p,
      singular_fit = singular_fit,
      effect_type = "raw planned treatment difference",
      model = model_label,
      plot_eligible = is.na(interaction_p) | interaction_p >= 0.05,
      note = if_else(!is.na(interaction_p) & interaction_p < 0.05,
                     "Treatment-by-year heterogeneity detected; pooled arrow suppressed", "")
    )

  # Export year-specific estimates when heterogeneity is detected.
  if (n_years >= 2 && is.finite(interaction_p) && interaction_p < 0.05) {
    emm_y <- emmeans::emmeans(fit_int, ~ treatment | year_f)
    cy <- as.data.frame(emmeans::contrast(emm_y, method = contrast_methods_use, adjust = "none"))
    ciy <- as.data.frame(confint(emmeans::contrast(emm_y, method = contrast_methods_use, adjust = "none")))
    out_y <- cy %>%
      transmute(
        metric = metric_name,
        contrast_code = as.character(contrast),
        contrast = unname(contrast_name_map_use[contrast_code]),
        from = contrast_defs$from[match(contrast, contrast_defs$contrast)],
        to = contrast_defs$to[match(contrast, contrast_defs$contrast)],
        scope = "year-specific",
        year = as.integer(as.character(year_f)),
        n_obs = nrow(d), n_years = n_years,
        estimate = estimate, SE = SE, df = df,
        lower95 = ciy$lower.CL, upper95 = ciy$upper.CL,
        statistic = t.ratio, p = p.value,
        interaction_p = interaction_p,
        singular_fit = lme4::isSingular(fit_int, tol = 1e-5),
        effect_type = "raw planned treatment difference",
        model = "treatment x year mixed model",
        plot_eligible = FALSE,
        note = "Year-specific estimate due to treatment-by-year heterogeneity"
      )
    out <- bind_rows(out, out_y)
  }
  out
}

management_univariate <- bind_rows(lapply(names(metric_list), function(nm) {
  if (nm == "BacterialPCoA1") return(NULL)
  fit_management_metric(metric_list[[nm]], nm)
}), lapply(names(crop_metric_list), function(nm) {
  fit_management_metric(crop_metric_list[[nm]], nm)
})) %>%
  group_by(metric, scope, year) %>%
  mutate(q_within_metric = p.adjust(p, method = "BH")) %>%
  ungroup()

management_results <- bind_rows(management_univariate, bacterial_contrasts) %>%
  mutate(q_global = p.adjust(p, method = "BH")) %>%
  dplyr::select(metric, contrast, from, to, scope, year, n_obs, n_years,
         estimate, SE, df, lower95, upper95, statistic, p,
         q_within_metric, q_global, interaction_p, effect_type, model,
         singular_fit, plot_eligible, note)

write.csv(
  management_results,
  file.path(out_dir, "Figure7B_management_contrasts.csv"),
  row.names = FALSE, fileEncoding = "UTF-8"
)

# ---- 7. Year-matched soil-N associations ----------------------------------
outcome_long <- bind_rows(
  crops %>% transmute(year, treatment, block, season,
                      outcome = "Total N uptake", response = total_N_uptake_kg_ha,
                      source = "plant_N_plot_all; 2020-2025"),
  n15 %>% transmute(year, treatment, block, season,
                    outcome = "Fertilizer-derived N uptake", response = fertilizer_N_uptake_kg_ha,
                    source = "direct 15N source allocation; 2024 only"),
  n15 %>% transmute(year, treatment, block, season,
                    outcome = "Soil-derived N uptake", response = soil_N_uptake_kg_ha,
                    source = "direct 15N source allocation; 2024 only")
)

fit_association <- function(metric_dat, metric_name, season_name, outcome_name) {
  y <- outcome_long %>% filter(season == season_name, outcome == outcome_name)
  d <- inner_join(
    metric_dat %>% dplyr::select(year, treatment, block, x = value),
    y %>% dplyr::select(year, treatment, block, response),
    by = c("year", "treatment", "block")
  ) %>%
    filter(is.finite(x), is.finite(response)) %>%
    mutate(
      treatment = factor(treatment, levels = pp_levels),
      block = factor(block, levels = block_levels),
      year_f = factor(year),
      plot_id = interaction(treatment, block, drop = TRUE),
      x_z = as.numeric(scale(x)),
      y_z = as.numeric(scale(response))
    )

  base <- tibble(
    metric = metric_name, season = season_name, N_outcome = outcome_name,
    analysis_family = if_else(outcome_name == "Total N uptake", "primary total-N", "exploratory 15N-source"),
    n = nrow(d), years = paste(sort(unique(d$year)), collapse = ","),
    treatments = paste(sort(unique(as.character(d$treatment))), collapse = ","),
    blocks = paste(sort(unique(as.character(d$block))), collapse = ","),
    beta_overall = NA_real_, SE_overall = NA_real_, lower95_overall = NA_real_, upper95_overall = NA_real_, p_overall = NA_real_,
    beta_treatment_adjusted = NA_real_, SE_treatment_adjusted = NA_real_, lower95_treatment_adjusted = NA_real_, upper95_treatment_adjusted = NA_real_,
    df_treatment_adjusted = NA_real_, partial_R2_treatment_adjusted = NA_real_, p_treatment_adjusted = NA_real_,
    singular_fit = NA, model = NA_character_, status = "not estimable"
  )

  if (nrow(d) < 8 || n_distinct(d$treatment) < 2 || sd(d$x) == 0 || sd(d$response) == 0) return(base)

  tryCatch({
    repeated <- n_distinct(d$year) >= 2
    if (repeated) {
      fit0 <- suppressWarnings(lmerTest::lmer(
        y_z ~ x_z + year_f + block + (1 | plot_id), data = d, REML = FALSE
      ))
      fit1 <- suppressWarnings(lmerTest::lmer(
        y_z ~ x_z + treatment + year_f + block + (1 | plot_id), data = d, REML = FALSE
      ))
      model_label <- "mixed model; year/block adjusted; random plot; treatment-adjusted coefficient is primary"
      singular_flag <- lme4::isSingular(fit1, tol = 1e-5)
    } else {
      fit0 <- lm(y_z ~ x_z + block, data = d)
      fit1 <- lm(y_z ~ x_z + treatment + block, data = d)
      model_label <- "one-year blocked linear model; treatment-adjusted coefficient is primary"
      singular_flag <- FALSE
    }
    a <- extract_coef(fit0)
    b <- extract_coef(fit1)
    crit0 <- ifelse(is.finite(a["df"]), qt(0.975, a["df"]), 1.96)
    crit1 <- ifelse(is.finite(b["df"]), qt(0.975, b["df"]), 1.96)
    partial_r2 <- ifelse(is.finite(b["statistic"]) && is.finite(b["df"]),
                         b["statistic"]^2 / (b["statistic"]^2 + b["df"]), NA_real_)
    base %>% mutate(
      beta_overall = a["beta"], SE_overall = a["SE"],
      lower95_overall = a["beta"] - crit0 * a["SE"], upper95_overall = a["beta"] + crit0 * a["SE"], p_overall = a["p"],
      beta_treatment_adjusted = b["beta"], SE_treatment_adjusted = b["SE"],
      lower95_treatment_adjusted = b["beta"] - crit1 * b["SE"], upper95_treatment_adjusted = b["beta"] + crit1 * b["SE"],
      df_treatment_adjusted = b["df"], partial_R2_treatment_adjusted = partial_r2,
      p_treatment_adjusted = b["p"], singular_fit = singular_flag,
      model = model_label, status = "ok"
    )
  }, error = function(e) base %>% mutate(status = paste("model error:", conditionMessage(e))))
}

association_specs <- bind_rows(
  expand_grid(
    metric = c("SWC", "Kunsat", "SOC", "BulkDensity", "CapillaryPorosity", "WheatMineralN"),
    season = "wheat",
    outcome = c("Total N uptake", "Fertilizer-derived N uptake", "Soil-derived N uptake")
  ),
  expand_grid(
    metric = "RiceMineralN", season = "rice",
    outcome = c("Total N uptake", "Fertilizer-derived N uptake", "Soil-derived N uptake")
  )
)
if (microbe_available && "BacterialPCoA1" %in% names(metric_list)) {
  association_specs <- bind_rows(
    association_specs,
    expand_grid(
      metric = "BacterialPCoA1", season = "wheat",
      outcome = c("Total N uptake", "Fertilizer-derived N uptake", "Soil-derived N uptake")
    )
  )
}

association_results <- bind_rows(lapply(seq_len(nrow(association_specs)), function(i) {
  sp <- association_specs[i, ]
  fit_association(metric_list[[sp$metric]], sp$metric, sp$season, sp$outcome)
})) %>%
  group_by(analysis_family) %>%
  mutate(q_family = p.adjust(p_treatment_adjusted, method = "BH")) %>%
  ungroup() %>%
  mutate(
    q_global = p.adjust(p_treatment_adjusted, method = "BH"),
    interpretation = case_when(
      status != "ok" ~ "Not estimable from matched data",
      q_family < 0.05 & beta_treatment_adjusted > 0 ~ "Significant positive treatment-adjusted association",
      q_family < 0.05 & beta_treatment_adjusted < 0 ~ "Significant negative treatment-adjusted association",
      TRUE ~ "Not significant after BH adjustment"
    )
  )

write.csv(
  association_results,
  file.path(out_dir, "Figure7B_soil_N_associations.csv"),
  row.names = FALSE, fileEncoding = "UTF-8"
)

# ---- 8. Data audit ---------------------------------------------------------
audit_metric <- function(d, nm) {
  tibble(
    variable = nm,
    source_file = basename(soil_file),
    source_sheet = first(d$source_sheet),
    season = first(d$season),
    stage = first(d$stage),
    depth = first(d$depth),
    years = paste(sort(unique(d$year)), collapse = ","),
    treatments = paste(sort(unique(d$treatment)), collapse = ","),
    blocks = paste(sort(unique(d$block)), collapse = ","),
    n_records = nrow(d),
    missing_value_records = sum(!is.finite(d$value)),
    unit = first(d$unit),
    notes = first(d$notes)
  )
}

data_audit <- bind_rows(
  bind_rows(lapply(names(metric_list), function(nm) audit_metric(metric_list[[nm]], nm))),
  tibble(
    variable = c("Seasonal total N uptake", "Seasonal yield", "Seasonal REN", "Seasonal PEN",
                 "Direct fertilizer-derived N uptake", "Direct soil-derived N uptake",
                 "Annual grain yield", "Annual NUE (REN)"),
    source_file = basename(crop_file),
    source_sheet = c(rep("plant_N_plot_all", 4), rep("N15_source", 2), "plant_N_plot_all", "NUE_annual/check calculation"),
    season = c(rep("wheat,rice", 6), "annual", "annual"),
    stage = NA_character_, depth = NA_character_,
    years = c(rep("2020,2021,2022,2023,2024,2025", 4), "2024", "2024",
              "2020,2021,2022,2023,2024,2025", "2020,2021,2022,2023,2024,2025"),
    treatments = c(rep("PP1,PP2,PP3,PP4,PP5,PP6", 4), rep("PP2,PP3,PP4,PP5,PP6", 2),
                   "PP1,PP2,PP3,PP4,PP5,PP6", "PP2,PP3,PP4,PP5,PP6"),
    blocks = c(rep("I,II,III", 4), rep("I,II", 2), "I,II,III", "I,II,III"),
    n_records = c(nrow(crops), nrow(crops), sum(is.finite(crops$REN_pct)), sum(is.finite(crops$PEN_kg_grain_per_kg_N_uptake)),
                  nrow(n15), nrow(n15), nrow(annual_calc), sum(is.finite(annual_calc$annual_REN_pct))),
    missing_value_records = c(
      sum(!is.finite(crops$total_N_uptake_kg_ha)), sum(!is.finite(crops$yield_kg_ha)),
      sum(!is.finite(crops$REN_pct)), sum(!is.finite(crops$PEN_kg_grain_per_kg_N_uptake)),
      sum(!is.finite(n15$fertilizer_N_uptake_kg_ha)), sum(!is.finite(n15$soil_N_uptake_kg_ha)),
      sum(!is.finite(annual_calc$annual_yield_kg_ha)), sum(!is.finite(annual_calc$annual_REN_pct))
    ),
    unit = c("kg N ha-1", "kg ha-1", "%", "kg grain kg-1 N uptake",
             "kg N ha-1", "kg N ha-1", "kg ha-1", "%"),
    notes = c(
      "Three blocks in crop workbook; soil matches use only common blocks",
      "Three blocks in crop workbook", "Calculated against PP1 within year, season and block",
      "Calculated against PP1; mathematical index, not a causal response to REN",
      "Direct 15N allocation only; 2024, PP2-PP6, blocks I-II",
      "Direct 15N allocation only; 2024, PP2-PP6, blocks I-II",
      "Wheat + rice yield in the same year/block",
      "Annual recovery efficiency; checked against workbook ren_pct; not an average of seasonal percentages"
    )
  )
)
write.csv(data_audit, file.path(out_dir, "Figure7B_data_audit.csv"), row.names = FALSE, fileEncoding = "UTF-8")

# ---- 9. Figure -------------------------------------------------------------
# Boxes are layered as requested. Root traits are parallel responses and have
# no forced root -> N uptake arrows. Yield, REN and PEN are peer outcomes.
nodes <- tribble(
  ~id, ~x, ~y, ~w, ~h, ~title, ~subtitle, ~fill,
  "M1", 1.9, 11.55, 3.05, 0.86, "N addition", "PP2 - PP1", "#FFFFFF",
  "M2", 5.7, 11.55, 3.05, 0.86, "Straw return", "PP3 - PP2", "#FFFFFF",
  "M3", 9.7, 11.55, 3.35, 0.86, "Wheat INtS + straw", "PP4 - PP3", "#FFFFFF",
  "M4", 13.9, 11.55, 3.25, 0.86, "Rice rotary direct", "PP5 - PP4", "#FFFFFF",
  "M5", 18.1, 11.55, 3.45, 0.86, "Rice no-till dry direct", "PP4 -> PP6", "#FFFFFF",

  "WheatMineralN", 1.20, 9.15, 2.25, 0.95, "Wheat mineral N", "jointing; 0-40 cm", "#F5DFD1",
  "SOC", 3.65, 9.15, 2.15, 0.95, "SOC", "pre-sowing; 0-10 cm", "#F5DFD1",
  "SWC", 6.05, 9.15, 2.20, 0.95, "Soil moisture", "jointing; 0-10 cm", "#F5DFD1",
  "Kunsat", 8.45, 9.15, 2.10, 0.95, "Kunsat", "jointing; 2024", "#F5DFD1",
  "BulkDensity", 10.80, 9.15, 2.20, 0.95, "Bulk density", "jointing; 0-10 cm", "#F5DFD1",
  "CapillaryPorosity", 13.25, 9.15, 2.30, 0.95, "Capillary porosity", "jointing; 0-10 cm", "#F5DFD1",
  "BacterialCommunity", 15.85, 9.15, 2.40, 0.95, "Bacterial community", "Bray-Curtis; 2024", "#F5DFD1",
  "RiceMineralN", 18.50, 9.15, 2.25, 0.95, "Rice mineral N", "anthesis; 0-10 cm", "#F5DFD1",

  "RootDryWeight", 1.65, 6.80, 2.35, 0.95, "Root dry weight", "wheat; parallel trait", "#DDEBF3",
  "RootActivity", 4.20, 6.80, 2.35, 0.95, "Root activity", "wheat; parallel trait", "#DDEBF3",
  "WheatTotalN", 7.05, 6.80, 2.45, 0.95, "Wheat total N", "2020-2025", "#DDEBF3",
  "WheatFertN", 9.75, 6.80, 2.45, 0.95, "Wheat fertilizer N", "direct 15N; 2024", "#DDEBF3",
  "WheatSoilN", 12.45, 6.80, 2.45, 0.95, "Wheat soil N", "direct 15N; 2024", "#DDEBF3",
  "RiceTotalN", 15.15, 6.80, 2.45, 0.95, "Rice total N", "2020-2025", "#DDEBF3",
  "RiceFertN", 17.85, 6.80, 2.45, 0.95, "Rice fertilizer N", "direct 15N; 2024", "#DDEBF3",
  "RiceSoilN", 20.55, 6.80, 2.45, 0.95, "Rice soil N", "direct 15N; 2024", "#DDEBF3",

  "WheatYield", 2.15, 4.25, 2.45, 0.88, "Wheat yield", "peer outcome", "#DFEFD9",
  "WheatREN", 5.05, 4.25, 2.45, 0.88, "Wheat REN", "calculated vs PP1", "#DFEFD9",
  "WheatPEN", 7.95, 4.25, 2.45, 0.88, "Wheat PEN", "calculated vs PP1", "#DFEFD9",
  "RiceYield", 13.10, 4.25, 2.45, 0.88, "Rice yield", "peer outcome", "#DFEFD9",
  "RiceREN", 16.00, 4.25, 2.45, 0.88, "Rice REN", "calculated vs PP1", "#DFEFD9",
  "RicePEN", 18.90, 4.25, 2.45, 0.88, "Rice PEN", "calculated vs PP1", "#DFEFD9",

  "AnnualYield", 6.75, 1.75, 3.40, 0.95, "Annual grain yield", "wheat + rice", "#F1D7DA",
  "AnnualNUE", 14.10, 1.75, 3.40, 0.95, "Annual NUE (REN)", "annual delta N / annual N input", "#F1D7DA"
)

get_edge_xy <- function(source, target) {
  a <- nodes[nodes$id == source, , drop = FALSE]
  b <- nodes[nodes$id == target, , drop = FALSE]
  if (nrow(a) != 1 || nrow(b) != 1) stop("Unknown edge node: ", source, " -> ", target)
  dx <- b$x - a$x
  dy <- b$y - a$y
  ta <- min(ifelse(abs(dx) < 1e-9, Inf, (a$w / 2) / abs(dx)),
            ifelse(abs(dy) < 1e-9, Inf, (a$h / 2) / abs(dy)))
  tb <- min(ifelse(abs(dx) < 1e-9, Inf, (b$w / 2) / abs(dx)),
            ifelse(abs(dy) < 1e-9, Inf, (b$h / 2) / abs(dy)))
  c(x = a$x + ta * dx, y = a$y + ta * dy,
    xend = b$x - tb * dx, yend = b$y - tb * dy)
}

add_coords <- function(d) {
  if (!nrow(d)) return(d %>% mutate(x = numeric(), y = numeric(), xend = numeric(), yend = numeric(), xmid = numeric(), ymid = numeric()))
  bind_rows(lapply(seq_len(nrow(d)), function(i) {
    xy <- get_edge_xy(d$source[i], d$target[i])
    bind_cols(d[i, , drop = FALSE], as_tibble_row(as.list(xy)))
  })) %>% mutate(xmid = (x + xend) / 2, ymid = (y + yend) / 2)
}

management_node <- setNames(contrast_defs$management_id, contrast_defs$contrast)
metric_node <- c(
  SWC = "SWC", Kunsat = "Kunsat", SOC = "SOC", BulkDensity = "BulkDensity",
  CapillaryPorosity = "CapillaryPorosity", WheatMineralN = "WheatMineralN",
  RiceMineralN = "RiceMineralN", RootDryWeight = "RootDryWeight", RootActivity = "RootActivity",
  WheatTotalN = "WheatTotalN", WheatFertN = "WheatFertN", WheatSoilN = "WheatSoilN",
  RiceTotalN = "RiceTotalN", RiceFertN = "RiceFertN", RiceSoilN = "RiceSoilN",
  WheatYield = "WheatYield", WheatREN = "WheatREN", WheatPEN = "WheatPEN",
  RiceYield = "RiceYield", RiceREN = "RiceREN", RicePEN = "RicePEN",
  AnnualYield = "AnnualYield", AnnualREN = "AnnualNUE"
)

management_edges <- management_results %>%
  filter(scope == "pooled", plot_eligible, is.finite(q_within_metric), q_within_metric < 0.05,
         metric != "BacterialCommunity") %>%
  mutate(
    source = unname(management_node[contrast]),
    target = unname(metric_node[metric]),
    colour = if_else(estimate >= 0, "#2166AC", "#B2182B"),
    linetype = "solid",
    label = sprintf("%+.2f\nq=%.3f", estimate, q_within_metric)
  ) %>%
  filter(!is.na(source), !is.na(target)) %>%
  add_coords()

# A priori soil-N links shown even when non-significant; all other tested links
# remain in the CSV. The primary coefficient adjusts for treatment.
primary_paths <- tribble(
  ~metric, ~season, ~N_outcome, ~source, ~target,
  "WheatMineralN", "wheat", "Total N uptake", "WheatMineralN", "WheatTotalN",
  "SWC", "wheat", "Total N uptake", "SWC", "WheatTotalN",
  "Kunsat", "wheat", "Total N uptake", "Kunsat", "WheatTotalN",
  "CapillaryPorosity", "wheat", "Total N uptake", "CapillaryPorosity", "WheatTotalN",
  "RiceMineralN", "rice", "Total N uptake", "RiceMineralN", "RiceTotalN"
)
association_edges <- primary_paths %>%
  left_join(association_results,
            by = c("metric", "season", "N_outcome")) %>%
  mutate(
    significant = status == "ok" & is.finite(q_family) & q_family < 0.05,
    colour = case_when(
      significant & beta_treatment_adjusted >= 0 ~ "#2166AC",
      significant & beta_treatment_adjusted < 0 ~ "#B2182B",
      TRUE ~ "#A9B0B5"
    ),
    linetype = if_else(significant, "solid", "dashed"),
    label = if_else(status == "ok",
                    sprintf("beta=%+.2f\nq=%.3f", beta_treatment_adjusted, q_family),
                    "not estimable")
  ) %>%
  add_coords()

# Formula-derived annual links are intentionally grey and dashed.
formula_edges <- tribble(
  ~source, ~target, ~label,
  "WheatYield", "AnnualYield", "calculated",
  "RiceYield", "AnnualYield", "calculated",
  "WheatTotalN", "AnnualNUE", "calculated",
  "RiceTotalN", "AnnualNUE", "calculated"
) %>%
  mutate(colour = "#B0B6BA", linetype = "dashed") %>%
  add_coords()

edge_curvature <- function(source_x, target_x) {
  pmax(-0.18, pmin(0.18, (target_x - source_x) / 70))
}
management_edges <- management_edges %>%
  left_join(nodes %>% dplyr::select(id, source_x = x), by = c("source" = "id")) %>%
  left_join(nodes %>% dplyr::select(id, target_x = x), by = c("target" = "id")) %>%
  mutate(curvature = edge_curvature(source_x, target_x))
association_edges <- association_edges %>%
  left_join(nodes %>% dplyr::select(id, source_x = x), by = c("source" = "id")) %>%
  left_join(nodes %>% dplyr::select(id, target_x = x), by = c("target" = "id")) %>%
  mutate(curvature = edge_curvature(source_x, target_x))

p <- ggplot() +
  geom_curve(
    data = management_edges,
    aes(x = x, y = y, xend = xend, yend = yend, colour = colour,
        linetype = linetype),
    linewidth = 0.85, curvature = 0.06,
    arrow = grid::arrow(length = grid::unit(0.13, "cm"), type = "closed"),
    show.legend = FALSE
  ) +
  geom_curve(
    data = association_edges,
    aes(x = x, y = y, xend = xend, yend = yend, colour = colour,
        linetype = linetype),
    linewidth = 0.95, curvature = -0.06,
    arrow = grid::arrow(length = grid::unit(0.14, "cm"), type = "closed"),
    show.legend = FALSE
  ) +
  geom_segment(
    data = formula_edges,
    aes(x = x, y = y, xend = xend, yend = yend),
    colour = "#B0B6BA", linetype = "dashed", linewidth = 0.55,
    arrow = grid::arrow(length = grid::unit(0.11, "cm"), type = "closed")
  ) +
  geom_rect(
    data = nodes,
    aes(xmin = x - w / 2, xmax = x + w / 2, ymin = y - h / 2, ymax = y + h / 2, fill = fill),
    colour = "#232B30", linewidth = 0.55
  ) +
  geom_text(
    data = nodes,
    aes(x = x, y = y + 0.13, label = title),
    fontface = "bold", colour = "#1D252A", size = 3.05, lineheight = 0.92
  ) +
  geom_text(
    data = nodes,
    aes(x = x, y = y - 0.20, label = subtitle),
    colour = "#55636C", size = 2.20, lineheight = 0.90
  ) +
  geom_label(
    data = management_edges,
    aes(x = xmid, y = ymid, label = label),
    size = 2.05, label.size = 0, fill = "white", colour = "#4B5962",
    label.padding = grid::unit(0.05, "lines")
  ) +
  geom_label(
    data = association_edges,
    aes(x = xmid, y = ymid, label = label),
    size = 2.15, label.size = 0, fill = "white", colour = "#37474F",
    label.padding = grid::unit(0.06, "lines")
  ) +
  annotate("text", x = 10.4, y = 5.18,
           label = "Seasonal yield, REN and PEN are peer outcomes; no causal arrows fitted",
           size = 2.65, colour = "#60717B") +
  annotate("segment", x = 13.9, xend = 15.1, y = 0.63, yend = 0.63,
           colour = "#2166AC", linewidth = 0.95) +
  annotate("text", x = 15.3, y = 0.63, label = "Significant positive", hjust = 0, size = 2.35) +
  annotate("segment", x = 17.35, xend = 18.55, y = 0.63, yend = 0.63,
           colour = "#B2182B", linewidth = 0.95) +
  annotate("text", x = 18.75, y = 0.63, label = "Significant negative", hjust = 0, size = 2.35) +
  annotate("segment", x = 7.0, xend = 8.2, y = 0.63, yend = 0.63,
           colour = "#A9B0B5", linetype = "dashed", linewidth = 0.75) +
  annotate("text", x = 8.4, y = 0.63, label = "Non-significant / formula-derived", hjust = 0, size = 2.35) +
  annotate("text", x = 0.35, y = 0.18,
           label = "Associations, not causal paths. Solid paths: BH q < 0.05. Soil-N coefficients adjust for treatment, year and block. 15N source data: 2024 only.",
           hjust = 0, size = 2.35, colour = "#5E6D76") +
  scale_colour_identity() +
  scale_fill_identity() +
  scale_linetype_identity() +
  coord_cartesian(xlim = c(0.0, 21.9), ylim = c(0.0, 12.4), expand = FALSE, clip = "off") +
  labs(tag = "B") +
  theme_void(base_size = 10) +
  theme(
    plot.tag = element_text(face = "bold", size = 18),
    plot.margin = margin(12, 18, 14, 12)
  )

ggsave(
  file.path(out_dir, "Figure7B_final.png"), p,
  width = 20, height = 11.5, units = "in", dpi = 450,
  device = ragg::agg_png, bg = "white", limitsize = FALSE
)
ggsave(
  file.path(out_dir, "Figure7B_final.tiff"), p,
  width = 20, height = 11.5, units = "in", dpi = 600,
  device = ragg::agg_tiff, compression = "lzw", bg = "white", limitsize = FALSE
)
pdf_tmp <- file.path(tempdir(), "Figure7B_final.pdf")
ggsave(
  pdf_tmp, p,
  width = 20, height = 11.5, units = "in",
  device = grDevices::cairo_pdf, bg = "white", limitsize = FALSE
)
if (!file.copy(pdf_tmp, file.path(out_dir, "Figure7B_final.pdf"), overwrite = TRUE)) {
  stop("Could not copy the vector PDF into the output directory: ", out_dir)
}

write.csv(management_edges, file.path(out_dir, "Figure7B_plotted_management_edges.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(association_edges, file.path(out_dir, "Figure7B_plotted_soil_N_edges.csv"), row.names = FALSE, fileEncoding = "UTF-8")

# ---- 10. Statistical audit and interpretation ------------------------------
sig_mgmt <- management_results %>%
  filter(scope == "pooled", plot_eligible, q_within_metric < 0.05, metric != "BacterialCommunity") %>%
  arrange(q_within_metric)
sig_assoc <- association_results %>%
  filter(status == "ok", q_family < 0.05) %>%
  arrange(q_family)
heterogeneous_metrics <- management_results %>%
  filter(scope == "pooled", is.finite(interaction_p), interaction_p < 0.05) %>%
  distinct(metric) %>%
  pull(metric)
singular_management_metrics <- management_results %>%
  filter(scope == "pooled", singular_fit %in% TRUE) %>%
  distinct(metric) %>%
  pull(metric)

max_yield_diff <- max(abs(annual_check$yield_difference), na.rm = TRUE)
max_N_diff <- max(abs(annual_check$N_difference), na.rm = TRUE)
max_REN_diff <- max(abs(annual_check$REN_difference), na.rm = TRUE)

summary_lines <- c(
  "FIGURE 7B STATISTICAL AND DATA AUDIT",
  "=====================================",
  paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  paste0("Soil/plant workbook: ", soil_file),
  paste0("Crop/N workbook: ", crop_file),
  paste0("Microbial data: ", microbe_note),
  "",
  "ANALYSIS DESIGN",
  "- Five planned transitions (effect = second minus first): PP1->PP2, PP2->PP3, PP3->PP4, PP4->PP5, PP4->PP6.",
  "- Repeated-year variables: mixed model with treatment, year and block fixed, permanent plot random intercept.",
  "- One-year variables: blocked linear model.",
  "- Treatment x year heterogeneity was tested; a pooled arrow is suppressed when P < 0.05.",
  "- Management P values are BH-adjusted within each metric across the five planned contrasts.",
  "- Soil-N paths use only matching year x treatment x block records.",
  "- The displayed soil-N coefficient is standardized and adjusted for treatment, year and block; repeated plots use a random intercept.",
  "- Soil-N P values are BH-adjusted separately for primary total-N and exploratory 15N-source families.",
  "- Bacterial composition uses Bray-Curtis pairwise PERMANOVA with permutations restricted within block; R2 is unsigned and therefore not plotted as a positive/negative arrow.",
  "",
  "FORMULA CHECKS",
  "- Seasonal REN = 100 x (treatment total N uptake - PP1 total N uptake) / seasonal N applied, within year/season/block.",
  "- Seasonal PEN = (treatment yield - PP1 yield) / (treatment total N uptake - PP1 total N uptake), within year/season/block.",
  "- Annual grain yield is wheat + rice yield in the same year/treatment/block.",
  "- Annual NUE is annual REN calculated from annual totals, not a sum or mean of seasonal percentages.",
  sprintf("- Maximum absolute check difference versus workbook annual yield: %.8f", max_yield_diff),
  sprintf("- Maximum absolute check difference versus workbook annual N uptake: %.8f", max_N_diff),
  sprintf("- Maximum absolute check difference versus workbook annual REN: %.8f", max_REN_diff),
  "",
  "SAMPLE-SCOPE LIMITATIONS",
  "- Crop total N/yield data: 2020-2025, three blocks.",
  "- Most soil variables: two blocks; associations use only common blocks I-II.",
  "- Direct fertilizer-derived and soil-derived crop N: 2024 only, PP2-PP6, blocks I-II (n = 10 per season).",
  "- Kunsat, capillary porosity and bacterial community composition are one-year (2024) measurements.",
  "- Rice anthesis 0-10 cm mineral N has no 2024 record, so it cannot be paired with 2024 rice 15N-source uptake.",
  "- Root dry weight and root activity are parallel responses; no root-to-N mediation path is imposed.",
  "- Path arrows represent associations and planned contrasts, not verified causality.",
  "",
  paste0("Significant pooled management arrows plotted: ", nrow(sig_mgmt)),
  paste0("Significant BH-adjusted soil-N associations: ", nrow(sig_assoc)),
  paste0("Pooled arrows suppressed for treatment-by-year heterogeneity: ",
         ifelse(length(heterogeneous_metrics), paste(heterogeneous_metrics, collapse = ", "), "none")),
  paste0("Metrics with near-zero random-intercept variance (singular boundary): ",
         ifelse(length(singular_management_metrics), paste(singular_management_metrics, collapse = ", "), "none")),
  "- A singular boundary means the fitted plot-level variance was approximately zero; the model still retained the repeated-plot structure and the condition is recorded in the CSV."
)

if (nrow(sig_mgmt)) {
  summary_lines <- c(summary_lines, "", "SIGNIFICANT POOLED MANAGEMENT CONTRASTS")
  for (i in seq_len(nrow(sig_mgmt))) {
    z <- sig_mgmt[i, ]
    summary_lines <- c(summary_lines, sprintf(
      "- %s -> %s: estimate = %.4f, 95%% CI [%.4f, %.4f], q = %.4g, years = %d, n = %d",
      z$contrast, z$metric, z$estimate, z$lower95, z$upper95,
      z$q_within_metric, z$n_years, z$n_obs
    ))
  }
}
if (nrow(sig_assoc)) {
  summary_lines <- c(summary_lines, "", "SIGNIFICANT SOIL-N ASSOCIATIONS")
  for (i in seq_len(nrow(sig_assoc))) {
    z <- sig_assoc[i, ]
    summary_lines <- c(summary_lines, sprintf(
      "- %s -> %s %s: adjusted beta = %.3f, 95%% CI [%.3f, %.3f], q = %.4g, years = %s, n = %d",
      z$metric, z$season, z$N_outcome, z$beta_treatment_adjusted,
      z$lower95_treatment_adjusted, z$upper95_treatment_adjusted,
      z$q_family, z$years, z$n
    ))
  }
}
writeLines(summary_lines, file.path(out_dir, "Figure7B_statistics_summary.txt"), useBytes = TRUE)

english_mgmt <- if (nrow(sig_mgmt)) {
  paste(vapply(seq_len(nrow(sig_mgmt)), function(i) {
    z <- sig_mgmt[i, ]
    sprintf("%s changed %s (estimate %.2f, 95%% CI %.2f to %.2f, q = %.3f)",
            z$contrast, z$metric, z$estimate, z$lower95, z$upper95, z$q_within_metric)
  }, character(1)), collapse = "; ")
} else {
  "None of the pooled planned management contrasts met the within-metric BH-adjusted q < 0.05 threshold"
}

english_assoc <- if (nrow(sig_assoc)) {
  paste(vapply(seq_len(nrow(sig_assoc)), function(i) {
    z <- sig_assoc[i, ]
    sprintf("%s was associated with %s %s after treatment adjustment (beta = %.2f, q = %.3f)",
            z$metric, z$season, tolower(z$N_outcome), z$beta_treatment_adjusted, z$q_family)
  }, character(1)), collapse = "; ")
} else {
  "No soil indicator retained a BH-adjusted treatment-controlled association with N uptake"
}
english_heterogeneity <- if (length(heterogeneous_metrics)) {
  paste0(
    paste(heterogeneous_metrics, collapse = ", "),
    " showed treatment-by-year heterogeneity, so pooled arrows were suppressed and year-specific contrasts are reported in the statistical table"
  )
} else {
  "No treatment-by-year heterogeneity required suppression of a pooled arrow"
}

interpretation <- c(
  "# Figure 7B interpretation",
  "",
  "## English Results draft",
  "",
  paste0(
    "Figure 7B summarizes planned management contrasts and year-matched associations rather than a causal structural equation model. ",
    english_mgmt, ". ", english_assoc, ". ", english_heterogeneity, "."
  ),
  "",
  paste0(
    "Total crop N uptake was available for 2020-2025, whereas direct fertilizer-derived and soil-derived N uptake was available only for 2024 (PP2-PP6; two blocks). ",
    "Accordingly, source-specific associations are exploratory and are not extrapolated to years without 15N observations. Wheat and rice yield, REN and PEN are shown as parallel seasonal outcomes. ",
    "Annual grain yield and annual REN are parallel system outcomes calculated from same-year wheat and rice totals."
  ),
  "",
  "These results support only the associations printed as solid paths. Grey dashed links are non-significant hypotheses or formula-derived aggregation links. The diagram therefore does not establish that a soil property caused N uptake, yield or NUE.",
  "",
  "## 中文解释",
  "",
  paste0(
    "本图是基于相同年份、处理和区组匹配数据的关联图，不是因果结构方程模型。管理措施之间采用五组预设相邻对比；多年数据用重复小区混合模型，单年数据用区组校正线性模型。",
    "土壤指标与吸氮量的主结果为控制处理、年份和区组后的标准化关联，并对同一分析族进行 BH 校正。",
    ifelse(length(heterogeneous_metrics),
           paste0("其中 ", paste(heterogeneous_metrics, collapse = "、"), " 存在显著处理×年份异质性，因此未绘制合并年份的管理箭头，分年度结果保留在统计表中。"),
           "未发现需要抑制合并年份箭头的处理×年份异质性。")
  ),
  "",
  paste0(
    "作物总吸氮量覆盖 2020-2025 年，但肥料来源氮和土壤来源氮的直接 15N 分配数据仅覆盖 2024 年 PP2-PP6 的两个区组，因此来源分配关系只能作为小样本探索结果。",
    "水稻扬花期 0-10 cm 矿质氮没有 2024 年记录，不能与 2024 年水稻 15N 来源吸氮量强行配对。"
  ),
  "",
  "根系干重和根系活力在图中作为并列响应变量，没有被设定为土壤物理性质影响吸氮量的必经中介。小麦和水稻的产量、REN、PEN也作为并列结果；周年产量与周年 REN 分别由同一年两季数据计算，不存在“产量导致 NUE”的箭头。"
)
writeLines(interpretation, file.path(out_dir, "Figure7B_interpretation.md"), useBytes = TRUE)

message("Figure 7B workflow completed successfully. Outputs: ", out_dir)

# ---- 11. Main-text integrated pathway diagram -----------------------------
# The detailed audit network above retains every tested a-priori path.  The
# main-text figure below is intentionally more compact.  It displays only
# BH-supported planned management contrasts whose direction was stable across
# all measured years (or a direct one-year 15N contrast), plus clearly marked
# non-causal evidence/aggregation links.  This rule prevents a heterogeneous
# pooled mean from being presented as a universal pathway.

get_mgmt <- function(metric_name, contrast_name) {
  z <- management_results %>%
    filter(metric == metric_name, contrast == contrast_name, scope == "pooled")
  if (nrow(z) != 1) stop("Expected one pooled contrast for ", metric_name, " / ", contrast_name)
  z
}

direction_stable <- function(metric_name, contrast_name) {
  pooled <- get_mgmt(metric_name, contrast_name)
  if (!is.finite(pooled$interaction_p) || pooled$interaction_p >= 0.05) return(TRUE)
  yy <- management_results %>%
    filter(metric == metric_name, contrast == contrast_name, scope == "year-specific")
  nrow(yy) > 0 && all(sign(yy$estimate) == sign(pooled$estimate))
}

robust_required <- tribble(
  ~metric, ~contrast,
  "WheatMineralN", "N addition",
  "WheatTotalN", "N addition",
  "RiceTotalN", "N addition",
  "AnnualREN", "Straw return",
  "SWC", "Wheat INtS + straw mode",
  "WheatMineralN", "Wheat INtS + straw mode",
  "WheatTotalN", "Wheat INtS + straw mode",
  "WheatYield", "Wheat INtS + straw mode",
  "WheatREN", "Wheat INtS + straw mode",
  "AnnualYield", "Wheat INtS + straw mode",
  "RiceFertN", "Rice rotary direct seeding",
  "AnnualYield", "Rice rotary direct seeding",
  "AnnualREN", "Rice rotary direct seeding",
  "WheatMineralN", "Rice no-till dry direct seeding vs PP4",
  "RiceFertN", "Rice no-till dry direct seeding vs PP4"
)
for (i in seq_len(nrow(robust_required))) {
  z <- get_mgmt(robust_required$metric[i], robust_required$contrast[i])
  if (!is.finite(z$q_within_metric) || z$q_within_metric >= 0.05 ||
      !direction_stable(robust_required$metric[i], robust_required$contrast[i])) {
    stop("A pre-specified main-text path failed the significance/direction-stability gate: ",
         robust_required$metric[i], " / ", robust_required$contrast[i])
  }
}

season_cor <- function(season_name, variable) {
  d <- crops %>%
    filter(season == season_name, treatment != "PP1") %>%
    dplyr::select(yield_kg_ha, all_of(variable)) %>%
    filter(if_all(everything(), is.finite))
  ct <- cor.test(d$yield_kg_ha, d[[variable]], method = "pearson")
  c(r = unname(ct$estimate), p = ct$p.value)
}
w_ren_cor <- season_cor("wheat", "REN_pct")
w_pen_cor <- season_cor("wheat", "PEN_kg_grain_per_kg_N_uptake")
r_ren_cor <- season_cor("rice", "REN_pct")
r_pen_cor <- season_cor("rice", "PEN_kg_grain_per_kg_N_uptake")

n15_source_relation <- function(season_name) {
  d <- n15 %>% filter(season == season_name) %>%
    group_by(treatment) %>%
    summarise(
      fertilizer_N = mean(fertilizer_N_uptake_kg_ha),
      soil_N = mean(soil_N_uptake_kg_ha),
      .groups = "drop"
    )
  fit <- lm(soil_N ~ fertilizer_N, data = d)
  c(R2 = summary(fit)$r.squared,
    p = coef(summary(fit))["fertilizer_N", "Pr(>|t|)"])
}
w_source <- n15_source_relation("wheat")
r_source <- n15_source_relation("rice")
pp4_annual_yield <- annual_calc %>%
  filter(treatment == "PP4") %>%
  summarise(value = mean(annual_yield_kg_ha)) %>% pull(value)

fmt_q <- function(x) ifelse(x < 0.001, "<0.001", sprintf("=%.3f", x))
est <- function(metric_name, contrast_name) get_mgmt(metric_name, contrast_name)$estimate
qq <- function(metric_name, contrast_name) get_mgmt(metric_name, contrast_name)$q_within_metric

main_nodes <- tribble(
  ~id, ~x, ~y, ~w, ~h, ~title, ~subtitle, ~fill,
  "M1", 1.85, 10.15, 3.20, 1.05, "N addition", "PP2 - PP1", "#FFFFFF",
  "M2", 1.85, 8.25, 3.20, 1.05, "Straw return", "PP3 - PP2", "#FFFFFF",
  "M3", 1.85, 6.35, 3.20, 1.05, "Wheat INtS +\nsurface straw", "PP4 - PP3", "#FFFFFF",
  "M4", 1.85, 4.45, 3.20, 1.05, "Rice wet direct\nseeding", "PP5 - PP4", "#FFFFFF",
  "M5", 1.85, 2.55, 3.20, 1.05, "Rice no-till dry\ndirect seeding", "PP4 -> PP6", "#FFFFFF",

  "ENV", 6.10, 7.75, 4.85, 2.55, "Wheat root-zone environment",
  "SWC increased; Kunsat and capillary porosity\nwere near the BH threshold (q = 0.055-0.062)\nResidual mineral N did not increase under INtS",
  "#F4DFD2",
  "WN", 10.75, 7.75, 3.75, 2.55, "Wheat N acquisition",
  sprintf("Total N uptake, 2020-2025\n2024 15N: fertilizer- and soil-derived N covaried\nR2 = %.2f, P = %.3f", w_source["R2"], w_source["p"]),
  "#DCEAF2",
  "WP", 14.85, 7.75, 3.75, 2.30, "Wheat yield and NUE",
  sprintf("Yield-REN: r = %.2f, P < 0.001\nYield-PEN: r = %.2f, P = %.3f", w_ren_cor["r"], w_pen_cor["r"], w_pen_cor["p"]),
  "#DDEED8",

  "RN", 10.75, 3.45, 3.75, 2.55, "Rice N acquisition",
  sprintf("Total N uptake, 2020-2025\n2024: soil-derived N = 69-81%%; fertilizer-soil\nsource relation R2 = %.2f, P = %.3f", r_source["R2"], r_source["p"]),
  "#DCEAF2",
  "RP", 14.85, 3.45, 3.75, 2.30, "Rice yield and NUE",
  sprintf("Yield-REN: r = %.2f, P < 0.001\nYield-PEN: r = %.2f, P = %.3f", r_ren_cor["r"], r_pen_cor["r"], r_pen_cor["p"]),
  "#DDEED8",
  "ANNUAL", 18.75, 5.60, 3.60, 2.90, "Annual grain yield and NUE",
  sprintf("PP4 had the highest mean annual yield\n(%.0f kg ha-1)\nAnnual NUE = annual REN from same-year\nwheat + rice N totals", pp4_annual_yield),
  "#F1D7DA"
)

main_xy <- function(source, target) {
  a <- main_nodes[main_nodes$id == source, , drop = FALSE]
  b <- main_nodes[main_nodes$id == target, , drop = FALSE]
  dx <- b$x - a$x; dy <- b$y - a$y
  ta <- min(ifelse(abs(dx) < 1e-9, Inf, (a$w / 2) / abs(dx)),
            ifelse(abs(dy) < 1e-9, Inf, (a$h / 2) / abs(dy)))
  tb <- min(ifelse(abs(dx) < 1e-9, Inf, (b$w / 2) / abs(dx)),
            ifelse(abs(dy) < 1e-9, Inf, (b$h / 2) / abs(dy)))
  c(x = a$x + ta * dx, y = a$y + ta * dy,
    xend = b$x - tb * dx, yend = b$y - tb * dy)
}

add_main_xy <- function(d) {
  bind_rows(lapply(seq_len(nrow(d)), function(i) {
    bind_cols(d[i, , drop = FALSE], as_tibble_row(as.list(main_xy(d$source[i], d$target[i]))))
  }))
}

main_mgmt_edges <- tribble(
  ~source, ~target, ~label, ~colour, ~curvature, ~label_x, ~label_y,
  "M1", "WN", sprintf("Total N %+.1f kg ha-1\nq%s dagger", est("WheatTotalN", "N addition"), fmt_q(qq("WheatTotalN", "N addition"))), "#2166AC", 0.20, 6.65, 9.55,
  "M1", "RN", sprintf("Total N %+.1f kg ha-1\nq%s dagger", est("RiceTotalN", "N addition"), fmt_q(qq("RiceTotalN", "N addition"))), "#2166AC", -0.28, 6.25, 4.95,
  "M2", "ANNUAL", sprintf("Annual REN %+.2f pp\nq%s dagger", est("AnnualREN", "Straw return"), fmt_q(qq("AnnualREN", "Straw return"))), "#B2182B", 0.24, 9.10, 10.05,
  "M3", "ENV", sprintf("SWC %+.2f pp (q%s dagger)\nMineral N %+.2f (q%s)", est("SWC", "Wheat INtS + straw mode"), fmt_q(qq("SWC", "Wheat INtS + straw mode")), est("WheatMineralN", "Wheat INtS + straw mode"), fmt_q(qq("WheatMineralN", "Wheat INtS + straw mode"))), "#6A51A3", -0.10, 3.95, 7.30,
  "M3", "WN", sprintf("Total N %+.1f kg ha-1\nq%s dagger", est("WheatTotalN", "Wheat INtS + straw mode"), fmt_q(qq("WheatTotalN", "Wheat INtS + straw mode"))), "#2166AC", 0.10, 6.95, 6.35,
  "M3", "WP", sprintf("Yield %+.0f kg ha-1\nREN %+.2f pp; q <= 0.006 dagger", est("WheatYield", "Wheat INtS + straw mode"), est("WheatREN", "Wheat INtS + straw mode")), "#2166AC", 0.17, 10.65, 8.95,
  "M4", "RN", sprintf("Fertilizer N %+.2f kg ha-1\nq%s (2024)", est("RiceFertN", "Rice rotary direct seeding"), fmt_q(qq("RiceFertN", "Rice rotary direct seeding"))), "#B2182B", -0.10, 6.70, 3.95,
  "M4", "ANNUAL", sprintf("Yield %+.0f kg ha-1\nAnnual REN %+.2f pp; q <= 0.013 dagger", est("AnnualYield", "Rice rotary direct seeding"), est("AnnualREN", "Rice rotary direct seeding")), "#B2182B", -0.20, 10.60, 2.00
) %>% add_main_xy()

main_evidence_edges <- tribble(
  ~source, ~target, ~label, ~curvature, ~label_x, ~label_y,
  "ENV", "WN", "Treatment-adjusted matched-year\nassociations: all BH q >= 0.620", 0.00, 8.45, 8.45,
  "WN", "WP", "Association / shared calculation\nno causal coefficient fitted", 0.00, 12.80, 8.45,
  "RN", "RP", "Association / shared calculation\nno causal coefficient fitted", 0.00, 12.80, 4.15,
  "WP", "ANNUAL", "same-year aggregation", 0.08, 17.05, 7.45,
  "RP", "ANNUAL", "same-year aggregation", -0.08, 17.05, 3.80
) %>% add_main_xy() %>%
  mutate(colour = "#9EA8AE", linetype = "dashed")

label_dagger <- function(x) str_replace_all(x, "dagger", "\u2020")
main_mgmt_edges$label <- label_dagger(main_mgmt_edges$label)

p_main <- ggplot() +
  geom_curve(
    data = main_evidence_edges,
    aes(x = x, y = y, xend = xend, yend = yend, group = interaction(source, target)),
    colour = "#9EA8AE", linetype = "dashed", linewidth = 0.70,
    curvature = 0.02,
    arrow = grid::arrow(length = grid::unit(0.12, "cm"), type = "closed")
  ) +
  geom_curve(
    data = main_mgmt_edges,
    aes(x = x, y = y, xend = xend, yend = yend, colour = colour,
        group = interaction(source, target)),
    linewidth = 0.90, curvature = 0.10,
    arrow = grid::arrow(length = grid::unit(0.14, "cm"), type = "closed")
  ) +
  geom_rect(
    data = main_nodes,
    aes(xmin = x - w / 2, xmax = x + w / 2, ymin = y - h / 2, ymax = y + h / 2, fill = fill),
    colour = "#273238", linewidth = 0.58
  ) +
  geom_text(
    data = main_nodes,
    aes(x = x, y = y + h * 0.23, label = title),
    fontface = "bold", colour = "#1E282D", size = 3.25, lineheight = 0.92
  ) +
  geom_text(
    data = main_nodes,
    aes(x = x, y = y - h * 0.13, label = subtitle),
    colour = "#50616A", size = 2.32, lineheight = 0.93
  ) +
  geom_label(
    data = main_mgmt_edges,
    aes(x = label_x, y = label_y, label = label),
    size = 2.12, label.size = 0, fill = "white", colour = "#37474F",
    label.padding = grid::unit(0.06, "lines")
  ) +
  geom_label(
    data = main_evidence_edges,
    aes(x = label_x, y = label_y, label = label),
    size = 2.02, label.size = 0, fill = "white", colour = "#6A777E",
    label.padding = grid::unit(0.05, "lines")
  ) +
  annotate("text", x = 1.85, y = 11.10, label = "Adjacent management contrasts",
           fontface = "bold", size = 3.35, colour = "#263238") +
  annotate("segment", x = 5.25, xend = 6.35, y = 0.72, yend = 0.72,
           colour = "#2166AC", linewidth = 0.90) +
  annotate("text", x = 6.55, y = 0.72, label = "BH-supported positive contrast", hjust = 0, size = 2.25) +
  annotate("segment", x = 10.05, xend = 11.15, y = 0.72, yend = 0.72,
           colour = "#B2182B", linewidth = 0.90) +
  annotate("text", x = 11.35, y = 0.72, label = "BH-supported negative contrast", hjust = 0, size = 2.25) +
  annotate("segment", x = 15.10, xend = 16.20, y = 0.72, yend = 0.72,
           colour = "#9EA8AE", linetype = "dashed", linewidth = 0.70) +
  annotate("text", x = 16.40, y = 0.72, label = "non-causal / unsupported link", hjust = 0, size = 2.25) +
  annotate("text", x = 0.25, y = 0.18,
           label = "\u2020 Treatment x year interaction was significant, but the displayed contrast retained the same direction in every measured year. Paths are evidence links, not a fitted causal SEM.",
           hjust = 0, size = 2.18, colour = "#596970") +
  scale_colour_identity() + scale_fill_identity() +
  coord_cartesian(xlim = c(0, 20.8), ylim = c(0, 11.55), expand = FALSE, clip = "off") +
  theme_void(base_size = 10) +
  theme(plot.margin = margin(12, 18, 14, 12))

# Publication layout: panel A reports the direct management evidence without
# long crossing arrows; panel B shows the seasonal pathway and makes the
# unsupported soil-to-N step visually explicit.  This replaces the drafting
# layout above while using the same computed statistics and audit gates.
pub_nodes <- tribble(
  ~id, ~x, ~y, ~w, ~h, ~title, ~subtitle, ~fill,
  "A1", 3.25, 10.15, 6.10, 1.35, "M1  N addition  PP1 -> PP2",
  sprintf("Wheat mineral N %+.2f mg kg-1 (q%s)\nWheat total N %+.1f; rice total N %+.1f kg N ha-1\nBoth crop-N contrasts: q < 0.001\u2020",
          est("WheatMineralN", "N addition"), fmt_q(qq("WheatMineralN", "N addition")),
          est("WheatTotalN", "N addition"), est("RiceTotalN", "N addition")), "#E8F1F7",
  "A2", 3.25, 8.35, 6.10, 1.35, "M2  Straw return  PP2 -> PP3",
  sprintf("Annual REN %+.2f percentage points (q%s\u2020)\nNo sign-consistent positive adjacent-treatment pathway", 
          est("AnnualREN", "Straw return"), fmt_q(qq("AnnualREN", "Straw return"))), "#F8E6E7",
  "A3", 3.25, 6.30, 6.10, 1.90, "M3  Wheat INtS + surface straw  PP3 -> PP4",
  sprintf("SWC %+.2f points (q%s\u2020)\nResidual mineral N %+.2f mg kg-1 (q%s)\nWheat total N %+.1f kg N ha-1 (q%s\u2020)\nWheat yield %+.0f; annual yield %+.0f kg ha-1 (q <= 0.007\u2020)\nWheat REN %+.2f points (q = 0.006\u2020)",
          est("SWC", "Wheat INtS + straw mode"), fmt_q(qq("SWC", "Wheat INtS + straw mode")),
          est("WheatMineralN", "Wheat INtS + straw mode"), fmt_q(qq("WheatMineralN", "Wheat INtS + straw mode")),
          est("WheatTotalN", "Wheat INtS + straw mode"), fmt_q(qq("WheatTotalN", "Wheat INtS + straw mode")),
          est("WheatYield", "Wheat INtS + straw mode"), est("AnnualYield", "Wheat INtS + straw mode"),
          est("WheatREN", "Wheat INtS + straw mode")), "#E8F1F7",
  "A4", 3.25, 4.15, 6.10, 1.55, "M4  Rice wet direct seeding  PP4 -> PP5",
  sprintf("Rice fertilizer N %+.2f kg N ha-1 (q%s; 2024)\nAnnual yield %+.0f kg ha-1\nAnnual REN %+.2f points (q <= 0.013\u2020)",
          est("RiceFertN", "Rice rotary direct seeding"), fmt_q(qq("RiceFertN", "Rice rotary direct seeding")),
          est("AnnualYield", "Rice rotary direct seeding"), est("AnnualREN", "Rice rotary direct seeding")), "#F8E6E7",
  "A5", 3.25, 2.10, 6.10, 1.45, "M5  Rice no-till dry direct seeding  PP4 -> PP6",
  sprintf("Wheat residual mineral N %+.2f mg kg-1 (q%s)\nRice fertilizer N %+.2f kg N ha-1 (q%s; 2024)\nNo stable annual yield or annual NUE contrast",
          est("WheatMineralN", "Rice no-till dry direct seeding vs PP4"),
          fmt_q(qq("WheatMineralN", "Rice no-till dry direct seeding vs PP4")),
          est("RiceFertN", "Rice no-till dry direct seeding vs PP4"),
          fmt_q(qq("RiceFertN", "Rice no-till dry direct seeding vs PP4"))), "#F2F3F4",

  "ENV2", 9.15, 8.50, 4.25, 2.65, "Wheat root-zone environment",
  "M3 increased SWC\nResidual mineral N decreased\nKunsat and capillary porosity were positive\nbut just above the BH threshold (q = 0.055-0.062)", "#F4DFD2",
  "WN2", 13.75, 8.50, 4.10, 2.65, "Wheat N acquisition",
  sprintf("M1 and M3 increased total N uptake\n2024 15N: fertilizer- and soil-derived N covaried\nR2 = %.2f, P = %.3f", w_source["R2"], w_source["p"]), "#DCEAF2",
  "WP2", 18.15, 8.50, 3.80, 2.40, "Wheat yield and NUE",
  sprintf("Yield-REN: r = %.2f, P < 0.001\nYield-PEN: r = %.2f, P = %.3f\nM3 increased yield and REN", w_ren_cor["r"], w_pen_cor["r"], w_pen_cor["p"]), "#DDEED8",
  "RN2", 13.75, 3.70, 4.10, 2.65, "Rice N acquisition",
  sprintf("M1 increased total N uptake\nM4 reduced fertilizer N uptake in 2024\nSoil-derived N = 69-81%%\nFertilizer-soil source relation R2 = %.2f", r_source["R2"]), "#DCEAF2",
  "RP2", 18.15, 3.70, 3.80, 2.40, "Rice yield and NUE",
  sprintf("Yield-REN: r = %.2f, P < 0.001\nYield-PEN: r = %.2f, P = %.3f\nREN, not PEN, tracked yield", r_ren_cor["r"], r_pen_cor["r"], r_pen_cor["p"]), "#DDEED8",
  "AN2", 22.35, 6.10, 3.65, 3.05, "Annual grain yield and NUE",
  sprintf("PP4 had the highest mean annual yield\n%.0f kg ha-1\nAnnual NUE = annual REN\nfrom same-year wheat + rice N totals", pp4_annual_yield), "#F1D7DA"
)

pub_xy <- function(source, target) {
  a <- pub_nodes[pub_nodes$id == source, , drop = FALSE]
  b <- pub_nodes[pub_nodes$id == target, , drop = FALSE]
  dx <- b$x - a$x; dy <- b$y - a$y
  ta <- min(ifelse(abs(dx) < 1e-9, Inf, (a$w / 2) / abs(dx)),
            ifelse(abs(dy) < 1e-9, Inf, (a$h / 2) / abs(dy)))
  tb <- min(ifelse(abs(dx) < 1e-9, Inf, (b$w / 2) / abs(dx)),
            ifelse(abs(dy) < 1e-9, Inf, (b$h / 2) / abs(dy)))
  c(x = a$x + ta * dx, y = a$y + ta * dy,
    xend = b$x - tb * dx, yend = b$y - tb * dy)
}

pub_edges <- tribble(
  ~source, ~target, ~label, ~label_x, ~label_y,
  "ENV2", "WN2", "Matched year x treatment x block\nall treatment-adjusted BH q >= 0.620", 11.45, 10.12,
  "WN2", "WP2", "Association only\nno causal coefficient fitted", 15.95, 10.12,
  "RN2", "RP2", "Association only\nno causal coefficient fitted", 15.95, 5.30,
  "WP2", "AN2", "same-year aggregation", 20.35, 8.10,
  "RP2", "AN2", "same-year aggregation", 20.35, 4.05
) %>%
  bind_cols(bind_rows(lapply(seq_len(nrow(.)), function(i) {
    as_tibble_row(as.list(pub_xy(.$source[i], .$target[i])))
  })))

p_first_layout <- ggplot() +
  geom_curve(
    data = pub_edges,
    aes(x = x, y = y, xend = xend, yend = yend, group = interaction(source, target)),
    colour = "#9EA8AE", linetype = "dashed", linewidth = 0.78,
    curvature = 0.02,
    arrow = grid::arrow(length = grid::unit(0.13, "cm"), type = "closed")
  ) +
  geom_rect(
    data = pub_nodes,
    aes(xmin = x - w / 2, xmax = x + w / 2, ymin = y - h / 2, ymax = y + h / 2, fill = fill),
    colour = "#273238", linewidth = 0.58
  ) +
  geom_text(
    data = pub_nodes,
    aes(x = x, y = y + h * 0.23, label = title),
    fontface = "bold", colour = "#1E282D", size = 3.15, lineheight = 0.92
  ) +
  geom_text(
    data = pub_nodes,
    aes(x = x, y = y - h * 0.13, label = subtitle),
    colour = "#50616A", size = 2.28, lineheight = 0.93
  ) +
  geom_label(
    data = pub_edges,
    aes(x = label_x, y = label_y, label = label),
    size = 2.03, label.size = 0, fill = "white", colour = "#6A777E",
    label.padding = grid::unit(0.05, "lines")
  ) +
  annotate("text", x = 0.25, y = 11.25, label = "A", fontface = "bold", size = 5.2, hjust = 0) +
  annotate("text", x = 0.70, y = 11.25, label = "Management contrasts and direct responses", fontface = "bold", size = 3.5, hjust = 0) +
  annotate("text", x = 7.20, y = 11.25, label = "B", fontface = "bold", size = 5.2, hjust = 0) +
  annotate("text", x = 7.65, y = 11.25, label = "Integrated seasonal pathway", fontface = "bold", size = 3.5, hjust = 0) +
  annotate("segment", x = 8.25, xend = 9.35, y = 0.72, yend = 0.72,
           colour = "#9EA8AE", linetype = "dashed", linewidth = 0.75) +
  annotate("text", x = 9.55, y = 0.72,
           label = "non-causal, statistically unsupported, or formula-derived link",
           hjust = 0, size = 2.25) +
  annotate("text", x = 0.25, y = 0.18,
           label = "\u2020 Treatment x year interaction was significant, but the displayed contrast retained the same direction in every measured year. Management effects are planned PP contrasts with within-variable BH q < 0.05. This is not a fitted causal SEM.",
           hjust = 0, size = 2.15, colour = "#596970") +
  scale_fill_identity() +
  coord_cartesian(xlim = c(0, 24.40), ylim = c(0, 11.65), expand = FALSE, clip = "off") +
  theme_void(base_size = 10) +
  theme(plot.margin = margin(12, 18, 14, 12))

# ---- 12. Compact coefficient-and-path layout -------------------------------
# This is the publication-facing rendering.  It deliberately follows the
# visual grammar of a path diagram (indicator nodes, labelled lines, minimal
# prose), while retaining the inferential safeguards above.  It is not a
# fitted causal SEM: management-line labels are standardized planned
# contrasts (Delta z), matched-year soil-to-N lines are adjusted standardized
# coefficients (beta), and within-season outcome lines are correlations (r).

all_metric_data <- c(metric_list, crop_metric_list)
response_sd <- bind_rows(lapply(names(all_metric_data), function(nm) {
  tibble(metric = nm, response_sd = sd(all_metric_data[[nm]]$value, na.rm = TRUE))
}))

q_stars <- function(q) {
  case_when(
    is.na(q) ~ "",
    q < 0.001 ~ "***",
    q < 0.01 ~ "**",
    q < 0.05 ~ "*",
    TRUE ~ ""
  )
}
p_stars <- function(p) {
  case_when(
    is.na(p) ~ "",
    p < 0.001 ~ "***",
    p < 0.01 ~ "**",
    p < 0.05 ~ "*",
    TRUE ~ " ns"
  )
}

compact_groups <- tribble(
  ~id, ~x, ~y, ~w, ~h, ~title, ~fill,
  "ROOT",  3.85, 7.45, 3.35, 3.35, "Wheat root-zone traits", "#F4E4D7",
  "WN",    7.75, 7.45, 3.35, 3.35, "Wheat N uptake",         "#DDECF4",
  "WO",   11.65, 7.45, 3.35, 3.35, "Wheat performance",      "#DDECF4",
  "RN",    7.75, 2.95, 3.35, 3.35, "Rice N uptake",          "#E3EFD9",
  "RO",   11.65, 2.95, 3.35, 3.35, "Rice performance",       "#E3EFD9",
  "AN",   16.55, 5.20, 3.25, 3.15, "Annual performance",     "#F2DEE2"
)

compact_nodes <- tribble(
  ~id, ~x, ~y, ~w, ~h, ~title, ~subtitle, ~fill, ~kind,
  "M1",  2.15, 10.55, 2.75, 0.82, "N addition",           "PP1 -> PP2", "#F3F4F5", "management",
  "M2",  6.05, 10.55, 2.75, 0.82, "Straw return",         "PP2 -> PP3", "#F3F4F5", "management",
  "M3", 10.05, 10.55, 2.75, 0.82, "Wheat INtS + straw", "PP3 -> PP4", "#F3F4F5", "management",
  "M4", 14.05, 10.55, 2.75, 0.82, "Rice wet-direct",      "PP4 -> PP5", "#F3F4F5", "management",
  "M5", 17.85, 10.55, 2.75, 0.82, "Rice no-till dry",     "PP4 -> PP6", "#ECEFF1", "management",

  "SWC",               3.15, 8.15, 1.30, 0.70, "SWC",          "", "#FFFDFB", "indicator",
  "Kunsat",            4.55, 8.15, 1.30, 0.70, "SIR",       "", "#FFFDFB", "indicator",
  "CapillaryPorosity", 3.15, 6.82, 1.30, 0.70, "Capillary\nporosity", "", "#FFFDFB", "indicator",
  "WheatMineralN",     4.55, 6.82, 1.30, 0.70, "Mineral N",    "", "#FFFDFB", "indicator",

  "WheatTotalN", 7.75, 8.15, 2.70, 0.70, "Total N",       "", "#F9FCFE", "indicator",
  "WheatFertN",  7.05, 6.82, 1.30, 0.70, "Fertilizer N",  "", "#F9FCFE", "indicator",
  "WheatSoilN",  8.45, 6.82, 1.30, 0.70, "Soil N",        "", "#F9FCFE", "indicator",

  "WheatYield", 11.65, 8.15, 2.70, 0.70, "Yield",          "", "#FBFDF9", "indicator",
  "WheatREN",   10.95, 6.82, 1.30, 0.70, "REN",            "", "#FBFDF9", "indicator",
  "WheatPEN",   12.35, 6.82, 1.30, 0.70, "PEN",            "", "#FBFDF9", "indicator",

  "RiceTotalN", 7.75, 3.65, 2.70, 0.70, "Total N",         "", "#F9FCFE", "indicator",
  "RiceFertN",  7.05, 2.32, 1.30, 0.70, "Fertilizer N",    "", "#F9FCFE", "indicator",
  "RiceSoilN",  8.45, 2.32, 1.30, 0.70, "Soil N",          "", "#F9FCFE", "indicator",

  "RiceYield", 11.65, 3.65, 2.70, 0.70, "Yield",            "", "#FBFDF9", "indicator",
  "RiceREN",   10.95, 2.32, 1.30, 0.70, "REN",              "", "#FBFDF9", "indicator",
  "RicePEN",   12.35, 2.32, 1.30, 0.70, "PEN",              "", "#FBFDF9", "indicator",

  "AnnualYield", 16.55, 5.88, 2.65, 0.76, "Annual yield",    "", "#FFF9FA", "indicator",
  "AnnualNUE",   16.55, 4.50, 2.65, 0.76, "Annual REN",      "", "#FFF9FA", "indicator"
) %>%
  mutate(fill = if_else(kind == "indicator", "#FFFFFF", fill))

compact_xy <- function(source, target) {
  a <- compact_nodes[compact_nodes$id == source, , drop = FALSE]
  b <- compact_nodes[compact_nodes$id == target, , drop = FALSE]
  if (nrow(a) != 1 || nrow(b) != 1) stop("Unknown compact node: ", source, " -> ", target)
  dx <- b$x - a$x; dy <- b$y - a$y
  ta <- min(ifelse(abs(dx) < 1e-9, Inf, (a$w / 2) / abs(dx)),
            ifelse(abs(dy) < 1e-9, Inf, (a$h / 2) / abs(dy)))
  tb <- min(ifelse(abs(dx) < 1e-9, Inf, (b$w / 2) / abs(dx)),
            ifelse(abs(dy) < 1e-9, Inf, (b$h / 2) / abs(dy)))
  c(x = a$x + ta * dx, y = a$y + ta * dy,
    xend = b$x - tb * dx, yend = b$y - tb * dy)
}
add_compact_xy <- function(d) {
  bind_rows(lapply(seq_len(nrow(d)), function(i) {
    bind_cols(d[i, , drop = FALSE], as_tibble_row(as.list(compact_xy(d$source[i], d$target[i]))))
  }))
}

compact_management_specs <- tribble(
  ~source, ~target, ~metric, ~contrast, ~edge_class, ~curvature, ~label_x, ~label_y,
  "M3", "SWC",               "SWC",               "Wheat INtS + straw mode", "supported",  0.10,  2.55, 9.28,
  "M3", "Kunsat",            "Kunsat",            "Wheat INtS + straw mode", "trend",     -0.10,  3.65, 9.42,
  "M3", "CapillaryPorosity", "CapillaryPorosity", "Wheat INtS + straw mode", "trend",      0.18,  2.58, 8.42,
  "M3", "WheatMineralN",     "WheatMineralN",     "Wheat INtS + straw mode", "supported", -0.15,  4.12, 8.55,
  "M3", "WheatTotalN",       "WheatTotalN",       "Wheat INtS + straw mode", "supported", -0.18,  5.15, 9.55,
  "M3", "WheatYield",        "WheatYield",        "Wheat INtS + straw mode", "supported", -0.22,  7.62, 9.62,
  "M3", "WheatREN",          "WheatREN",          "Wheat INtS + straw mode", "supported",  0.18,  9.20, 8.98,
  "M1", "WheatMineralN",     "WheatMineralN",     "N addition",              "supported",  0.18,  5.38, 9.02,
  "M1", "WheatTotalN",       "WheatTotalN",       "N addition",              "supported", -0.05,  6.70, 9.33,
  "M1", "RiceTotalN",        "RiceTotalN",        "N addition",              "supported",  0.08,  6.55, 5.55,
  "M2", "AnnualNUE",         "AnnualREN",         "Straw return",             "supported", -0.10, 17.75, 7.55,
  "M4", "RiceFertN",         "RiceFertN",         "Rice rotary direct seeding", "supported", 0.10, 11.12, 5.40,
  "M4", "AnnualYield",       "AnnualYield",       "Rice rotary direct seeding", "supported", 0.10, 15.30, 8.32,
  "M4", "AnnualNUE",         "AnnualREN",         "Rice rotary direct seeding", "supported", -0.10, 15.50, 7.35,
  "M5", "WheatMineralN",     "WheatMineralN",     "Rice no-till dry direct seeding vs PP4", "supported", -0.12, 7.05, 8.55,
  "M5", "RiceFertN",         "RiceFertN",         "Rice no-till dry direct seeding vs PP4", "supported", 0.16, 8.55, 5.10
)

compact_management_edges <- compact_management_specs %>%
  left_join(
    management_results %>%
      filter(scope == "pooled") %>%
      dplyr::select(metric, contrast, estimate, q_within_metric, interaction_p, year, n_years),
    by = c("metric", "contrast")
  ) %>%
  left_join(response_sd, by = "metric") %>%
  mutate(
    delta_z = estimate / response_sd,
    direction_mark = if_else(is.finite(interaction_p) & interaction_p < 0.05 & n_years > 1, "\u2020", ""),
    one_year_mark = if_else(n_years == 1, " [24]", ""),
    label = if_else(
      edge_class == "trend",
      sprintf("%+.2f\nq=%.3f", delta_z, q_within_metric),
      sprintf("%+.2f%s%s%s", delta_z, q_stars(q_within_metric), direction_mark, one_year_mark)
    ),
    colour = case_when(
      edge_class == "trend" ~ "#D98C00",
      delta_z >= 0 ~ "#C92532",
      TRUE ~ "#2166AC"
    ),
    linetype = if_else(edge_class == "trend", "dotted", "solid"),
    linewidth = if_else(edge_class == "trend", 0.72, 1.05)
  ) %>%
  add_compact_xy()

get_assoc_row <- function(metric_name) {
  z <- association_results %>%
    filter(metric == metric_name, season == "wheat", N_outcome == "Total N uptake")
  if (nrow(z) != 1) stop("Expected one matched-year association for ", metric_name)
  z
}
compact_assoc_specs <- tribble(
  ~source, ~target, ~metric, ~curvature, ~label_x, ~label_y,
  "SWC",               "WheatTotalN", "SWC",                0.02, 5.55, 8.42,
  "Kunsat",            "WheatTotalN", "Kunsat",            -0.06, 6.12, 8.03,
  "CapillaryPorosity", "WheatTotalN", "CapillaryPorosity",  0.06, 5.58, 7.45,
  "WheatMineralN",     "WheatTotalN", "WheatMineralN",     -0.02, 6.15, 6.98
)
compact_assoc_edges <- bind_rows(lapply(seq_len(nrow(compact_assoc_specs)), function(i) {
  sp <- compact_assoc_specs[i, ]
  ar <- get_assoc_row(sp$metric)
  bind_cols(sp, ar %>% dplyr::select(beta_treatment_adjusted, q_family, years))
})) %>%
  mutate(label = sprintf("%+.2f ns", beta_treatment_adjusted)) %>%
  add_compact_xy()

compact_formula_edges <- tribble(
  ~source, ~target, ~label, ~curvature, ~label_x, ~label_y,
  "WheatYield",  "AnnualYield", "same-year sum",  0.08, 14.20, 7.20,
  "RiceYield",   "AnnualYield", "same-year sum", -0.08, 14.18, 4.28,
  "WheatTotalN", "AnnualNUE",   "same-year REN", -0.12, 13.75, 5.72,
  "RiceTotalN",  "AnnualNUE",   "same-year REN",  0.12, 13.78, 3.67
) %>% add_compact_xy()

compact_relation_edges <- tribble(
  ~source, ~target, ~label, ~significant, ~curvature, ~label_x, ~label_y,
  "WheatFertN", "WheatSoilN", sprintf("R2=%.2f%s", w_source["R2"], p_stars(w_source["p"])), TRUE,  -0.35, 7.75, 7.30,
  "RiceFertN",  "RiceSoilN",  sprintf("R2=%.2f%s", r_source["R2"], p_stars(r_source["p"])), FALSE, -0.35, 7.75, 3.10,
  "WheatYield", "WheatREN",   sprintf("r=%.2f%s", w_ren_cor["r"], p_stars(w_ren_cor["p"])), TRUE,   0.18, 10.86, 7.55,
  "WheatYield", "WheatPEN",   sprintf("r=%.2f%s", w_pen_cor["r"], p_stars(w_pen_cor["p"])), FALSE, -0.18, 12.42, 7.55,
  "RiceYield",  "RiceREN",    sprintf("r=%.2f%s", r_ren_cor["r"], p_stars(r_ren_cor["p"])), TRUE,    0.18, 10.86, 3.05,
  "RiceYield",  "RicePEN",    sprintf("r=%.2f%s", r_pen_cor["r"], p_stars(r_pen_cor["p"])), FALSE,  -0.18, 12.42, 3.05
) %>%
  mutate(
    colour = if_else(significant, "#C92532", "#A3ABB0"),
    linetype = if_else(significant, "solid", "dashed")
  ) %>% add_compact_xy()

p_compact_layout <- ggplot() +
  geom_rect(
    data = compact_groups,
    aes(xmin = x - w / 2, xmax = x + w / 2, ymin = y - h / 2, ymax = y + h / 2, fill = fill),
    colour = "#3F4A50", linewidth = 0.70
  ) +
  geom_curve(
    data = compact_formula_edges,
    aes(x = x, y = y, xend = xend, yend = yend, group = interaction(source, target)),
    colour = "#A3ABB0", linetype = "dashed", linewidth = 0.62,
    curvature = 0.08,
    arrow = grid::arrow(length = grid::unit(0.11, "cm"), type = "closed")
  ) +
  geom_curve(
    data = compact_assoc_edges,
    aes(x = x, y = y, xend = xend, yend = yend, group = interaction(source, target)),
    colour = "#A3ABB0", linetype = "dashed", linewidth = 0.64,
    curvature = 0.02,
    arrow = grid::arrow(length = grid::unit(0.11, "cm"), type = "closed")
  ) +
  geom_curve(
    data = compact_relation_edges,
    aes(x = x, y = y, xend = xend, yend = yend, colour = colour, linetype = linetype,
        group = interaction(source, target)),
    linewidth = 0.82, curvature = 0.12, show.legend = FALSE
  ) +
  geom_curve(
    data = compact_management_edges,
    aes(x = x, y = y, xend = xend, yend = yend, colour = colour,
        linetype = linetype, linewidth = linewidth, group = interaction(source, target)),
    curvature = 0.08,
    arrow = grid::arrow(length = grid::unit(0.13, "cm"), type = "closed"),
    show.legend = FALSE
  ) +
  geom_rect(
    data = compact_nodes %>% filter(kind == "management"),
    aes(xmin = x - w / 2, xmax = x + w / 2, ymin = y - h / 2, ymax = y + h / 2, fill = fill),
    colour = "#263238", linewidth = 0.65
  ) +
  geom_rect(
    data = compact_nodes %>% filter(kind == "indicator"),
    aes(xmin = x - w / 2, xmax = x + w / 2, ymin = y - h / 2, ymax = y + h / 2),
    fill = scales::alpha("#FFFFFF", 0.62),
    colour = "#263238", linewidth = 0.65
  ) +
  geom_label(
    data = compact_groups,
    aes(x = x, y = y - h / 2 + 0.24, label = title, fill = fill),
    fontface = "bold", size = 4.10, colour = "#263238",
    label.size = 0, label.padding = grid::unit(0.055, "lines"),
    show.legend = FALSE
  ) +
  geom_text(
    data = compact_nodes %>% filter(kind == "indicator"),
    aes(x = x, y = y, label = title),
    fontface = "bold", size = 3.82, lineheight = 0.90, colour = "#263238"
  ) +
  geom_text(
    data = compact_nodes %>% filter(kind == "management"),
    aes(x = x, y = y + 0.12, label = title),
    fontface = "bold", size = 3.95, colour = "#263238"
  ) +
  geom_text(
    data = compact_nodes %>% filter(kind == "management"),
    aes(x = x, y = y - 0.20, label = subtitle),
    size = 3.15, colour = "#5E6A70"
  ) +
  geom_label(
    data = compact_management_edges,
    aes(x = label_x, y = label_y, label = label, colour = colour),
    fill = "white", label.size = 0, size = 3.12, fontface = "bold",
    label.padding = grid::unit(0.05, "lines"), show.legend = FALSE
  ) +
  geom_label(
    data = compact_assoc_edges,
    aes(x = label_x, y = label_y, label = label),
    fill = "white", label.size = 0, size = 2.85, colour = "#737F85",
    label.padding = grid::unit(0.045, "lines")
  ) +
  geom_label(
    data = compact_relation_edges,
    aes(x = label_x, y = label_y, label = label, colour = colour),
    fill = "white", label.size = 0, size = 3.00, fontface = "bold",
    label.padding = grid::unit(0.045, "lines"), show.legend = FALSE
  ) +
  geom_label(
    data = compact_formula_edges,
    aes(x = label_x, y = label_y, label = label),
    fill = "white", label.size = 0, size = 2.76, colour = "#778288",
    label.padding = grid::unit(0.045, "lines")
  ) +
  annotate("text", x = 0.35, y = 11.30, label = "Management", fontface = "bold", size = 4.55, hjust = 0) +
  annotate("segment", x = 0.55, xend = 1.55, y = 0.66, yend = 0.66, colour = "#C92532", linewidth = 1.00) +
  annotate("text", x = 1.72, y = 0.66, label = "positive", hjust = 0, size = 3.12) +
  annotate("segment", x = 3.05, xend = 4.05, y = 0.66, yend = 0.66, colour = "#2166AC", linewidth = 1.00) +
  annotate("text", x = 4.22, y = 0.66, label = "negative", hjust = 0, size = 3.12) +
  annotate("segment", x = 5.55, xend = 6.55, y = 0.66, yend = 0.66, colour = "#D98C00", linetype = "dotted", linewidth = 0.80) +
  annotate("text", x = 6.72, y = 0.66, label = "0.05 <= q < 0.10", hjust = 0, size = 3.12) +
  annotate("segment", x = 9.55, xend = 10.55, y = 0.66, yend = 0.66, colour = "#A3ABB0", linetype = "dashed", linewidth = 0.72) +
  annotate("text", x = 10.72, y = 0.66, label = "non-significant / aggregation", hjust = 0, size = 3.12) +
  annotate("text", x = 19.25, y = 0.66, label = "line labels: Delta z, beta, r or R2", hjust = 1, size = 2.96, colour = "#55636A") +
  annotate("text", x = 0.35, y = 0.16,
           label = "* q or P < 0.05; ** < 0.01; *** < 0.001.  dagger: significant treatment x year interaction but the same sign in every measured year.  [24]: 2024 only.  Association diagram, not causal SEM.",
           hjust = 0, size = 2.78, colour = "#596970") +
  scale_colour_identity() + scale_fill_identity() + scale_linetype_identity() + scale_linewidth_identity() +
  coord_cartesian(xlim = c(0, 19.60), ylim = c(0, 11.60), expand = FALSE, clip = "off") +
  theme_void(base_family = "Arial", base_size = 13) +
  theme(plot.margin = margin(12, 18, 14, 12), text = element_text(colour = "#263238"))

# ---- 13. Nature-style two-panel publication layout ------------------------
# Panel a keeps the five planned treatment transitions in experimental order
# and places each supported response directly below its parent contrast. This
# removes crossing management arrows. Panel b retains the integrated seasonal
# evidence pathway. All text is sized for a 183-mm-wide final figure.

nature_management <- tribble(
  ~id, ~x, ~y, ~w, ~h, ~title, ~transition,
  "M1",  1.50, 10.72, 2.45, 0.72, "N addition",         "PP1 -> PP2",
  "M2",  4.35, 10.72, 2.45, 0.72, "Straw return",       "PP2 -> PP3",
  "M3",  8.60, 10.72, 3.05, 0.72, "Wheat INtS + straw", "PP3 -> PP4",
  "M4", 13.35, 10.72, 2.75, 0.72, "Rice wet-direct",    "PP4 -> PP5",
  "M5", 17.05, 10.72, 2.75, 0.72, "Rice no-till dry",   "PP4 -> PP6"
)

nature_response_specs <- tribble(
  ~source, ~metric, ~contrast, ~display, ~x, ~y, ~w, ~h, ~edge_class,
  "M1", "WheatMineralN", "N addition", "Wheat mineral N", 1.50, 9.55, 2.35, 0.62, "supported",
  "M1", "WheatTotalN",   "N addition", "Wheat total N",   1.50, 8.72, 2.35, 0.62, "supported",
  "M1", "RiceTotalN",    "N addition", "Rice total N",    1.50, 7.89, 2.35, 0.62, "supported",

  "M2", "AnnualREN", "Straw return", "Annual NUE", 4.35, 8.90, 2.25, 0.62, "supported",

  "M3", "SWC",               "Wheat INtS + straw mode", "SWC",                7.42, 9.55, 2.12, 0.62, "supported",
  "M3", "Kunsat",            "Wheat INtS + straw mode", "Kunsat",             7.42, 8.72, 2.12, 0.62, "trend",
  "M3", "CapillaryPorosity", "Wheat INtS + straw mode", "Capillary porosity", 7.42, 7.89, 2.12, 0.62, "trend",
  "M3", "WheatMineralN",     "Wheat INtS + straw mode", "Mineral N",          7.42, 7.06, 2.12, 0.62, "supported",
  "M3", "WheatTotalN",       "Wheat INtS + straw mode", "Total N",            9.78, 9.55, 2.12, 0.62, "supported",
  "M3", "WheatYield",        "Wheat INtS + straw mode", "Wheat yield",        9.78, 8.72, 2.12, 0.62, "supported",
  "M3", "WheatREN",          "Wheat INtS + straw mode", "Wheat REN",          9.78, 7.89, 2.12, 0.62, "supported",

  "M4", "RiceFertN",   "Rice rotary direct seeding", "Rice fertilizer N", 13.35, 9.55, 2.60, 0.62, "supported",
  "M4", "AnnualYield", "Rice rotary direct seeding", "Annual yield",      13.35, 8.72, 2.60, 0.62, "supported",
  "M4", "AnnualREN",   "Rice rotary direct seeding", "Annual NUE",        13.35, 7.89, 2.60, 0.62, "supported",

  "M5", "WheatMineralN", "Rice no-till dry direct seeding vs PP4", "Wheat mineral N",   17.05, 9.35, 2.60, 0.62, "supported",
  "M5", "RiceFertN",     "Rice no-till dry direct seeding vs PP4", "Rice fertilizer N", 17.05, 8.45, 2.60, 0.62, "supported"
)

nature_responses <- nature_response_specs %>%
  left_join(
    management_results %>%
      filter(scope == "pooled") %>%
      dplyr::select(metric, contrast, estimate, q_within_metric, interaction_p, n_years),
    by = c("metric", "contrast")
  ) %>%
  left_join(response_sd, by = "metric") %>%
  left_join(nature_management %>% dplyr::select(source = id, source_x = x, source_y = y, source_h = h), by = "source") %>%
  mutate(
    delta_z = estimate / response_sd,
    direction_mark = if_else(is.finite(interaction_p) & interaction_p < 0.05 & n_years > 1, "\u2020", ""),
    year_mark = if_else(n_years == 1, " [24]", ""),
    effect_text = if_else(
      edge_class == "trend",
      sprintf("%+.2f; q=%.3f", delta_z, q_within_metric),
      sprintf("%+.2f%s%s%s", delta_z, q_stars(q_within_metric), direction_mark, year_mark)
    ),
    target = display,
    label = paste(display, effect_text, sep = "\n"),
    colour = case_when(
      edge_class == "trend" ~ "#E69F00",
      delta_z >= 0 ~ "#D55E00",
      TRUE ~ "#0072B2"
    ),
    fill = case_when(
      edge_class == "trend" ~ "#FFF7DF",
      delta_z >= 0 ~ "#FFF1EA",
      TRUE ~ "#EBF5FA"
    ),
    linetype = if_else(edge_class == "trend", "dotted", "solid"),
    x0 = source_x, y0 = source_y - source_h / 2,
    x1 = x, y1 = y + h / 2
  )

nature_groups <- tribble(
  ~id, ~x, ~y, ~w, ~h, ~title, ~fill,
  "ROOT",  2.10, 3.55, 3.35, 2.35, "Wheat root-zone traits", "#FFF8F3",
  "WN",    6.10, 3.55, 3.20, 2.35, "Wheat N uptake",        "#F3F8FB",
  "WO",   10.05, 3.55, 3.20, 2.35, "Wheat performance",     "#F5F9F1",
  "RN",    6.10, 1.18, 3.20, 1.70, "Rice N uptake",         "#F3F8FB",
  "RO",   10.05, 1.18, 3.20, 1.70, "Rice performance",      "#F5F9F1",
  "AN",   15.25, 2.55, 3.10, 2.40, "Annual performance",    "#FBF4F5"
)

abeta <- function(metric_name) get_assoc_row(metric_name)$beta_treatment_adjusted
nature_nodes <- tribble(
  ~id, ~x, ~y, ~w, ~h, ~title, ~stat,
  "SWC",               1.38, 3.83, 1.22, 0.68, "SWC",          sprintf("beta=%+.2f ns", abeta("SWC")),
  "Kunsat",            2.82, 3.83, 1.22, 0.68, "SIR",       sprintf("beta=%+.2f ns", abeta("Kunsat")),
  "CapillaryPorosity", 1.38, 2.98, 1.22, 0.68, "Cap. porosity",sprintf("beta=%+.2f ns", abeta("CapillaryPorosity")),
  "WheatMineralN",     2.82, 2.98, 1.22, 0.68, "Mineral N",    sprintf("beta=%+.2f ns", abeta("WheatMineralN")),

  "WheatTotalN", 6.10, 3.83, 2.55, 0.68, "Total N",      "",
  "WheatFertN",  5.42, 2.98, 1.22, 0.68, "Fertilizer N", "",
  "WheatSoilN",  6.78, 2.98, 1.22, 0.68, "Soil N",       "",

  "WheatYield", 10.05, 3.83, 2.55, 0.68, "Yield", "",
  "WheatREN",    9.37, 2.98, 1.22, 0.68, "REN", sprintf("r=%.2f%s", w_ren_cor["r"], p_stars(w_ren_cor["p"])),
  "WheatPEN",   10.73, 2.98, 1.22, 0.68, "PEN", sprintf("r=%.2f%s", w_pen_cor["r"], p_stars(w_pen_cor["p"])),

  "RiceTotalN", 6.10, 1.33, 2.55, 0.60, "Total N",      "",
  "RiceFertN",  5.42, 0.68, 1.22, 0.60, "Fertilizer N", "",
  "RiceSoilN",  6.78, 0.68, 1.22, 0.60, "Soil N",       "",

  "RiceYield", 10.05, 1.33, 2.55, 0.60, "Yield", "",
  "RiceREN",    9.37, 0.68, 1.22, 0.60, "REN", sprintf("r=%.2f%s", r_ren_cor["r"], p_stars(r_ren_cor["p"])),
  "RicePEN",   10.73, 0.68, 1.22, 0.60, "PEN", sprintf("r=%.2f%s", r_pen_cor["r"], p_stars(r_pen_cor["p"])),

  "AnnualYield", 15.25, 2.95, 2.55, 0.68, "Annual yield", "",
  "AnnualNUE",   15.25, 2.10, 2.55, 0.68, "Annual REN",   ""
)

nature_xy <- function(source, target) {
  a <- nature_nodes[nature_nodes$id == source, , drop = FALSE]
  b <- nature_nodes[nature_nodes$id == target, , drop = FALSE]
  if (nrow(a) != 1 || nrow(b) != 1) stop("Unknown Nature-layout node: ", source, " -> ", target)
  dx <- b$x - a$x; dy <- b$y - a$y
  ta <- min(ifelse(abs(dx) < 1e-9, Inf, (a$w / 2) / abs(dx)),
            ifelse(abs(dy) < 1e-9, Inf, (a$h / 2) / abs(dy)))
  tb <- min(ifelse(abs(dx) < 1e-9, Inf, (b$w / 2) / abs(dx)),
            ifelse(abs(dy) < 1e-9, Inf, (b$h / 2) / abs(dy)))
  c(x = a$x + ta * dx, y = a$y + ta * dy,
    xend = b$x - tb * dx, yend = b$y - tb * dy)
}
add_nature_xy <- function(d) {
  bind_rows(lapply(seq_len(nrow(d)), function(i) {
    bind_cols(d[i, , drop = FALSE], as_tibble_row(as.list(nature_xy(d$source[i], d$target[i]))))
  }))
}

nature_root_edges <- tribble(
  ~source, ~target,
  "SWC", "WheatTotalN",
  "Kunsat", "WheatTotalN",
  "CapillaryPorosity", "WheatTotalN",
  "WheatMineralN", "WheatTotalN"
) %>% add_nature_xy()

nature_relation_edges <- tribble(
  ~source, ~target, ~label, ~significant, ~label_x, ~label_y,
  "WheatFertN", "WheatSoilN", sprintf("R2=%.2f%s", w_source["R2"], p_stars(w_source["p"])), TRUE,  6.10, 2.56,
  "RiceFertN",  "RiceSoilN",  sprintf("R2=%.2f%s", r_source["R2"], p_stars(r_source["p"])), FALSE, 6.10, 0.31,
  "WheatYield", "WheatREN",   "", TRUE,  NA_real_, NA_real_,
  "WheatYield", "WheatPEN",   "", FALSE, NA_real_, NA_real_,
  "RiceYield",  "RiceREN",    "", TRUE,  NA_real_, NA_real_,
  "RiceYield",  "RicePEN",    "", FALSE, NA_real_, NA_real_
) %>%
  mutate(
    colour = if_else(significant, "#D55E00", "#9AA2A8"),
    linetype = if_else(significant, "solid", "dashed")
  ) %>% add_nature_xy()

nature_evidence_edges <- tribble(
  ~source, ~target, ~evidence_type,
  "WheatTotalN", "WheatYield", "association only",
  "RiceTotalN",  "RiceYield",  "association only",
  "WheatYield",  "AnnualYield", "same-year aggregation",
  "RiceYield",   "AnnualYield", "same-year aggregation",
  "WheatTotalN", "AnnualNUE",   "same-year calculation",
  "RiceTotalN",  "AnnualNUE",   "same-year calculation"
) %>% add_nature_xy()

# Nature Portfolio-friendly specifications: colour-blind-safe red/blue pair,
# Arial/sans-serif text, no decorative shadows, and <=1-pt strokes at final size.
update_geom_defaults("text", list(family = "Arial"))
update_geom_defaults("label", list(family = "Arial"))

p_main <- ggplot() +
  # Panel divider and light module containers.
  annotate("segment", x = 0.35, xend = 18.65, y = 6.28, yend = 6.28,
           colour = "#D5D9DC", linewidth = 0.25) +
  geom_rect(
    data = nature_groups,
    aes(xmin = x - w / 2, xmax = x + w / 2, ymin = y - h / 2, ymax = y + h / 2, fill = fill),
    colour = "#50585D", linewidth = 0.25
  ) +
  # Conceptual, non-significant and aggregation links sit behind nodes.
  geom_segment(
    data = nature_root_edges,
    aes(x = x, y = y, xend = xend, yend = yend),
    colour = "#9AA2A8", linetype = "dashed", linewidth = 0.28,
    arrow = grid::arrow(length = grid::unit(0.07, "cm"), type = "closed")
  ) +
  geom_curve(
    data = nature_evidence_edges,
    aes(x = x, y = y, xend = xend, yend = yend, group = interaction(source, target)),
    colour = "#9AA2A8", linetype = "dashed", linewidth = 0.28, curvature = 0.06,
    arrow = grid::arrow(length = grid::unit(0.07, "cm"), type = "closed")
  ) +
  geom_curve(
    data = nature_relation_edges,
    aes(x = x, y = y, xend = xend, yend = yend, colour = colour,
        linetype = linetype, group = interaction(source, target)),
    linewidth = 0.31, curvature = 0.10, show.legend = FALSE
  ) +
  # Panel a: within-column response arrows eliminate crossovers.
  geom_segment(
    data = nature_responses,
    aes(x = x0, y = y0, xend = x1, yend = y1, colour = colour, linetype = linetype),
    linewidth = 0.34,
    arrow = grid::arrow(length = grid::unit(0.075, "cm"), type = "closed"),
    show.legend = FALSE
  ) +
  geom_rect(
    data = nature_management,
    aes(xmin = x - w / 2, xmax = x + w / 2, ymin = y - h / 2, ymax = y + h / 2),
    fill = "#F7F7F7", colour = "#30383D", linewidth = 0.28
  ) +
  geom_rect(
    data = nature_responses,
    aes(xmin = x - w / 2, xmax = x + w / 2, ymin = y - h / 2, ymax = y + h / 2,
        fill = fill, colour = colour, linetype = linetype),
    linewidth = 0.28, show.legend = FALSE
  ) +
  # Panel b nodes.
  geom_rect(
    data = nature_nodes,
    aes(xmin = x - w / 2, xmax = x + w / 2, ymin = y - h / 2, ymax = y + h / 2),
    fill = "white", colour = "#30383D", linewidth = 0.25
  ) +
  geom_text(
    data = nature_management,
    aes(x = x, y = y + 0.12, label = title),
    fontface = "bold", size = 2.10, colour = "#263238"
  ) +
  geom_text(
    data = nature_management,
    aes(x = x, y = y - 0.17, label = transition),
    size = 1.82, colour = "#5E686E"
  ) +
  geom_text(
    data = nature_responses,
    aes(x = x, y = y, label = label, colour = colour),
    fontface = "bold", size = 1.83, lineheight = 0.90, show.legend = FALSE
  ) +
  geom_text(
    data = nature_groups,
    aes(x = x, y = y + h / 2 - 0.23, label = title),
    fontface = "bold", size = 2.02, colour = "#30383D"
  ) +
  geom_text(
    data = nature_nodes %>% filter(stat == ""),
    aes(x = x, y = y, label = title),
    fontface = "bold", size = 1.92, colour = "#30383D"
  ) +
  geom_text(
    data = nature_nodes %>% filter(stat != ""),
    aes(x = x, y = y + 0.12, label = title),
    fontface = "bold", size = 1.82, colour = "#30383D"
  ) +
  geom_text(
    data = nature_nodes %>% filter(stat != ""),
    aes(x = x, y = y - 0.15, label = stat),
    size = 1.78, colour = "#68737A"
  ) +
  geom_label(
    data = nature_relation_edges %>% filter(label != ""),
    aes(x = label_x, y = label_y, label = label, colour = colour),
    fill = "white", label.size = 0, size = 1.78, fontface = "bold",
    label.padding = grid::unit(0.02, "lines"), show.legend = FALSE
  ) +
  # Nature-style panel letters and concise in-figure key.
  annotate("text", x = 0.28, y = 11.55, label = "a", fontface = "bold", size = 2.82, hjust = 0) +
  annotate("text", x = 0.68, y = 11.55, label = "Management contrasts and direct responses",
           fontface = "bold", size = 2.32, hjust = 0) +
  annotate("text", x = 0.28, y = 5.91, label = "b", fontface = "bold", size = 2.82, hjust = 0) +
  annotate("text", x = 0.68, y = 5.91, label = "Integrated seasonal pathway",
           fontface = "bold", size = 2.32, hjust = 0) +
  annotate("segment", x = 5.00, xend = 5.70, y = 5.48, yend = 5.48, colour = "#D55E00", linewidth = 0.34) +
  annotate("text", x = 5.82, y = 5.48, label = "positive", hjust = 0, size = 1.78) +
  annotate("segment", x = 7.15, xend = 7.85, y = 5.48, yend = 5.48, colour = "#0072B2", linewidth = 0.34) +
  annotate("text", x = 7.97, y = 5.48, label = "negative", hjust = 0, size = 1.78) +
  annotate("segment", x = 9.35, xend = 10.05, y = 5.48, yend = 5.48, colour = "#E69F00", linetype = "dotted", linewidth = 0.31) +
  annotate("text", x = 10.17, y = 5.48, label = "0.05 <= q < 0.10", hjust = 0, size = 1.78) +
  annotate("segment", x = 13.05, xend = 13.75, y = 5.48, yend = 5.48, colour = "#9AA2A8", linetype = "dashed", linewidth = 0.28) +
  annotate("text", x = 13.87, y = 5.48, label = "non-significant / calculated", hjust = 0, size = 1.78) +
  scale_colour_identity() + scale_fill_identity() + scale_linetype_identity() +
  coord_cartesian(xlim = c(0, 18.65), ylim = c(0.20, 11.72), expand = FALSE, clip = "off") +
  theme_void(base_family = "Arial", base_size = 7) +
  theme(plot.margin = margin(4, 5, 4, 5), text = element_text(colour = "#20272B"))

# Exported audit tables now correspond exactly to the visible compact diagram.
main_mgmt_edges <- nature_responses %>%
  dplyr::select(source, target, metric, contrast, edge_class, estimate, response_sd,
         delta_z, q_within_metric, interaction_p, n_years, label, colour, linetype)
main_evidence_edges <- bind_rows(
  nature_root_edges %>%
    left_join(
      association_results %>%
        filter(season == "wheat", N_outcome == "Total N uptake") %>%
        dplyr::select(metric, beta_treatment_adjusted, q_family, years),
      by = c("source" = "metric")
    ) %>%
    transmute(source, target, evidence_type = "matched-year adjusted association",
              label = sprintf("beta=%+.2f ns", beta_treatment_adjusted),
              coefficient = beta_treatment_adjusted, q = q_family, years),
  nature_relation_edges %>%
    transmute(source, target, evidence_type = "within-season association", label,
              coefficient = NA_real_, q = NA_real_, years = NA_character_),
  nature_evidence_edges %>%
    transmute(source, target, evidence_type, label = evidence_type,
              coefficient = NA_real_, q = NA_real_, years = NA_character_)
)

# Final user-selected layout: coefficient-and-path network supplied by the
# author.  Existing node positions and evidence paths are retained; planned
# comparison labels use baseline -> treatment notation and M5 uses PP4 -> PP6.
p_main <- p_compact_layout

ggsave(
  file.path(out_dir, "Figure7_integrated_pathway.png"), p_main,
  width = 18.00, height = 10.50, units = "in", dpi = 1000,
  device = ragg::agg_png, bg = "white", limitsize = FALSE
)
ggsave(
  file.path(out_dir, "Figure7_integrated_pathway.tiff"), p_main,
  width = 18.00, height = 10.50, units = "in", dpi = 600,
  device = ragg::agg_tiff, compression = "lzw", bg = "white", limitsize = FALSE
)
pdf_main_tmp <- file.path(tempdir(), "Figure7_integrated_pathway.pdf")
ggsave(
  pdf_main_tmp, p_main, width = 18.00, height = 10.50, units = "in",
  device = grDevices::cairo_pdf, bg = "white", limitsize = FALSE
)
if (!file.copy(pdf_main_tmp, file.path(out_dir, "Figure7_integrated_pathway.pdf"), overwrite = TRUE)) {
  stop("Could not copy the main-text vector PDF into the output directory: ", out_dir)
}

write.csv(main_mgmt_edges,
          file.path(out_dir, "Figure7_main_management_paths.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
write.csv(main_evidence_edges,
          file.path(out_dir, "Figure7_main_evidence_links.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

caption_lines <- c(
  "Figure 7. Integrated evidence pathways linking planned management transitions, root-zone traits, crop N acquisition and annual performance in the rice-wheat rotation.",
  "a, Direct responses to PP1->PP2, PP2->PP3, PP3->PP4, PP4->PP5 and PP4->PP6. Each coefficient is the standardized effect of the second treatment minus the first (Delta z). Solid vermillion and blue arrows denote BH-supported positive and negative responses, respectively; orange dotted arrows denote 0.05 <= q < 0.10. Only effects retaining the same sign in every measured year, or direct one-year measurements, are shown as supported paths. A dagger denotes a significant treatment-by-year interaction with sign stability; [24] denotes 2024-only 15N data. b, Seasonal evidence pathway. Grey dashed arrows from root-zone traits to wheat total N show treatment-adjusted coefficients fitted only to matching year x treatment x block observations; all were non-significant after BH adjustment. Source-N lines show treatment-mean R2 values from the 2024 15N data (PP2-PP6, blocks I-II; n=10). Yield-REN and yield-PEN lines show same-season Pearson correlations. Grey dashed links between N uptake, yield and annual outcomes are association-only or deterministic same-year calculations, not fitted causal coefficients. Annual NUE is annual REN calculated from same-year wheat and rice N totals. This is an evidence diagram rather than a fitted causal structural equation model. INtS, innovative no-till seeding; REN, recovery efficiency of applied N; PEN, physiological efficiency of applied N; SWC, soil water content; Kunsat, infiltration-derived apparent field hydraulic conductivity."
)
writeLines(caption_lines, file.path(out_dir, "Figure7_caption.txt"), useBytes = TRUE)

message("Main-text integrated Figure 7 completed successfully: ", out_dir)
