# ============================================================================
# Soil & Tillage Research | Revised Figure 5
# Panels A: 15N fertilizer partitioning (plant / soil 0-40 cm / unrecovered)
#        B: Sources of crop N uptake (fertilizer-derived / non-current-fertilizer)
#        C: Association between the absolute uptake of both N sources
# Source: latest 'yield and NUE.xlsx', N15_fate sheet (2024; 2 microplots per PP)
# Based on the original Figure1.R, panels D/E and Figure6.R, panel E.
# IMPORTANT: Original file_path/out_dir are intentionally unchanged.
# ============================================================================

# 1. Packages and unchanged paths ------------------------------------------------
pkgs <- c('readxl','dplyr','tidyr','tibble','ggplot2','patchwork',
          'writexl','ggrepel')
not_installed <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if(length(not_installed) > 0) {
  stop('Please install missing R packages: ', paste(not_installed, collapse = ', '))
}
suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr); library(tibble)
  library(ggplot2); library(patchwork); library(writexl); library(ggrepel)
})

file_path <- 'F:/Article/SCI/2025/Planting pattern/figure/3.1/yield and NUE.xlsx'
out_dir   <- 'F:/Article/figures/fig5'
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
if(!file.exists(file_path)) stop('Workbook not found at unchanged file_path: ',file_path)

trt_levels <- paste0('PP',2:6)
season_levels <- c('Wheat','Rice')
trt_cols <- c(PP2='#6B8FB3', PP3='#5B9E78', PP4='#C9A542',
              PP5='#D79073', PP6='#B95A55')
fate_cols <- c('Plant uptake'='#4DBBD5',
               'Soil residual (0-40 cm)'='#C49A6C',
               'Unrecovered 15N'='#E64B35')
source_cols <- c('Fertilizer-derived N'='#4DBBD5',
                 'Soil-derived N'='#C49A6C')

# 2. Read and validate the original isotope microplot data -----------------------
n15_raw <- readxl::read_excel(file_path, sheet='N15_fate')
needed <- c('Year','Season','Treatment','Block','Pool','N_rate_kg_ha',
            'Biomass_kg_ha','N_concentration_g_kg','Bulk_density_g_cm3',
            'Atom15N_sample_pct','Atom15N_background_pct','Atom15N_fertilizer_pct')
miss <- setdiff(needed,names(n15_raw))
if(length(miss)) stop('Missing N15_fate columns: ',paste(miss,collapse=', '))

n15 <- n15_raw %>%
  dplyr::transmute(
    year = as.integer(Year),
    season = dplyr::recode(tolower(trimws(as.character(Season))),
                           wheat='Wheat',rice='Rice', .default=NA_character_),
    treatment = as.character(Treatment),
    block = as.character(Block),
    pool = trimws(as.character(Pool)),
    n_rate = as.numeric(N_rate_kg_ha),
    biomass = as.numeric(Biomass_kg_ha),
    n_conc = as.numeric(N_concentration_g_kg),
    bulk_density = as.numeric(Bulk_density_g_cm3),
    atom_sample = as.numeric(Atom15N_sample_pct),
    atom_background = as.numeric(Atom15N_background_pct),
    atom_fertilizer = as.numeric(Atom15N_fertilizer_pct)
  ) %>%
  dplyr::filter(year==2024L, treatment %in% trt_levels,
                season %in% season_levels) %>%
  dplyr::mutate(
    season = factor(season,levels=season_levels),
    treatment = factor(treatment,levels=trt_levels),
    pool_type = ifelse(pool %in% c('10','20','40'),'Soil','Plant'),
    # 10, 20, 40 denote the *bottom boundaries* of 0-10, 10-20, 20-40 cm.
    depth_thickness_cm = dplyr::case_when(pool=='10'~10,
                                         pool=='20'~10,
                                         pool=='40'~20,
                                         TRUE~NA_real_),
    total_n_kg_ha = ifelse(pool_type=='Soil',
                          bulk_density * depth_thickness_cm * n_conc * 100,
                          biomass * n_conc / 1000),
    atom_excess = atom_sample-atom_background,
    atom_excess_fert = atom_fertilizer-atom_background,
    ndff_fraction = atom_excess/atom_excess_fert
  )

if(nrow(n15)==0) stop('No 2024 N15 records found for PP2-PP6.')
if(any(!is.finite(n15$total_n_kg_ha)) || any(n15$total_n_kg_ha < 0))
  stop('Invalid soil/plant total-N pool size. Check units and missing measurements.')
if(any(!is.finite(n15$ndff_fraction)) ||
   any(n15$ndff_fraction < -1e-6 | n15$ndff_fraction > 1+1e-6))
  stop('15N atom-excess ratio outside 0-1 or missing; check isotope input values.')
if(any(!is.finite(n15$n_rate)) || any(n15$n_rate <= 0))
  stop('Missing/invalid fertilizer N application rate.')

# Very small floating-point artifacts are clipped; no measurement outliers clipped.
n15 <- n15 %>%
  dplyr::mutate(ndff_fraction=pmin(pmax(ndff_fraction,0),1),
                fertilizer_n_kg_ha=total_n_kg_ha*ndff_fraction,
                non_current_fertilizer_n_kg_ha=total_n_kg_ha-fertilizer_n_kg_ha,
                recovery_pct=100*fertilizer_n_kg_ha/n_rate)

# Each crop/treatment/microplot should have 4 plant organs and 3 soil depths.
coverage <- n15 %>%
  dplyr::group_by(season,treatment,block) %>%
  dplyr::summarise(n_plant=sum(pool_type=='Plant'),
                   n_soil=sum(pool_type=='Soil'),
                   n_rows=dplyr::n(),
                   rate_unique=dplyr::n_distinct(n_rate),
                   .groups='drop')
expected <- tidyr::expand_grid(season=factor(season_levels,levels=season_levels),
                               treatment=factor(trt_levels,levels=trt_levels))
if(nrow(coverage)!=20 ||
   any(coverage$n_plant!=4 | coverage$n_soil!=3 | coverage$rate_unique!=1) ||
   nrow(dplyr::anti_join(expected,coverage,by=c('season','treatment')))>0)
  stop('Incomplete or inconsistent N15 sample coverage (2024, 2 blocks per PP). ',
       'Review N15_fate input and the coverage table.')

# 3. Calculate microplot-level fate and crop-N source partition ------------------
per_microplot <- n15 %>%
  dplyr::group_by(season,treatment,block) %>%
  dplyr::summarise(
    fertilizer_rate_kg_ha=dplyr::first(n_rate),
    plant_total_n_kg_ha=sum(total_n_kg_ha[pool_type=='Plant']),
    fertilizer_n_plant_kg_ha=sum(fertilizer_n_kg_ha[pool_type=='Plant']),
    fertilizer_n_soil_kg_ha=sum(fertilizer_n_kg_ha[pool_type=='Soil']),
    .groups='drop') %>%
  dplyr::mutate(
    non_current_fertilizer_n_plant_kg_ha=plant_total_n_kg_ha-fertilizer_n_plant_kg_ha,
    plant_recovery_pct=100*fertilizer_n_plant_kg_ha/fertilizer_rate_kg_ha,
    soil_recovery_pct=100*fertilizer_n_soil_kg_ha/fertilizer_rate_kg_ha,
    unrecovered_pct=100-plant_recovery_pct-soil_recovery_pct,
    fertilizer_source_pct=100*fertilizer_n_plant_kg_ha/plant_total_n_kg_ha,
    non_current_fertilizer_source_pct=100-fertilizer_source_pct,
    fate_balance_pct=plant_recovery_pct+soil_recovery_pct+unrecovered_pct,
    source_balance_pct=fertilizer_source_pct+non_current_fertilizer_source_pct
  )
if(any(per_microplot$unrecovered_pct < -0.01) ||
   any(per_microplot$fertilizer_source_pct < 0 | per_microplot$fertilizer_source_pct > 100) ||
   any(!is.finite(per_microplot$unrecovered_pct))) {
  stop('15N fate/source mass balance invalid: inspect per_microplot; no values are silently replaced.')
}
if(any(abs(per_microplot$fate_balance_pct-100)>0.001 | abs(per_microplot$source_balance_pct-100)>0.001))
  stop('Mass balance does not close to 100%.')

# Treatment means are the means of 2 independent labeled microplots.
means <- per_microplot %>%
  dplyr::group_by(season,treatment) %>%
  dplyr::summarise(
    n_microplots=dplyr::n(),
    plant_recovery_pct=mean(plant_recovery_pct),
    soil_recovery_pct=mean(soil_recovery_pct),
    unrecovered_pct=mean(unrecovered_pct),
    fertilizer_source_pct=mean(fertilizer_source_pct),
    non_current_fertilizer_source_pct=mean(non_current_fertilizer_source_pct),
    fertilizer_n_plant_kg_ha=mean(fertilizer_n_plant_kg_ha),
    non_current_fertilizer_n_plant_kg_ha=mean(non_current_fertilizer_n_plant_kg_ha),
    plant_total_n_kg_ha=mean(plant_total_n_kg_ha),
    .groups='drop')
stopifnot(nrow(means)==10, all(means$n_microplots==2))

# Scatter and regressions use only FIVE treatment means per crop; n=5 (not 10).
regression <- means %>%
  dplyr::group_by(season) %>%
  dplyr::group_modify(function(x,key){
    model <- stats::lm(non_current_fertilizer_n_plant_kg_ha ~ fertilizer_n_plant_kg_ha, data=x)
    out <- summary(model)
    tibble::tibble(n=nrow(x),r_squared=out$r.squared,
                   p_value=unname(coef(out)[2,'Pr(>|t|)']),
                   slope=unname(coef(model)[2]),
                   intercept=unname(coef(model)[1]))
  }) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(stat_label=sprintf('R² = %.2f, P = %.3f (n = %d)',r_squared,p_value,n))

# 4. Plot A: fertilizer fate -----------------------------------------------------
pd <- means %>%
  dplyr::select(season,treatment,plant_recovery_pct,soil_recovery_pct,unrecovered_pct) %>%
  tidyr::pivot_longer(cols=c(plant_recovery_pct,soil_recovery_pct,unrecovered_pct),
                      names_to='component',values_to='percentage') %>%
  dplyr::mutate(
    component=dplyr::recode(component,
                            plant_recovery_pct='Plant uptake',
                            soil_recovery_pct='Soil residual (0-40 cm)',
                            unrecovered_pct='Unrecovered 15N'),
    component=factor(component,levels=c('Unrecovered 15N',
                                        'Soil residual (0-40 cm)','Plant uptake')),
    lab=ifelse(percentage>=7,sprintf('%.0f',percentage),'')
  )

# 4. Main Figure 5 layout settings ------------------------------------------------
# New arrangement: A and B occupy the left and middle columns (both seasons);
# C occupies the third column, with Wheat above Rice.
# Large-font edition: label sizes are defined in ONE place for adjustment.
# The A/B/C panel tags remain at the original 21 pt.
FONT_AXIS_TICK_PT    <- 16.0
FONT_AXIS_TITLE_PT   <- 18.0
FONT_STRIP_PT        <- 17.0
FONT_LEGEND_PT       <- 13.7
FONT_BAR_LABEL_MM    <- 5.6  # geom_text uses mm (approximately 15.9 pt)
FONT_POINT_LABEL_MM  <- 5.4  # geom_text_repel uses mm
FONT_REGRESSION_MM   <- 5.2  # R-squared/P-value annotations
FONT_PANEL_TAG_PT    <- 21.0

fig_theme <- function() {
  ggplot2::theme_classic(base_size=16) +
    ggplot2::theme(
      axis.text=ggplot2::element_text(colour='black',size=FONT_AXIS_TICK_PT),
      axis.title=ggplot2::element_text(colour='black',size=FONT_AXIS_TITLE_PT),
      strip.background=ggplot2::element_rect(fill='#F2F2F2',colour='#BBBBBB',linewidth=0.4),
      strip.text=ggplot2::element_text(face='bold',size=FONT_STRIP_PT),
      legend.text=ggplot2::element_text(size=FONT_LEGEND_PT),
      legend.title=ggplot2::element_text(size=FONT_LEGEND_PT),
      legend.key.size=grid::unit(0.32,'cm'),
      legend.spacing.x=grid::unit(0.06,'cm'),
      legend.margin=ggplot2::margin(0,0,0,0),
      # A/B/C panel tags remain unchanged in size.
      plot.tag=ggplot2::element_text(face='bold',size=FONT_PANEL_TAG_PT),
      plot.margin=ggplot2::margin(t=7,r=6,b=6,l=6)
    )
}

# A. 15N fertilizer fate: Wheat/Rice stacked vertically within FIRST column.
pA <- ggplot2::ggplot(pd,ggplot2::aes(treatment,percentage,fill=component))+
  ggplot2::geom_col(width=0.72,color='white',linewidth=0.6)+
  ggplot2::geom_text(ggplot2::aes(label=lab),
                     position=ggplot2::position_stack(vjust=0.5),
                     color='white',size=FONT_BAR_LABEL_MM,fontface='bold')+
  ggplot2::facet_grid(season~.)+
  ggplot2::scale_fill_manual(values=fate_cols,
      breaks=c('Plant uptake','Soil residual (0-40 cm)','Unrecovered 15N'))+
  ggplot2::scale_y_continuous(breaks=c(0,25,50,75,100),
      expand=ggplot2::expansion(mult=c(0,0)))+
  ggplot2::coord_cartesian(ylim=c(0,100))+
  ggplot2::labs(x=NULL,y=expression({}^{15}*N~'fate (%)'),fill=NULL,tag='A')+
  fig_theme()+
  ggplot2::theme(
    legend.position='top',
    legend.direction='horizontal',
    legend.box='horizontal',
    legend.justification='center',
    axis.text.x=ggplot2::element_text(size=FONT_AXIS_TICK_PT),
    legend.key.width=grid::unit(0.32,'cm'),
    panel.spacing.y=grid::unit(0.40,'cm'))+
  # All THREE fate categories must appear on ONE horizontal row.
  ggplot2::guides(fill=ggplot2::guide_legend(nrow=1,byrow=TRUE,
                                           title=NULL,
                                           keywidth=grid::unit(0.32,'cm'),
                                           keyheight=grid::unit(0.32,'cm')))

# 5. Plot B: origin of plant N --------------------------------------------------
ps <- means %>%
  dplyr::select(season,treatment,fertilizer_source_pct,non_current_fertilizer_source_pct) %>%
  tidyr::pivot_longer(cols=c(fertilizer_source_pct,non_current_fertilizer_source_pct),
                      names_to='source',values_to='percentage') %>%
  dplyr::mutate(
    source=dplyr::recode(source,
                         fertilizer_source_pct='Fertilizer-derived N',
                         non_current_fertilizer_source_pct='Soil-derived N'),
    source=factor(source,levels=c('Soil-derived N','Fertilizer-derived N')),
    lab=ifelse(percentage>=7,sprintf('%.0f',percentage),'')
  )
# B occupies the SECOND column, extending over both crop-season rows.
pB <- ggplot2::ggplot(ps,ggplot2::aes(treatment,percentage,fill=source))+
  ggplot2::geom_col(width=0.72,color='white',linewidth=0.6)+
  ggplot2::geom_text(ggplot2::aes(label=lab),
                     position=ggplot2::position_stack(vjust=0.5),
                     color='white',size=FONT_BAR_LABEL_MM,fontface='bold')+
  ggplot2::facet_grid(season~.)+
  ggplot2::scale_fill_manual(values=source_cols,
      breaks=c('Fertilizer-derived N','Soil-derived N'))+
  ggplot2::scale_y_continuous(breaks=c(0,25,50,75,100),
      expand=ggplot2::expansion(mult=c(0,0)))+
  ggplot2::coord_cartesian(ylim=c(0,100))+
  ggplot2::labs(x=NULL,y='Sources of crop N uptake (%)',fill=NULL,tag='B')+
  fig_theme()+
  ggplot2::theme(legend.position='top',
                 legend.direction='horizontal',
                 legend.box='horizontal',
                 legend.justification='center',
                 axis.text.x=ggplot2::element_text(size=FONT_AXIS_TICK_PT),
                 panel.spacing.y=grid::unit(0.40,'cm'))+
  # Both N sources displayed together in ONE legend row.
  ggplot2::guides(fill=ggplot2::guide_legend(nrow=1,byrow=TRUE,title=NULL,
                                           keywidth=grid::unit(0.32,'cm'),
                                           keyheight=grid::unit(0.32,'cm')))

# 6. Plot C: source-specific N uptake ------------------------------------------
# facet_wrap(ncol=1, scales='free') preserves independently scaled wheat/rice
# axes and places Wheat on ROW 1, Rice on ROW 2 in the THIRD column.
points <- means %>% dplyr::mutate(treatment=factor(treatment,levels=trt_levels))
pC <- ggplot2::ggplot(points,
    ggplot2::aes(x=fertilizer_n_plant_kg_ha,
                 y=non_current_fertilizer_n_plant_kg_ha))+
  ggplot2::geom_smooth(method='lm',formula=y~x,se=TRUE,
                       colour='#333333',fill='#D2D2D2',linewidth=0.7)+
  ggplot2::geom_point(ggplot2::aes(colour=treatment),size=4.8)+
  ggrepel::geom_text_repel(ggplot2::aes(label=treatment,colour=treatment),
                          size=FONT_POINT_LABEL_MM,show.legend=FALSE,seed=2026,
                          box.padding=0.50,point.padding=0.30,
                          max.overlaps=Inf,min.segment.length=0.05)+
  ggplot2::facet_wrap(~season,ncol=1,scales='free')+
  ggplot2::geom_text(data=regression,
                     ggplot2::aes(x=-Inf,y=Inf,label=stat_label),
                     inherit.aes=FALSE,hjust=-0.04,vjust=1.35,size=FONT_REGRESSION_MM)+
  ggplot2::scale_colour_manual(values=trt_cols,drop=FALSE)+
  ggplot2::scale_x_continuous(expand=ggplot2::expansion(mult=c(0.13,0.20)))+
  ggplot2::scale_y_continuous(expand=ggplot2::expansion(mult=c(0.15,0.18)))+
  ggplot2::labs(x=expression('Fertilizer-derived N uptake (kg ha'^{-1}*')'),
                y=expression('Soil-derived N uptake (kg ha'^{-1}*')'),
                colour=NULL,tag='C')+
  fig_theme()+
  ggplot2::theme(legend.position='none',
                 panel.grid.major=ggplot2::element_line(
                    colour='#EAEAEA',linewidth=0.22),
                 panel.grid.minor=ggplot2::element_blank(),
                 strip.text=ggplot2::element_text(face='bold',size=FONT_STRIP_PT))

# NEW LAYOUT: A | B | C, each plot spanning the full height of both seasons.
# Inside C: Wheat above Rice. The first column is slightly wider so
# the 3 enlarged items in legend A still fit on ONE row.
# Do NOT auto-tag here: A/B/C tags were set explicitly with labs(tag=...).
fig5 <- (pA | pB | pC) +
  patchwork::plot_layout(ncol=3,widths=c(1.25,1.00,1.10))

# 7. Outputs -------------------------------------------------------------------
ggplot2::ggsave(file.path(out_dir,'Figure5_STR_15N_fate_and_crop_N_sources.png'),
                fig5,width=19.5,height=9.2,units='in',dpi=1000,bg='white',limitsize=FALSE)
ggplot2::ggsave(file.path(out_dir,'Figure5_STR_15N_fate_and_crop_N_sources.pdf'),
                fig5,width=19.5,height=9.2,units='in',bg='white',limitsize=FALSE)

# Individual panels for convenient review/adjustment
ggplot2::ggsave(file.path(out_dir,'Figure5_panel_A_fate.png'),pA,
                width=7.0,height=8.4,units='in',dpi=400,bg='white')
ggplot2::ggsave(file.path(out_dir,'Figure5_panel_B_sources.png'),pB,
                width=7.0,height=8.4,units='in',dpi=400,bg='white')
ggplot2::ggsave(file.path(out_dir,'Figure5_panel_C_source_correlation.png'),pC,
                width=7.0,height=8.4,units='in',dpi=400,bg='white')

# Export fully traceable original and processed tables; suitable for preparation
# of a revised supplementary table after figure confirmation.
export_tables <- list(
  N15_original_2024=n15_raw %>% dplyr::filter(Year==2024,Treatment %in% trt_levels),
  N15_pool_calculation=n15 %>% dplyr::mutate(season=as.character(season),treatment=as.character(treatment)),
  Microplot_15N_balances=per_microplot %>% dplyr::mutate(season=as.character(season),treatment=as.character(treatment)),
  Treatment_15N_means=means %>% dplyr::mutate(season=as.character(season),treatment=as.character(treatment)),
  Figure5A_plot_data=pd %>% dplyr::mutate(season=as.character(season),treatment=as.character(treatment),component=as.character(component)),
  Figure5B_plot_data=ps %>% dplyr::mutate(season=as.character(season),treatment=as.character(treatment),source=as.character(source)),
  Figure5C_regressions=regression %>% dplyr::mutate(season=as.character(season)),
  Sample_coverage=coverage %>% dplyr::mutate(season=as.character(season),treatment=as.character(treatment))
)
writexl::write_xlsx(export_tables,
  file.path(out_dir,'Figure5_STR_15N_statistics_and_source_audit.xlsx'))
utils::write.csv(means,file.path(out_dir,'Figure5_STR_treatment_means.csv'),row.names=FALSE)
utils::write.csv(regression,file.path(out_dir,'Figure5_STR_source_correlations.csv'),row.names=FALSE)

print(regression)
message('Figure 5 completed: 2024, n = 2 labeled microplots per crop x treatment; ',
        'n = 5 treatment means per crop for correlations.')
message('PNG, PDF, and audit workbook saved to: ',out_dir)
