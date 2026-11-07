# ============================================================================
# STR resubmission | Main Figure 4 and supplementary anthesis-root figure
# Based on the original Figure6.R; independent of its Figures 6E-G modules.
# Main Figure 4: A root dry weight (jointing), B root TTC activity (jointing),
#                C wheat total plant N uptake, D rice total plant N uptake.
# Supplement:    A root dry weight (anthesis), B root TTC activity (anthesis).
# ============================================================================

# 1. Packages and ORIGINAL user paths (left unchanged)
required_pkgs <- c('readxl','writexl','dplyr','ggplot2','patchwork',
                   'nlme','emmeans','multcompView')
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly=TRUE)]
if(length(missing_pkgs)>0) {
  stop('Required packages not installed: ',paste(missing_pkgs,collapse=', '),
       '\nInstall with install.packages(c(', paste(sprintf('"%s"',missing_pkgs),collapse=', '), '))')
}

# Original paths from Figure6.R: do NOT change.
data_file <- 'F:/Article/SCI/2025/Planting pattern/Data.xlsx'
fig31_dir <- 'F:/Article/SCI/2025/Planting pattern/figure/3.1'
out_dir <- Sys.getenv('FIGURE5_OUT_DIR',
                      unset='F:/Article/figures/fig4')
dir.create(out_dir,recursive=TRUE,showWarnings=FALSE)

# The latest user-provided 'yield and NUE(4).xlsx' supersedes older processed
# workbooks. Automatically locate it; optionally supply its exact local path
# through the YIELD_NUE_FILE environment variable without editing the paths above.
yield_candidates <- c(
  Sys.getenv('YIELD_NUE_FILE',unset=''),
  file.path(dirname(data_file),'yield and NUE(4).xlsx'),
  file.path(dirname(data_file),'yield and NUE.xlsx'),
  file.path(fig31_dir,'yield and NUE(4).xlsx'),
  file.path(fig31_dir,'yield and NUE.xlsx')
)
yield_candidates <- yield_candidates[nzchar(yield_candidates)]
available_yield_files <- yield_candidates[file.exists(yield_candidates)]
if(!file.exists(data_file)) stop('The original Data.xlsx path is not accessible: ',data_file)
if(length(available_yield_files)==0) {
  stop('Latest yield and NUE workbook not found.\n',
       'Keep data_file unchanged and point YIELD_NUE_FILE to your latest local workbook, e.g.\n',
       'Sys.setenv(YIELD_NUE_FILE="F:/.../yield and NUE(4).xlsx")')
}
yield_file <- available_yield_files[1]
cat('Root data:',data_file,'\nPlant N data:',yield_file,'\nOutput:',out_dir,'\n')

# 2. Settings
trt_levels <- paste0('PP',1:6)
depth_levels <- c('0-10 cm','10-20 cm','20-40 cm')
trt_cols <- c(PP1='#4D4D4D', PP2='#6B8FB3', PP3='#5B9E78',
              PP4='#C9A542', PP5='#D79073', PP6='#B95A55')
root_fill <- '#D9E6EB'; root_edge <- '#627B86'
wheat_edge <- '#7BACC6'; rice_edge <- '#D3AA55'

# Protected Fisher LSD as in the author's original comparisons. The pooled
# error is now obtained from a repeated-plot mixed model, not an ordinary
# ANOVA pooling repeated observations as independent replicates.
protect_LSD <- TRUE

num <- function(x) suppressWarnings(as.numeric(as.character(x)))
treat <- function(x) {
  z <- toupper(trimws(as.character(x)))
  z <- ifelse(grepl('^T[1-6]$',z), paste0('PP',substr(z,2,2)), z)
  factor(z,levels=trt_levels)
}
block_name <- function(x) {
  z <- toupper(trimws(as.character(x)))
  z[z %in% c('4#','4','I','1')] <- 'I'
  z[z %in% c('5#','5','II','2')] <- 'II'
  z[z %in% c('6#','6','III','3')] <- 'III'
  z
}
check_unique <- function(x, keys, label) {
  if(anyNA(x[keys])) stop(label,': missing keys ',paste(keys,collapse=', '))
  dup <- duplicated(x[keys]); if(any(dup)) stop(label,': duplicated plot records (',sum(dup),')')
}
mean_se <- function(x) {
  n <- sum(is.finite(x))
  data.frame(mean=if(n) mean(x,na.rm=TRUE) else NA_real_,
             se=if(n>1) sd(x,na.rm=TRUE)/sqrt(n) else NA_real_,n=n)
}

# 3. Read latest root data
root0 <- readxl::read_excel(data_file,sheet='根系活力干重')
needed_root <- c('crop','year','date','re','treat','depth',
                 '根系活力ugTPF•g-1FW•h-1','根干重/g')
if(!all(needed_root %in% names(root0)))
  stop('Root sheet is missing: ',paste(setdiff(needed_root,names(root0)),collapse=', '))

root <- data.frame(
  crop=tolower(as.character(root0$crop)), year=as.integer(root0$year),
  stage=as.character(root0$date), block=block_name(root0$re),
  treatment=treat(root0$treat), depth=num(root0$depth),
  dry_weight=num(root0[['根干重/g']]),
  activity=num(root0[['根系活力ugTPF•g-1FW•h-1']]),
  stringsAsFactors=FALSE
)
root$stage <- ifelse(root$stage %in% c('拔节','拔节期','jointing'), 'Jointing',
                     ifelse(root$stage %in% c('开花','开花期','花期','anthesis'), 'Anthesis',NA_character_))
root$depth_layer <- factor(ifelse(root$depth==10,'0-10 cm',
                           ifelse(root$depth==20,'10-20 cm',
                           ifelse(root$depth==40,'20-40 cm',NA_character_))),levels=depth_levels)
root <- root[which(root$crop=='wheat' & root$year %in% c(2024L,2025L) &
                   !is.na(root$stage) & !is.na(root$depth_layer) &
                   !is.na(root$treatment)),]
check_unique(root,c('year','stage','block','treatment','depth_layer'),'Root')
if(any(!is.finite(root$dry_weight)) || any(!is.finite(root$activity)))
  stop('Missing/non-numeric root values found; check original Data.xlsx.')
root$plot_id <- factor(paste(root$block,root$treatment,sep='_'))
if(nrow(root)==0) stop('No root observations found.')

root_long <- rbind(
  data.frame(root[c('year','stage','block','plot_id','treatment','depth_layer')],
             trait='Root dry weight', value=root$dry_weight),
  data.frame(root[c('year','stage','block','plot_id','treatment','depth_layer')],
             trait='Root activity',value=root$activity)
)
root_long$trait <- factor(root_long$trait,levels=c('Root dry weight','Root activity'))

# 4. Read the newest plant_N worksheet and recompute total N uptake
plant0 <- readxl::read_excel(yield_file,sheet='plant_N')
needed_plant <- c('Year','Crop','Treatment','Block','Grain_yield_kg_ha',
  'Straw_biomass_kg_ha','Grain_N_g_kg','Straw_N_g_kg')
if(!all(needed_plant %in% names(plant0)))
  stop('plant_N sheet is missing: ',paste(setdiff(needed_plant,names(plant0)),collapse=', '))
plant <- data.frame(
  year=as.integer(plant0$Year),crop=tolower(trimws(as.character(plant0$Crop))),
  treatment=treat(plant0$Treatment),block=block_name(plant0$Block),
  grain_kg_ha=num(plant0$Grain_yield_kg_ha),
  straw_kg_ha=num(plant0$Straw_biomass_kg_ha),
  grain_N_g_kg=num(plant0$Grain_N_g_kg),
  straw_N_g_kg=num(plant0$Straw_N_g_kg),stringsAsFactors=FALSE
)
plant <- plant[which(plant$year %in% 2020:2025 & plant$crop %in% c('wheat','rice') &
                  !is.na(plant$treatment)),]
check_unique(plant,c('year','crop','block','treatment'),'plant_N')
plant$total_N_uptake_kg_ha <-
  (plant$grain_kg_ha*plant$grain_N_g_kg +
   plant$straw_kg_ha*plant$straw_N_g_kg)/1000
if(any(!is.finite(plant$total_N_uptake_kg_ha)) || any(plant$total_N_uptake_kg_ha<=0))
  stop('Non-positive/missing calculated plant N uptake. Check the four input columns.')
plant$plot_id <- factor(paste(plant$block,plant$treatment,sep='_'))
plant$crop <- factor(plant$crop,levels=c('wheat','rice'))

# 5. Statistics: independent plot is block x treatment; year is repeated on plot
#     Fits fixed treatment/year/block and a random plot intercept.
#     A failed model is NOT silently translated into all-'a' labels.
fit_and_letters <- function(dat,label) {
  dat$treatment <- factor(as.character(dat$treatment),levels=trt_levels)
  dat$year <- factor(dat$year)
  dat$block <- factor(dat$block)
  dat$plot_id <- factor(dat$plot_id)
  dat <- droplevels(dat[is.finite(dat$value),])
  if(length(unique(dat$treatment))<2 || length(unique(dat$plot_id))<6)
    stop(label,': too few independent plots to compare treatments.')
  fit <- tryCatch(
    nlme::lme(value ~ treatment + year + block,
              random=~1|plot_id, data=dat, na.action=na.omit,
              control=nlme::lmeControl(opt='optim',msMaxIter=250)),
    error=function(e) e
  )
  if(inherits(fit,'error')) {
    warning('Model FAILED for ',label,': ',conditionMessage(fit),
            '. Leaving grouping letters BLANK (not a).')
    return(list(letters=data.frame(treatment=factor(trt_levels,levels=trt_levels),
                                   sig=NA_character_),
                stats=data.frame(group=label,model='FAILED',n=nrow(dat),
                                 plots=length(unique(dat$plot_id)),omnibus_p=NA_real_),
                contrast=data.frame(group=character(),contrast=character(),p.value=numeric())))
  }
  a <- anova(fit)
  p_main <- if('treatment'%in%rownames(a)) as.numeric(a['treatment','p-value']) else NA_real_
  em <- emmeans::emmeans(fit,'treatment')
  pairs_df <- as.data.frame(emmeans::contrast(em, method='pairwise', adjust='none'))
  pp <- pairs_df$p.value
  names(pp) <- gsub(' - ','-',as.character(pairs_df$contrast),fixed=TRUE)
  if(is.finite(p_main) && protect_LSD && p_main>=0.05) {
    lbl <- setNames(rep('a',length(trt_levels)),trt_levels)
  } else if(!is.finite(p_main) && protect_LSD) {
    lbl <- setNames(rep(NA_character_,length(trt_levels)),trt_levels)
  } else {
    valid <- is.finite(pp)
    lbl <- if(all(valid)) multcompView::multcompLetters(pp)$Letters else
      setNames(rep(NA_character_,length(trt_levels)),trt_levels)
  }
  # Relabel letter symbols so "a" denotes groups with the highest fitted mean.
  if(all(!is.na(lbl))) {
    emm_df <- as.data.frame(em)
    fitted_means <- setNames(emm_df$emmean,as.character(emm_df$treatment))
    symbols <- unique(unlist(strsplit(paste(lbl,collapse=''),'')))
    scores <- vapply(symbols,function(z) {
      ids <- names(lbl)[grepl(z,lbl,fixed=TRUE)]
      max(fitted_means[ids],na.rm=TRUE)
    },numeric(1))
    order_syms <- symbols[order(scores,decreasing=TRUE)]
    symbol_map <- setNames(letters[seq_along(order_syms)],order_syms)
    lbl <- vapply(lbl,function(s) paste(symbol_map[strsplit(s,'')[[1]]],collapse=''),character(1))
  }
  pair_table <- data.frame(group=label,contrast=as.character(pairs_df$contrast),
                           p.value=pairs_df$p.value)
  list(letters=data.frame(treatment=factor(names(lbl),levels=trt_levels),
                          sig=unname(lbl)),
       stats=data.frame(group=label,model='nlme::lme, year and block fixed; plot random',
                        n=nrow(dat),plots=length(unique(dat$plot_id)),
                        omnibus_p=p_main),contrast=pair_table)
}

root_tests <- list(); stats_tables <- list(); pairs_tables <- list()
for(stage in c('Jointing','Anthesis')) for(trait_name in c('Root dry weight','Root activity'))
  for(depth_name in depth_levels) {
    k <- paste(stage,trait_name,depth_name,sep=' | ')
    d <- root_long[root_long$stage==stage & root_long$trait==trait_name &
                   root_long$depth_layer==depth_name,]
    root_tests[[k]] <- fit_and_letters(d,k)
    stats_tables[[k]] <- root_tests[[k]]$stats
    pairs_tables[[k]] <- root_tests[[k]]$contrast
  }
plant_tests <- list()
for(crop_name in c('wheat','rice')) {
  k <- paste('Plant N uptake',crop_name,sep=' | ')
  d <- plant[as.character(plant$crop)==crop_name,]
  d$value <- d$total_N_uptake_kg_ha
  plant_tests[[crop_name]] <- fit_and_letters(d,k)
  stats_tables[[k]] <- plant_tests[[crop_name]]$stats
  pairs_tables[[k]] <- plant_tests[[crop_name]]$contrast
}

root_summary_list <- list()
for(stage in c('Jointing','Anthesis')) for(trait_name in c('Root dry weight','Root activity'))
 for(depth_name in depth_levels) {
    k <- paste(stage,trait_name,depth_name,sep=' | ')
    d <- root_long[root_long$stage==stage & root_long$trait==trait_name &
                   root_long$depth_layer==depth_name,]
    s <- dplyr::summarise(dplyr::group_by(d,treatment),
                          mean=mean(value),se=sd(value)/sqrt(dplyr::n()),
                          n=dplyr::n(),.groups='drop')
    s <- dplyr::left_join(s,root_tests[[k]]$letters,by='treatment')
    s$stage <- stage; s$trait <- trait_name; s$depth_layer <- depth_name
    root_summary_list[[k]] <- s
 }
root_summary <- dplyr::bind_rows(root_summary_list)
root_summary$depth_layer <- factor(root_summary$depth_layer,levels=depth_levels)

plant_summary <- dplyr::summarise(dplyr::group_by(plant,crop,treatment),
  mean=mean(total_N_uptake_kg_ha),
  se=sd(total_N_uptake_kg_ha)/sqrt(dplyr::n()),
  n=dplyr::n(),.groups='drop')
plant_summary$sig <- NA_character_
for(crop_name in c('wheat','rice')) {
  ix <- as.character(plant_summary$crop)==crop_name
  map <- setNames(plant_tests[[crop_name]]$letters$sig,
                  as.character(plant_tests[[crop_name]]$letters$treatment))
  plant_summary$sig[ix] <- map[as.character(plant_summary$treatment[ix])]
}

# 6. Plots: all valid significance letters, including repeated "a", are retained.
# Figure 4 + supplementary: enlarge all data text by ~2 pt.
#   - theme_bw base: +2 pt (axis labels, tick marks, etc.)
#   - facet strip labels: +2 pt
#   - geom_text significance letters: +2 pt after mm-to-pt conversion
#   - A/B/C/D panel tags (plot.tag) intentionally remain 19 pt.
theme_str <- function(base=14.5) {
  ggplot2::theme_bw(base_size=base) +
    ggplot2::theme(panel.grid.minor=ggplot2::element_blank(),
      panel.grid.major.x=ggplot2::element_blank(),
      strip.background=ggplot2::element_rect(fill='#F3F3F3',colour='#C4C4C4'),
      strip.text=ggplot2::element_text(face='bold',size=14),
      axis.text=ggplot2::element_text(color='black'),
      axis.text.x=ggplot2::element_text(angle=90,vjust=0.5,hjust=1),
      plot.tag=ggplot2::element_text(size=19,face='bold'),
      plot.margin=ggplot2::margin(5,5,5,5),legend.position='none')
}

root_panel <- function(stage,trait_name,tag) {
  dd <- root_long[root_long$stage==stage & root_long$trait==trait_name,]
  sm <- root_summary[root_summary$stage==stage & root_summary$trait==trait_name,]
  span <- dplyr::summarise(dplyr::group_by(dd,depth_layer),
                           span=max(value)-min(value),.groups='drop')
  maxobs <- dplyr::summarise(dplyr::group_by(dd,depth_layer,treatment),
                             max_data=max(value),.groups='drop')
  sm <- dplyr::left_join(sm,span,by='depth_layer')
  sm <- dplyr::left_join(sm,maxobs,by=c('depth_layer','treatment'))
  sm$letter_y <- pmax(sm$mean+sm$se,sm$max_data)+pmax(0.06*sm$span,0.10)
  yl <- if(trait_name=='Root dry weight') 'Root dry weight (g)' else
      expression('Root activity ('*mu*'g TPF g'^-1*' FW h'^-1*')')
  ggplot2::ggplot() +
    ggplot2::geom_col(data=sm,ggplot2::aes(treatment,mean),width=.65,
                      fill=root_fill,color=root_edge,linewidth=.4) +
    ggplot2::geom_errorbar(data=sm,ggplot2::aes(treatment,ymin=mean-se,ymax=mean+se),
                            width=.17,linewidth=.45) +
    ggplot2::geom_point(data=dd,ggplot2::aes(treatment,value,color=treatment),
                        position=ggplot2::position_jitter(width=.12,height=0,seed=41),
                        size=1.75,alpha=.82) +
    ggplot2::geom_text(data=sm,ggplot2::aes(treatment,letter_y,label=sig),
                       size=5.10,na.rm=TRUE) +
    ggplot2::facet_wrap(~depth_layer,nrow=1,scales='free_y') +
    ggplot2::scale_colour_manual(values=trt_cols) +
    ggplot2::scale_y_continuous(expand=ggplot2::expansion(mult=c(.04,.20))) +
    ggplot2::labs(tag=tag,x=NULL,y=yl) + theme_str(14.8)
}

plant_panel <- function(crop_name,tag) {
  dd <- plant[as.character(plant$crop)==crop_name,]
  sm <- plant_summary[as.character(plant_summary$crop)==crop_name,]
  ymax <- dplyr::summarise(dplyr::group_by(dd,treatment),
    max_data=max(total_N_uptake_kg_ha),.groups='drop')
  sm <- dplyr::left_join(sm,ymax,by='treatment')
  span <- diff(range(dd$total_N_uptake_kg_ha))
  sm$letter_y <- sm$max_data + 0.055*span
  boxcol <- if(crop_name=='wheat') wheat_edge else rice_edge
  yl <- if(crop_name=='wheat') expression('Wheat plant N uptake (kg ha'^-1*')') else
    expression('Rice plant N uptake (kg ha'^-1*')')
  ggplot2::ggplot(dd,ggplot2::aes(treatment,total_N_uptake_kg_ha)) +
    ggplot2::geom_boxplot(width=.57,outlier.shape=NA,fill='white',
                          colour=boxcol,linewidth=.76) +
    ggplot2::geom_point(position=ggplot2::position_jitter(width=.12,height=0,seed=42),
                        size=1.35,alpha=.42,colour='#777777') +
    ggplot2::geom_text(data=sm,ggplot2::aes(x=treatment,y=letter_y,label=sig),
                       inherit.aes=FALSE,size=5.30,na.rm=TRUE) +
    ggplot2::scale_y_continuous(expand=ggplot2::expansion(mult=c(.04,.18))) +
    ggplot2::labs(tag=tag,x=NULL,y=yl) + theme_str(15.2)
}

pA <- root_panel('Jointing','Root dry weight','A')
pB <- root_panel('Jointing','Root activity','B')
pC <- plant_panel('wheat','C')
pD <- plant_panel('rice','D')
fig4 <- (pA | pB) / (pC | pD) + patchwork::plot_layout(heights=c(1.05,1))

supp_A <- root_panel('Anthesis','Root dry weight','A')
supp_B <- root_panel('Anthesis','Root activity','B')
fig_supp <- supp_A | supp_B

save_figure <- function(p,name,w,h) {
  ggplot2::ggsave(file.path(out_dir,paste0(name,'.png')),p,width=w,height=h,dpi=600,
                  units='in',limitsize=FALSE,bg='white')
  pdf_device <- if(isTRUE(capabilities('cairo'))) grDevices::cairo_pdf else grDevices::pdf
  ggplot2::ggsave(file.path(out_dir,paste0(name,'.pdf')),p,width=w,height=h,
                  units='in',limitsize=FALSE,device=pdf_device,bg='white')
}
save_figure(fig4,'Figure4_STR_root_growth_plant_N_uptake',17.8,10.4)
save_figure(fig_supp,'Supplementary_root_traits_anthesis_2024_2025',17.8,5.3)

# 7. Audit + optional supplementary statistical table
# Not all model errors lead to true non-significance. Failed models have BLANK letters.
stat_tbl <- dplyr::bind_rows(stats_tables)
pair_tbl <- dplyr::bind_rows(pairs_tables)
root_audit <- root[c('crop','year','stage','block','treatment','depth_layer','dry_weight','activity')]
plant_audit <- plant[c('year','crop','block','treatment','grain_kg_ha','straw_kg_ha',
                       'grain_N_g_kg','straw_N_g_kg','total_N_uptake_kg_ha')]

# Detect group sizes expected from the latest data (2 years x 2 root plots;
# 6 years x 3 crop plots). Deviations are written to the workbook.
root_expected <- dplyr::summarise(dplyr::group_by(root_long,stage,trait,depth_layer,treatment),
                                  n=dplyr::n(),.groups='drop')
plant_expected <- dplyr::summarise(dplyr::group_by(plant,crop,treatment),
                                   n=dplyr::n(),.groups='drop')
write_tab <- function(x) {
  x <- as.data.frame(x)
  for(nm in names(x)) if(is.factor(x[[nm]])) x[[nm]] <- as.character(x[[nm]])
  x
}
export_tabs <- list(
  source_files=data.frame(role=c('Root data','Plant N data'),path=c(data_file,yield_file)),
  root_raw=write_tab(root_audit),plant_N_recalculated=write_tab(plant_audit),
  root_group_counts=write_tab(root_expected),plant_group_counts=write_tab(plant_expected),
  root_mean_SE_letters=write_tab(root_summary),plant_N_mean_SE_letters=write_tab(plant_summary),
  mixed_model_tests=write_tab(stat_tbl),Fisher_LSD_pairwise_p=write_tab(pair_tbl)
)
writexl::write_xlsx(export_tabs,file.path(out_dir,'Figure4_data_statistics_audit.xlsx'))
cat('\nCOMPLETED Figure 4 and anthesis supplementary figure.\n')
cat('Root counts:\n'); print(table(root$year,root$stage))
cat('Crop N counts:\n'); print(table(plant$year,plant$crop))
cat('Check Model tests and letter groupings in Figure4_data_statistics_audit.xlsx.\n')
