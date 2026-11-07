# Fix for select() masking by packages such as MASS (2026-11-04):
# Explicit dplyr::select() is used throughout this script.
# Publication layout update (2026-11-04):
# Figure 3A legend moved to a vertical bar on the right;
# Figure 3C legend expanded horizontally, with one-line titles.
# Statistical analyses, input files, and supplementary figure logic unchanged.
# ==============================================================
# STR revised Figure 3: soil enzyme activities + bacterial community
# + taxonomically predicted N-cycling functional potential
# Based on the user's Figure3-5.R; 2026-10-21 data release.
# Outputs: MAIN Fig 3, S11 replacement (two subfigures), S12B genus,
# and all source/statistics/audit tables.
# ==============================================================

required_pkgs <- c("readxl", "readr", "writexl", "dplyr", "tidyr", "tibble",
                   "stringr", "ggplot2", "patchwork", "vegan", "permute",
                   "multcompView", "scales")
not_installed <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if(length(not_installed) > 0) stop("Install missing R packages: ", paste(not_installed, collapse = ", "))
suppressPackageStartupMessages({
  library(readxl); library(readr); library(writexl); library(dplyr)
  library(tidyr); library(tibble); library(stringr); library(ggplot2)
  library(patchwork); library(vegan); library(permute); library(multcompView)
})
set.seed(20261021)

# ---- SAME THREE INPUT/OUTPUT PATHS AS ORIGINAL Figure3-5.R ----
data_file <- "F:/Article/SCI/2025/Planting pattern/Data.xlsx"
microbe_dir <- "F:/Article/SCI/2025/Planting pattern/原始数据/微生物测试"
out_dir <- Sys.getenv("FIGURE3_OUT_DIR",
  unset = "F:/Article/figures/fig3")
dir.create(out_dir, recursive=TRUE, showWarnings=FALSE)
if(!file.exists(data_file)) stop("Updated Data.xlsx not found at: ", data_file)
if(!dir.exists(microbe_dir)) stop("Microbial parent folder not found at: ", microbe_dir)

trt_levels <- paste0("PP", 1:6)
depth_levels <- c("0-10 cm", "10-20 cm", "20-40 cm")
depth_folders <- c("0-10分析结果", "10-20分析结果", "20-40分析结果")
trt_cols <- c(PP1="#4D4D4D",PP2="#6B8FB3",PP3="#5B9E78",
              PP4="#C9A542",PP5="#D79073",PP6="#B95A55")

as_depth <- function(x) factor(
  dplyr::case_when(as.numeric(x)==10 ~ "0-10 cm",as.numeric(x)==20 ~ "10-20 cm",
                   as.numeric(x)==40 ~ "20-40 cm",TRUE ~ NA_character_),
  levels=depth_levels)
num_clean <- function(x) suppressWarnings(as.numeric(as.character(x)))
fmt_p <- function(x) ifelse(is.finite(x), ifelse(x<0.001,"<0.001",sprintf("%.3f",x)),"NA")
scale_0_1 <- function(x) {
  if(all(!is.finite(x))) return(rep(NA_real_,length(x)))
  rg <- range(x, na.rm=TRUE)
  if(diff(rg)==0) return(rep(0.5,length(x)))
  (x-rg[1])/diff(rg)
}
mean_se <- function(x) {
  n <- sum(is.finite(x))
  tibble::tibble(mean=if(n>0) mean(x,na.rm=TRUE) else NA_real_,
                 se=if(n>1) stats::sd(x,na.rm=TRUE)/sqrt(n) else NA_real_, n=n)
}

# Unlike a CLD failure, a valid nonsignificant comparison still returns 'a'.
# LSD has no multiplicity adjustment, reproducing the original figure's test.
lsd_letters_block <- function(df) {
  df <- df %>% dplyr::filter(is.finite(value), !is.na(treatment), !is.na(block)) %>%
    dplyr::mutate(treatment=factor(treatment,levels=trt_levels), block=factor(block))
  blank <- tibble::tibble(treatment=factor(trt_levels,levels=trt_levels), sig=NA_character_)
  err <- function(msg) list(letters=blank, test=tibble::tibble(p_global=NA_real_,
     df_error=NA_real_,method="not_tested",note=msg))
  if(n_distinct(df$treatment)<6 || n_distinct(df$block)<2 || nrow(df)<12)
    return(err("Insufficient independent observations"))
  fit <- tryCatch(stats::aov(value ~ block + treatment,data=df),error=function(e)e)
  if(inherits(fit,"error") || stats::df.residual(fit)<2)
    return(err("Block-adjusted ANOVA failed"))
  an <- stats::anova(fit); mse <- an["Residuals","Mean Sq"]; dferr <- an["Residuals","Df"]
  pglob <- an["treatment","Pr(>F)"]
  if(!is.finite(mse) || mse<=0 || !is.finite(dferr) || dferr<=0)
    return(err("Invalid ANOVA error term"))
  av <- df %>% group_by(treatment) %>% summarise(m=mean(value),n=n(),.groups="drop")
  pairs <- utils::combn(as.character(av$treatment),2,simplify=FALSE)
  pv <- vapply(pairs,function(pr) {
    z1 <- av %>% filter(as.character(treatment)==pr[1]); z2 <- av %>% filter(as.character(treatment)==pr[2])
    tt <- abs(z1$m-z2$m)/sqrt(mse*(1/z1$n+1/z2$n))
    2*stats::pt(-tt,df=dferr)
  },numeric(1))
  names(pv) <- vapply(pairs,paste,collapse="-",FUN.VALUE=character(1))
  L <- tryCatch(multcompView::multcompLetters(pv,threshold=0.05)$Letters,error=function(e)NULL)
  if(is.null(L)) return(err("LSD letter conversion failed"))
  letter_df <- tibble(treatment=factor(names(L),levels=trt_levels),sig=unname(L))
  # Rename letter classes consistently so classes containing high treatment means receive 'a' first.
  old <- unique(unlist(strsplit(letter_df$sig,"",fixed=TRUE)))
  letter_scores <- vapply(old,function(ch) max(av$m[match(as.character(letter_df$treatment)[grepl(ch,letter_df$sig,fixed=TRUE)],
                                         as.character(av$treatment))],na.rm=TRUE),numeric(1))
  old_order <- names(sort(letter_scores,decreasing=TRUE))
  new_names <- c(letters,paste0("a",letters))
  rename_map <- stats::setNames(new_names[seq_along(old_order)],old_order)
  letter_df$sig <- vapply(letter_df$sig,function(s) {
    new <- rename_map[strsplit(s,"",fixed=TRUE)[[1]]]
    paste(new[order(match(new,new_names))],collapse="")
  },character(1))
  list(letters=left_join(blank %>% dplyr::select(treatment),letter_df,by="treatment"),
       test=tibble(p_global=pglob, df_error=dferr,
                   method="ANOVA (block + treatment), unadjusted Fisher LSD",note=""))
}

plot_theme <- function(base=14) theme_bw(base_size=base) + theme(
  panel.grid.major=element_line(colour="grey93",linewidth=.22),panel.grid.minor=element_blank(),
  strip.background=element_rect(fill="grey96",colour="grey75",linewidth=.45),
  strip.text=element_text(face="bold",size=rel(1.02)),
  plot.tag=element_text(face="bold",size=22.2),
  axis.text=element_text(colour="black"),
  plot.margin=margin(6,7,6,7), legend.key=element_blank()
)
save_fig <- function(plot,name,w,h) {
  ggsave(file.path(out_dir,paste0(name,".png")),plot,width=w,height=h,units="in",dpi=1000,bg="white",limitsize=FALSE)
  ggsave(file.path(out_dir,paste0(name,".pdf")),plot,width=w,height=h,units="in",bg="white",limitsize=FALSE)
}

# ---- A: enzyme activity (Excel actually says anthesis, 2024; two blocks) ----
enz_raw <- readxl::read_excel(data_file,sheet="土壤酶活")
enz_columns <- c("脲酶","酸性磷酸酶","蔗糖酶","过氧化氢酶")
stopifnot(all(c("crop","year","date","re","treat","depth",enz_columns)%in%names(enz_raw)))
enz <- enz_raw %>%
  filter(tolower(as.character(crop))=="wheat",num_clean(year)==2024,
         tolower(as.character(date)) %in% c("anthesis","flowering")) %>%
  transmute(year=num_clean(year),stage="anthesis",block=as.character(re),
            treatment=factor(paste0("PP",gsub("[^0-9]","",as.character(treat))),levels=trt_levels),
            depth_layer=as_depth(depth),across(all_of(enz_columns),num_clean)) %>%
  pivot_longer(cols=all_of(enz_columns),names_to="enzyme_original",values_to="value") %>%
  mutate(enzyme=recode(enzyme_original,
         "脲酶"="Urease","酸性磷酸酶"="Acid phosphatase",
         "蔗糖酶"="Sucrase","过氧化氢酶"="Catalase"),
         enzyme=factor(enzyme,levels=c("Urease","Acid phosphatase","Sucrase","Catalase"))) %>%
  filter(!is.na(depth_layer),!is.na(treatment),is.finite(value))
if(nrow(enz)!=144) warning("Enzyme count differs from 36 field samples x 4 enzymes; inspect exports")
enz_groups <- enz %>% group_by(depth_layer,enzyme) %>% group_split()
enz_stats <- lapply(enz_groups,lsd_letters_block)
enz_keys <- enz %>% group_by(depth_layer,enzyme) %>% group_keys()
enz_letters <- bind_rows(lapply(seq_along(enz_stats),function(i)bind_cols(enz_keys[rep(i,6),],enz_stats[[i]]$letters)))
enz_tests <- bind_rows(lapply(seq_along(enz_stats),function(i)bind_cols(enz_keys[i,],enz_stats[[i]]$test)))
enz_summary <- enz %>% group_by(depth_layer,enzyme,treatment) %>%
  summarise(mean_se(value),.groups="drop") %>%
  group_by(depth_layer,enzyme) %>% mutate(relative_activity=scale_0_1(mean)) %>%
  ungroup() %>% left_join(enz_letters,by=c("depth_layer","enzyme","treatment")) %>%
  mutate(label=paste0(format(round(mean,1),nsmall=1,trim=TRUE),"\n",ifelse(is.na(sig),"",sig)))

# FIGURE 3A LAYOUT: vertical colourbar at the RIGHT of the full-width heatmap.
# Text reads top-to-bottom: two-line legend title, then 0.00--1.00 ticks
# alongside the bar, avoiding a long legend floating above the three facets.
pA <- ggplot(enz_summary,aes(x=enzyme,y=treatment,fill=relative_activity)) +
  geom_tile(colour="white",linewidth=.65) +
  geom_text(aes(label=label),size=3.95,lineheight=.94) +
  facet_wrap(~depth_layer,nrow=1) +
  scale_fill_gradient(low="white",high="#57B6C1",limits=c(0,1),
                      breaks=seq(0,1,by=.25),
                      labels=c("0.00","0.25","0.50","0.75","1.00"),
                      name="Relative activity\n(min-max)",na.value="grey90") +
  labs(x=NULL,y=NULL,tag="A") + plot_theme(14.5) +
  theme(axis.text.x=element_text(angle=35,hjust=1,size=13.3),
        axis.text.y=element_text(size=13.4),
        legend.position="right",
        legend.direction="vertical",
        legend.title=element_text(size=12.4,hjust=0,lineheight=1.1),
        legend.text=element_text(size=11.5),
        legend.justification="center",
        legend.box.margin=margin(0,2,0,5)) +
  guides(fill=guide_colourbar(direction="vertical",title.position="top",
                              title.hjust=0,
                              label.position="right",
                              barwidth=grid::unit(.38,"cm"),
                              barheight=grid::unit(3.9,"cm"),
                              ticks=FALSE))

# ---- B/C + Supplementary: find one matching result file per layer ----
roots <- stats::setNames(file.path(microbe_dir,depth_folders),depth_levels)
find_one <- function(root,path_regex) {
  if(!dir.exists(root)) stop("Missing soil-layer analysis folder: ",root)
  files <- list.files(root,recursive=TRUE,full.names=TRUE)
  ff <- gsub("\\\\","/",files)
  hit <- files[grepl(path_regex,ff,ignore.case=TRUE,perl=TRUE)]
  if(length(hit)==0) stop("Missing file matching ",path_regex," under ",root)
  hit[order(nchar(hit),hit)][1]
}
metadata_layer <- function(layer) {
  # Sample numbering follows the ORIGINAL Figure3-5.R and vendor reports.
  # PP1: W1/W2/W3 across the three layers; PP2: W4/W5/W6; ...;
  # PP6: W16/W17/W18. Each W sample has three replicate suffixes -1/-2/-3.
  depth_offset <- match(layer, depth_levels) - 1L
  if (is.na(depth_offset)) stop("Unknown depth layer: ", layer)
  base_number <- c(1L, 4L, 7L, 10L, 13L, 16L) + depth_offset
  d <- tibble::tibble(
    treatment = rep(trt_levels, each = 3L),
    block = rep(1:3, times = 6L),
    k = rep(base_number, each = 3L)
  )
  d %>%
    dplyr::mutate(sample_id = paste0("W", k, "-", block),
                  depth_layer = factor(layer, levels = depth_levels)) %>%
    dplyr::select(-k)
}

read_dist_layer <- function(root,meta) {
  f <- find_one(root,"05\\.Beta/01\\.beta_diversity/Bray_Curtis_distance\\.xls$")
  tab <- utils::read.delim(f,header=TRUE,check.names=FALSE,row.names=1,sep="\t")
  mat <- as.matrix(tab);storage.mode(mat)<-"double"
  wanted <- as.character(meta$sample_id)
  missing_rows <- setdiff(wanted, rownames(mat))
  missing_cols <- setdiff(wanted, colnames(mat))
  if(length(missing_rows) || length(missing_cols)) {
    stop("Incomplete PCoA sample names in ", f,
         "\nLayer expected IDs: ", paste(wanted, collapse=", "),
         "\nMissing row IDs: ", paste(missing_rows, collapse=", "),
         "\nMissing column IDs: ", paste(missing_cols, collapse=", "),
         "\nFile row IDs (first 20): ", paste(head(rownames(mat),20), collapse=", "),
         "\nFile column IDs (first 20): ", paste(head(colnames(mat),20), collapse=", "))
  }
  mat <- mat[wanted,wanted,drop=FALSE]
  if(any(!is.finite(mat)) || any(abs(mat-t(mat))>1e-6) || any(diag(mat)>1e-6))
    stop("Invalid or non-symmetric Bray-Curtis distance matrix: ",f)
  mat
}

read_alpha_layer <- function(root,meta) {
  f <- find_one(root,"02\\.Alpha/.*/Alpha_Index_Samples\\.xls$")
  x <- utils::read.delim(f,check.names=FALSE,sep="\t")
  names(x)[1]<-"sample_id"
  x %>% inner_join(meta,by="sample_id")
}
read_func_layer <- function(root,meta) {
  f <- find_one(root,"09\\.Function_FAPROTAX/02\\.Abundance/Function_sample\\.xls$")
  x <- utils::read.delim(f,check.names=FALSE,sep="\t")
  names(x)[1]<-"function_name"
  x %>% pivot_longer(cols=-function_name,names_to="sample_id",values_to="abundance") %>%
    mutate(abundance=num_clean(abundance)) %>% inner_join(meta,by="sample_id")
}
read_genus_layer <- function(root,meta) {
  f <- find_one(root,"03\\.Community/01\\.tax_summary/Genus\\.xls$")
  x <- utils::read.delim(f,check.names=FALSE,sep="\t")
  names(x)[1]<-"taxon"
  x %>% pivot_longer(cols=-taxon,names_to="sample_id",values_to="abundance") %>%
    mutate(abundance=num_clean(abundance)) %>% inner_join(meta,by="sample_id")
}

micro_metadata <- bind_rows(lapply(depth_levels,metadata_layer))
message("Three-layer sample ID mapping validated (18 samples per layer):")
print(micro_metadata %>% dplyr::group_by(depth_layer, treatment) %>%
        dplyr::summarise(ids = paste(sample_id, collapse=", "), .groups="drop"))
ord_list <- list(); perm_list <- list(); disp_list <- list(); alpha_list <- list();func_list <- list();genus_list <- list()
for(layer in depth_levels) {
  message("Reading microbial data for ",layer)
  meta <- metadata_layer(layer); root <- roots[[layer]]
  m <- read_dist_layer(root,meta)
  ord <- stats::cmdscale(stats::as.dist(m),k=2,eig=TRUE,add=TRUE)
  positive <- ord$eig[ord$eig>0]
  # Explicit columns avoid the as_tibble.matrix() nonunique-name warning.
  pcs <- tibble::tibble(PCoA1 = unname(ord$points[, 1]),
                        PCoA2 = unname(ord$points[, 2]))
  pcs <- bind_cols(meta,pcs) %>% mutate(axis1_pct=100*positive[1]/sum(positive),
       axis2_pct=100*positive[2]/sum(positive))
  perm_spec <- permute::how(nperm=999,within=permute::Within(type="free"),blocks=meta$block)
  # Explicit term-wise PERMANOVA: adonis2 defaults to a whole-model row named
  # 'Model'. That caused the previous R2/P annotation to become NA when indexing
  # a nonexistent 'treatment' row. Here the test is explicitly by="terms".
  per <- vegan::adonis2(stats::as.dist(m) ~ treatment, data=meta,
                        permutations=perm_spec, by="terms")
  trt_row <- which(trimws(rownames(per)) == "treatment")
  if(length(trt_row) != 1L) {
    stop("PERMANOVA treatment term was not found for ", layer,
         "; available rows: ", paste(rownames(per), collapse=", "))
  }
  r2_val <- as.numeric(per[trt_row, "R2"])
  p_val  <- as.numeric(per[trt_row, "Pr(>F)"])
  if(!is.finite(r2_val) || !is.finite(p_val))
    stop("PERMANOVA yielded non-finite R2/P for ", layer,
         "; check distance matrix and sample metadata")
  perm <- tibble(depth_layer=factor(layer,levels=depth_levels),
    R2=r2_val, P=p_val,
    permutations=999,restricted_within_block=TRUE)
  # Check dispersion to avoid mistaking multivariate spread for centroid shifts.
  bet <- vegan::betadisper(stats::as.dist(m),group=meta$treatment,type="median")
  dp <- tryCatch(vegan::permutest(bet,permutations=perm_spec)$tab["Groups","Pr(>F)"],error=function(e)NA_real_)
  disp_list[[layer]] <- tibble(depth_layer=layer,dispersion_p=as.numeric(dp))
  ord_list[[layer]] <- pcs
  perm_list[[layer]] <- perm
  alpha_list[[layer]] <- read_alpha_layer(root,meta)
  func_list[[layer]] <- read_func_layer(root,meta)
  genus_list[[layer]] <- read_genus_layer(root,meta)
}
beta_scores <- bind_rows(ord_list)
beta_permanova <- bind_rows(perm_list)
beta_dispersion <- bind_rows(disp_list)
alpha_raw <- bind_rows(alpha_list)
func_raw <- bind_rows(func_list)
genus_raw <- bind_rows(genus_list)

# pB: bacterial community composition. PCoA coordinates and PERMANOVA
# are from 2024 wheat jointing; each layer has 18 sequenced samples.
pcoa_text <- beta_permanova %>% mutate(
  PCoA1=-Inf,PCoA2=Inf,
  label=paste0("R² = ",sprintf("%.2f",R2),"; ",
               ifelse(P < 0.001, "P < 0.001",
                      paste0("P = ", sprintf("%.3f", P)))))
pB <- ggplot(beta_scores,aes(PCoA1,PCoA2,colour=treatment)) +
  geom_hline(yintercept=0,colour="grey88",linewidth=.3)+
  geom_vline(xintercept=0,colour="grey88",linewidth=.3)+
  geom_point(size=2.55,alpha=.95) +
  geom_text(data=pcoa_text,aes(x=PCoA1,y=PCoA2,label=label),inherit.aes=FALSE,
            hjust=-.03,vjust=1.18,size=3.95,colour="black") +
  facet_wrap(~depth_layer,nrow=1,scales="free") +
  scale_colour_manual(values=trt_cols,drop=FALSE,name="Planting pattern") +
  labs(x="PCoA1",y="PCoA2",tag="B") + plot_theme(13.0) +
  theme(legend.position="bottom",legend.direction="horizontal",
        legend.title=element_text(size=11.4),legend.text=element_text(size=11.6),
        legend.spacing.x=grid::unit(0.15,"cm"),
        strip.text=element_text(face="bold",size=12.7),
        axis.text=element_text(size=11.7),
        axis.title=element_text(size=13.4),
        panel.spacing.x=grid::unit(0.25,"lines")) +
  guides(colour=guide_legend(nrow=1,override.aes=list(size=2.8)))

# pC: FAPROTAX. Exactly the four modules from original Figure3-5.R.
# Some functions (e.g. ammonification) are absent in some/all layers;
# the code includes only those present, and audits the missing terms.
module_map <- tibble::tribble(
 ~module,          ~function_name,
 "N release", "ammonification",
 "N release", "ureolysis",
 "Nitrification",  "nitrification",
 "Nitrification",  "ammonia_oxidation",
 "Nitrification",  "aerobic_ammonia_oxidation",
 "Nitrification",  "nitrite_oxidation",
 "Nitrification",  "aerobic_nitrite_oxidation",
 "Denitrification","denitrification",
 "Denitrification","nitrous_oxide_denitrification",
 "N fixation",    "nitrogen_fixation")
module_levels <- c("N release","Nitrification","Denitrification","N fixation")
module_audit <- tidyr::crossing(depth_layer=factor(depth_levels,levels=depth_levels),module_map) %>%
  left_join(func_raw %>% distinct(depth_layer,function_name) %>% mutate(present=TRUE),
            by=c("depth_layer","function_name")) %>%
  mutate(present=ifelse(is.na(present),FALSE,TRUE))
modules_long <- func_raw %>% inner_join(module_map,by="function_name") %>%
  group_by(depth_layer,treatment,block,sample_id,module) %>%
  summarise(predicted_abundance=sum(abundance,na.rm=TRUE),.groups="drop") %>%
  mutate(module=factor(module,levels=module_levels))
# Stop rather than drawing fabricated zeros for a fully absent module.
missing_modules <- tidyr::crossing(depth_layer=factor(depth_levels,levels=depth_levels),
                                   module=factor(module_levels,levels=module_levels)) %>%
  anti_join(modules_long %>% distinct(depth_layer,module),by=c("depth_layer","module"))
if(nrow(missing_modules)>0) stop("No FAPROTAX annotations for at least one depth-by-module group. See module map and source reports")
modules_summary <- modules_long %>% group_by(depth_layer,treatment,module) %>%
  summarise(mean_se(predicted_abundance),.groups="drop") %>%
  group_by(depth_layer,module) %>% mutate(relative_potential=scale_0_1(mean)) %>% ungroup()
# FIGURE 3C LAYOUT: two guides side-by-side across most of the panel width.
# Both titles remain ONE LINE; the colour bar and circle-key row are enlarged.
# The guide widths below are tuned for Fig3 at 18.0 inches wide. If the full
# figure is resized substantially, adjust C_LEGEND_BAR_CM / C_LEGEND_KEY_CM.
C_LEGEND_BAR_CM <- 7.0   # length of the relative-potential colour bar
C_LEGEND_KEY_CM <- 1.20 # width allocated per predicted-abundance symbol
pC <- ggplot(modules_summary,aes(x=module,y=treatment,fill=relative_potential)) +
  geom_tile(colour="white",linewidth=.6) +
  geom_point(aes(size=mean),colour="grey30",fill="white",shape=21,stroke=.45,alpha=.90) +
  facet_wrap(~depth_layer,nrow=1) +
  scale_fill_gradient(low="#F8FBFD",high="#BE6562",limits=c(0,1),
                      breaks=seq(0,1,by=.25),
                      labels=c("0.00","0.25","0.50","0.75","1.00"),
                      name="Relative potential (min-max)") +
  scale_size_continuous(range=c(1.25,5.0),name="Predicted abundance") +
  labs(x=NULL,y=NULL,tag="C") + plot_theme(13.0) +
  theme(axis.text.x=element_text(angle=43,hjust=1,size=11.3),
        axis.text.y=element_text(size=12.0),
        strip.text=element_text(face="bold",size=12.7),
        panel.spacing.x=grid::unit(0.23,"lines"),
        legend.position="bottom",
        legend.direction="horizontal",
        legend.box="horizontal",
        legend.box.just="center",
        legend.justification="center",
        legend.title=element_text(size=12.0,hjust=0),
        legend.text=element_text(size=11.1),
        legend.spacing.x=grid::unit(.38,"cm"),
        legend.box.spacing=grid::unit(.12,"cm"),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(0,0,0,0)) +
  guides(
    fill=guide_colourbar(title.position="top",title.hjust=0,
                         direction="horizontal",
                         barwidth=grid::unit(C_LEGEND_BAR_CM,"cm"),
                         barheight=grid::unit(.35,"cm"),
                         ticks=FALSE,order=1),
    size=guide_legend(title.position="top",title.hjust=0,
                      direction="horizontal",nrow=1,byrow=TRUE,
                      keywidth=grid::unit(C_LEGEND_KEY_CM,"cm"),
                      keyheight=grid::unit(.58,"cm"),order=2,
                      override.aes=list(fill="white",colour="grey30",alpha=1))
  )

# ---- FINAL Figure 3 ----------------------------------------------------
# Requested layout: A (enzyme heatmap) spans the entire top row; the bottom
# row contains B (Bray-Curtis PCoA) on the left and C (FAPROTAX) on the right.
# The widths/heights below control panels only; supplementary figures unchanged.
# A little extra horizontal room for C helps the widened legend breathe.
bottom_row <- (pB + pC) + patchwork::plot_layout(ncol=2, widths=c(1.02,1.0))
Fig3 <- (pA / bottom_row) + patchwork::plot_layout(heights=c(0.92,1.08))
save_fig(Fig3,"Figure3_STR_enzymes_bacteria_FAPROTAX",18.0,10.4)

# Individual subfigures to check readability at full resolution.
save_fig(pA,"Figure3A_enzymes_only",17.0,4.1)
save_fig(pB,"Figure3B_PCoA_only",9.6,5.4)
save_fig(pC,"Figure3C_FAPROTAX_only",9.6,5.4)

# ---- Supplement S11 REVISED: move old Fig 5A's richness and Shannon here.
alpha_vars <- c("Observed_species","Shannon","Chao1","ACE","Simpson","Pielou_J","Pd_faith")
if(!all(alpha_vars %in% names(alpha_raw))) stop("Alpha diversity columns missing: ",
  paste(setdiff(alpha_vars,names(alpha_raw)),collapse=", "))
alpha_long <- alpha_raw %>% pivot_longer(cols=all_of(alpha_vars),names_to="index",values_to="value") %>%
 mutate(index=factor(index,levels=alpha_vars),treatment=factor(treatment,levels=trt_levels))
alpha_groups <- alpha_long %>% group_by(depth_layer,index) %>% group_split()
alpha_keys <- alpha_long %>% group_by(depth_layer,index) %>% group_keys()
alpha_fits <- lapply(alpha_groups,lsd_letters_block)
alpha_letters <- bind_rows(lapply(seq_along(alpha_fits),function(i)
  bind_cols(alpha_keys[rep(i,6),],alpha_fits[[i]]$letters)))
alpha_tests <- bind_rows(lapply(seq_along(alpha_fits),function(i)
  bind_cols(alpha_keys[i,],alpha_fits[[i]]$test)))
alpha_summary <- alpha_long %>% group_by(depth_layer,index,treatment) %>%
  summarise(mean_se(value),.groups="drop") %>% left_join(alpha_letters,
     by=c("depth_layer","index","treatment"))
# Letters above each treatment box, only if statistic was valid. Keep all 'a'.
alpha_box_top <- alpha_long %>% group_by(depth_layer,index,treatment) %>%
  summarise(top=grDevices::boxplot.stats(value)$stats[5],.groups="drop")
alpha_range <- alpha_long %>% group_by(depth_layer,index) %>%
  summarise(span=max(value,na.rm=TRUE)-min(value,na.rm=TRUE),.groups="drop")
alpha_labels <- alpha_summary %>% left_join(alpha_box_top,by=c("depth_layer","index","treatment")) %>%
  left_join(alpha_range,by=c("depth_layer","index")) %>%
  mutate(y_label=top+pmax(.12*span,.001))
make_alpha_supp <- function(indices,tag) {
  dd <- alpha_long %>% filter(index %in% indices)
  alpha_labs <- alpha_labels %>% filter(index %in% indices)
  ggplot(dd,aes(treatment,value))+
    geom_boxplot(fill="white",colour="#74A9BF",linewidth=.6,width=.58,outlier.shape=NA) +
    geom_point(position=position_jitter(width=.08,height=0),size=1.5,colour="#777777",alpha=.7)+
    geom_text(data=alpha_labs,aes(x=treatment,y=y_label,label=sig),inherit.aes=FALSE,
              size=4.05,na.rm=TRUE)+
    facet_grid(index ~ depth_layer,scales="free_y") +
    scale_y_continuous(expand=expansion(mult=c(.07,.19))) +
    ggplot2::labs(x=NULL,y=NULL,tag=tag)+plot_theme(13.8)+
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=.5,size=12.3),
          strip.text.y=element_text(size=13),legend.position="none")
}
S11a <- make_alpha_supp(c("Observed_species","Shannon"),"A")
S11b <- make_alpha_supp(c("Chao1","ACE","Simpson","Pielou_J","Pd_faith"),"B")
save_fig(S11a,"Supplementary_S11A_alpha_Observed_Shannon",14,6.3)
save_fig(S11b,"Supplementary_S11B_alpha_other_indices",14,13.1)

# ---- New supplement S12B: genus composition originally Figure 5C -------
genus_summary <- genus_raw %>%
  mutate(taxon=ifelse(is.na(taxon)|taxon=="", "Unclassified",as.character(taxon))) %>%
  group_by(depth_layer,treatment,taxon) %>%
  summarise(mean_abundance=mean(abundance,na.rm=TRUE),.groups="drop")
top_genera <- genus_summary %>% group_by(taxon) %>%
 summarise(overall_mean=mean(mean_abundance,na.rm=TRUE),.groups="drop") %>%
 arrange(desc(overall_mean)) %>% slice_head(n=18) %>% pull(taxon)
genus_plot <- genus_summary %>% filter(taxon %in% top_genera) %>%
  group_by(depth_layer,taxon) %>%
  mutate(z=ifelse(sd(mean_abundance)>0,
                  (mean_abundance-mean(mean_abundance))/sd(mean_abundance),0)) %>%
  ungroup() %>% mutate(taxon=factor(taxon,levels=rev(top_genera)))
S12B <- ggplot(genus_plot,aes(treatment,taxon)) +
 geom_point(aes(size=mean_abundance,fill=z),shape=21,colour="grey35",stroke=.3) +
 facet_wrap(~depth_layer,nrow=1) +
 scale_size_continuous(range=c(.7,5.6),name="Mean abundance") +
 scale_fill_gradient2(low="#4D84B1",mid="white",high="#C16E62",midpoint=0,name="z-score") +
 labs(x=NULL,y=NULL)+plot_theme(13.3)+
 theme(axis.text.x=element_text(angle=90,hjust=1,size=11.5),
       axis.text.y=element_text(size=11.5),
       strip.text=element_text(size=13.3),
       legend.title=element_text(size=12.0),
       legend.text=element_text(size=11.2),legend.position="right")
save_fig(S12B,"Supplementary_S12B_top18_bacterial_genera",15.6,8.7)

# ---- Auditable tables --------------------------------------------------
# NOTE n=2 enzyme block observations vs n=3 sequencing samples per treatment x depth.
# Do not pool them; do not use their cross-matrix correlations as synchronous data.
make_csv <- function(x,name) {
  x <- as.data.frame(x)
  for(nm in names(x)) if(is.factor(x[[nm]])) x[[nm]] <- as.character(x[[nm]])
  utils::write.csv(x,file.path(out_dir,paste0("Figure3_",name,".csv")),row.names=FALSE,fileEncoding="UTF-8")
}
exports <- list(
  Enzyme_2024_anthesis_raw=enz,
  Enzyme_mean_SE_letters=enz_summary,
  Enzyme_LSD_global_tests=enz_tests,
  Microbe_sample_metadata=micro_metadata,
  Microbe_PCoA_coordinates=beta_scores,
  Microbe_PERMANOVA=beta_permanova,
  Microbe_dispersion_check=beta_dispersion,
  FAPROTAX_module_map_check=module_audit,
  FAPROTAX_module_samples=modules_long,
  FAPROTAX_module_means=modules_summary,
  Alpha_all_samples=alpha_long,
  Alpha_mean_SE_letters=alpha_summary,
  Alpha_LSD_global_tests=alpha_tests,
  Genus_top18_means=genus_plot
)
for(nm in names(exports)) make_csv(exports[[nm]],nm)
# Optional supplementary tables using existing S1-S6 numbering.
# S7 is publication-oriented; numeric mean, SE and letters remain separately in the full workbook.
table_S7 <- enz_summary %>%
  mutate(cell=ifelse(is.finite(se),
     paste0(sprintf("%.1f",mean)," ± ",sprintf("%.1f",se)," ",dplyr::coalesce(sig,"")),
     paste0(sprintf("%.1f",mean)," (n=1)"))) %>%
  dplyr::select(depth_layer,enzyme,treatment,cell) %>%
  pivot_wider(names_from=treatment,values_from=cell) %>%
  left_join(enz_tests,by=c("depth_layer","enzyme")) %>%
  dplyr::select(depth_layer,enzyme,all_of(trt_levels),p_global,df_error)
make_csv(table_S7,"Supplementary_Table_S7_enzyme_mean_SE_LSD")
table_S8 <- beta_permanova %>%
  left_join(beta_dispersion,by="depth_layer") %>%
  mutate(test="PERMANOVA (within-block permutations); dispersion diagnostic")
make_csv(table_S8,"Supplementary_Table_S8_bacterial_ordination")
writexl::write_xlsx(lapply(c(exports,list(
  Supplementary_Table_S7=table_S7,
  Supplementary_Table_S8=table_S8
)),as.data.frame),
  file.path(out_dir,"Figure3_statistics_and_supplementary_tables.xlsx"))
writeLines(c(
 "Main Fig3A: enzyme activity, wheat ANTHESIS 2024, 2 independent blocks per treatment and depth.",
 "Main Fig3B/C: bacterial community, wheat JOINTING 2024, 3 sequenced samples per treatment and depth.",
 "Enzyme LSD: blocked ANOVA, unadjusted pairwise LSD; missing letters mean model failure, not nonsignificance.",
 "PCoA: Bray-Curtis distance from each depth's vendor file; PERMANOVA 999 permutations restricted by sample block.",
 "FAPROTAX: taxonomically predicted function abundances, NOT measured process rates or fluxes.",
 "Summing annotations within function modules can include non-independent/nested functions; modules are not additive mass fractions.",
 "S11A/B: revised combined alpha-diversity supplement; S12B: descriptive top-18 genera supplement.",
 "Original S10, S12A (phyla), S13, S14 should be checked/retained; see separate supplement plan."
 ),file.path(out_dir,"Figure3_README_statistics.txt"))
cat("\nFigure 3, S11A/B, S12B and statistics saved to:\n",out_dir,"\n")
print(beta_permanova); print(beta_dispersion); print(enz_tests)
