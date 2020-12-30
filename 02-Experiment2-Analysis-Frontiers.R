################################################################################
# Analysis Script - Experiment 2                                               #
# "Attraction Effects for Verbal Gender and Number Are Similar but Not         #
# Identical: Self-Paced Reading Evidence from Modern Standard Arabic"          #
# M. A. Tucker, A. Idrissi, and D. Almeida                                     #
# Script Author: Diogo Almeida <diogo@nyu.edu,                                 #
#                Matt Tucker <matt.tucker@nyu.edu>                             #
# v. 1, current 29 January 2017                                                #
# v. 2, current 24 December 2020                                               #
# v. 3, current 29 December 2020                                               #
################################################################################
## Loading Packages ------------------------------------------------------------
library(tidyverse)
library(viridis)
library(ragg)
library(pins)
library(fs)
library(here)
library(stargazer)
library(bootES)

## Functions -------------------------------------------------------------------
lamb.winsorize <- function(x, cut.off = 0.01, cut.off.unit = "percent") {
  if (is.list(x)) {
    x <- unlist(x)
  }
  winsorized.x <- x
  if (cut.off.unit == "sd") {
    sdx <- sd(x)
    mx  <- mean(x)
    lowerx <- mx - (cut.off * sdx)
    upperx <- mx + (cut.off * sdx)
  } else {
    if (cut.off.unit == "percent") {
      len.x <- length(x)
      x.ascending <- sort(x)
      g <- trunc(cut.off * len.x)
      lowerx <- x.ascending[g + 1]
      upperx <- x.ascending[len.x - g]
    } else {
      stop("'cut.off.unit' must be 'sd' (standard deviation) or 'percent'!")
    }
  }
  winsorized.x[x <= lowerx] <- lowerx
  winsorized.x[x >= upperx] <- upperx
  return(winsorized.x)
}
lamb.sem <- function(x) {
  if (any(is.na(x))){
    x <- x[!is.na(x)]
  }
  return(sqrt(var(x)/length(x)))
}

## plotting variables ----------------------------------------------------------
manuscript.spr.plot.theme <- theme_bw() + 
  theme(legend.key = element_blank(), 
        plot.background = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(), 
        axis.line = element_line(color = 'black'),
        axis.text.x = element_text(angle = 45, hjust = 1),        
        legend.position="bottom",
        legend.title = element_blank())

spr.rt.width  <-  6.7
spr.rt.height <-  3.7

## Set variable paths for data, figures and tables produced by analysis --------
this_exp <- "exp2"
data.dir <- here(this_exp, "data")
figs.dir <- here(this_exp, "figures")
tbls.dir <- here(this_exp, "tables")
meta.dir <- here("meta")
dir_create(c(data.dir, figs.dir, tbls.dir, meta.dir))

## Getting data ----------------------------------------------------------------
exp2.datafile.figshare <- "https://ndownloader.figshare.com/files/25914753"
pin(exp2.datafile.figshare, name = "experiment2_data")
exp2.original <- pin_get("experiment2_data") %>% 
  readr::read_csv()

## Experiment 2a and 2b
Exp2.A <- paste0("S", 1:128)

## Saving a copy of the original dataset on the data folder --------------------
exp2.original %>%
  readr::write_csv(here(data.dir, "experiment2.csv"))

## Set parameters for data analysis --------------------------------------------
wins.cutoff  <- .01   # 1% cutoff
error.cutoff <- .5    # Original Analysis = .5; Alternative = .67 (roughly 2/3)

## Regions of Interest for Data Analysis ---------------------------------------
rois <- c("Verb", "Verb+1", "Verb+2")

exp2.regions <- c("NP Subj", "Comp", "RC Verb", "Attr", "Adverb", "Verb", 
                  "Verb+1", "Verb+2")

## Exclude Fillers, Incorrect trials, NAs, and Regions after Verb+2 ------------
## Introduce Experiment and SubExperiment factors
exp2.data.ok <- exp2.original %>%
  mutate(Experiment = factor(rep("Exp2", times = length(SubjID))),
         SubExperiment = factor(if_else(SubjID %in% Exp2.A, "A", "B")))

## Error rates -----------------------------------------------------------------
## Matt's code to get error rates by experimental manipulation vs. filler. 
## I had to hardcode the subject exclusion in order to hook it into this code.
## I'm also doing this only for B to save time (we already have A)
by_exp <- filter(exp2.data.ok, SubExperiment == "B" & SubjID != "S231") %>% group_by(ExpID)
exp2.data.errors <- by_exp %>% summarize(Accuracy = mean(Correct, na.rm = TRUE))

exp2.data.ok <- exp2.data.ok %>% filter(Condition != "Filler" & Region %in% exp2.regions & !is.na(RT))

## get the percentage of data excluded for incorrect answers -------------------
tmp <- filter(exp2.data.ok, SubExperiment == "B")
ct <- nrow(tmp)
tmp <- tmp %>% filter(Correct == 1, SubExperiment == "B")
ct <- 1 - nrow(tmp)/ct

## Get only the correct responses ----------------------------------------------
exp2.data.ok <- exp2.data.ok %>% filter(Correct == 1)

## Winsorizing the data at 1% --------------------------------------------------
exp2.data.ok <- exp2.data.ok %>%
  group_by(SubExperiment, Condition, Region) %>%
  mutate(RTw01 = lamb.winsorize(RT, cut.off = 0.01, 
                                cut.off.unit = "percent")) %>%
  ungroup()

## Subject averages ------------------------------------------------------------
exp2.data.saves <- exp2.data.ok %>% 
  group_by(SubExperiment, SubjID, Condition, Region) %>%
  summarise(meanRT = mean(RTw01, na.rm = TRUE)) %>%
  ungroup()

## Find subjects who had empty cells -------------------------------------------
## i.e., who had conditons in which no average data can be computed given our
## exclusion criteria
exp2.subjects.empty.cells <- exp2.data.saves %>%
  group_by(SubExperiment, Region, SubjID) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  filter(count < 8) %>% 
  select(SubjID) %>%
  distinct() %>%
  unlist() %>%
  as.vector()

## Excluding participants that did not meet the error rate threshold -----------
exp2.original %>%
  filter(Condition != "Filler" & Region %in% exp2.regions &
           SubjID %in% Exp2.A) %>%
  group_by(SubjID) %>%
  summarise(MeanCorrect = mean(Correct, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(MeanCorrect < .5) %>%
  summarise(count = n())

exp2.bad.subjects <- exp2.original %>%
  filter(Condition != "Filler" & Region %in% exp2.regions) %>%
  group_by(SubjID) %>%
  summarise(MeanCorrect = mean(Correct, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(MeanCorrect < .5) %>%
  select(SubjID)

## Excluding subjects with empty cells and who answered less than 50% correct --
exp2.data.ok <- exp2.data.ok %>%
  filter(!SubjID %in% exp2.bad.subjects) %>%
  droplevels()
exp2.data.saves <- exp2.data.saves %>%
  filter(!SubjID %in% exp2.subjects.empty.cells &
           !SubjID %in% exp2.bad.subjects) %>%
  droplevels()

## Creating grand averages for plotting ----------------------------------------
## Annotating grand averages data set for plotting
exp2.data.gdave <- exp2.data.saves %>%
  group_by(SubExperiment, Condition, Region) %>%
  summarise(gd.avg.RT = mean(meanRT, na.rm = TRUE),
            gd.avg.RT.SE = lamb.sem(meanRT),
            se.min = gd.avg.RT - gd.avg.RT.SE,
            se.max = gd.avg.RT + gd.avg.RT.SE) %>%
  separate(Condition, into = c("SubjPhi", "Match", "Grammaticality"), sep = "/", 
           remove = FALSE) %>%
  ungroup() %>%
  mutate(Condition, MatchGramCondition = factor(paste(Match, Grammaticality, sep = "/"))) %>%
  mutate(across(where(is.character), factor)) %>%
  rename(SubjectPhiFeature = SubjPhi) %>%
  mutate(SubjPhi = factor(SubjectPhiFeature, levels = c("Masc", "Fem")),
         Experiment = factor(rep("Exp2", times = length(SubjPhi)))) %>%
  droplevels()

## Creating labeller for plotting ----------------------------------------------
exp2.lookup.phi <- c(Masc = "Masculine", Fem = "Feminine") 
exp2.lookup.exp <- c(A = "2A", B = "2B") 
exp2.labeller <- labeller(
  SubExperiment = exp2.lookup.exp,
  SubjPhi = exp2.lookup.phi,
  .default = label_both
)

## Means & Variances Tables for LaTeX ------------------------------------------
regions.for.table <- c("Verb", "Verb+1", "Verb+2")
exp2a.means.latex <- exp2.data.gdave %>%
  filter(Region %in% regions.for.table & SubExperiment == "A") %>% 
  select(Condition, Region, gd.avg.RT, gd.avg.RT.SE) %>%
  transmute(Cond = as.character(Condition),
            Region = Region,
            Average = round(gd.avg.RT, 0),
            SE = round(gd.avg.RT.SE, 0)) %>%
  arrange(Region)
colnames(exp2a.means.latex) <- c("Condition", "Region", "Mean", "SE")

exp2a.means.latex %>% select(Condition, Mean, SE) %>%
  stargazer::stargazer(summary = FALSE, rownames = FALSE, keep = c(1, 2, 3), 
                       out = here(tbls.dir, "exp2a-means.tex"))

exp2b.means.latex <- exp2.data.gdave %>%
  filter(Region %in% regions.for.table & SubExperiment == "B") %>% 
  select(Condition, Region, gd.avg.RT, gd.avg.RT.SE) %>%
  transmute(Cond = as.character(Condition),
            Region = Region,
            Average = round(gd.avg.RT, 0),
            SE = round(gd.avg.RT.SE, 0)) %>%
  arrange(Region)
colnames(exp2b.means.latex) <- c("Condition", "Region", "Mean", "SE")

exp2b.means.latex %>% select(Condition, Mean, SE) %>%
  stargazer::stargazer(summary = FALSE, rownames = FALSE, keep = c(1, 2, 3),
                       out = here(tbls.dir, "exp2b-means.tex"))


## Grand Average Plotting ------------------------------------------------------
exp2.grand.average.plot <- ggplot(exp2.data.gdave, 
                                  aes(x = Region, y = gd.avg.RT, 
                                      group = MatchGramCondition, 
                                      colour = MatchGramCondition))

plot.manuscript.all <- exp2.grand.average.plot +
  labs(x = "Region", y = "Raw RT (ms)") + 
  geom_line(aes(linetype = MatchGramCondition)) + 
  geom_point(aes(shape = MatchGramCondition), size = 2) +
  geom_errorbar(aes(ymax = se.max, ymin = se.min), width=0.1) + 
  facet_grid(SubjPhi ~ SubExperiment, labeller = exp2.labeller, 
             scales = "free_y") + 
  scale_shape_manual(values = c("Match/Gram" = 15, 
                                "Match/Ungram" = 15,
                                "NoMatch/Gram" = 0, 
                                "NoMatch/Ungram" = 0)) + 
  scale_linetype_manual(values = c("NoMatch/Gram" = 1, 
                                   "Match/Gram" = 1,
                                   "NoMatch/Ungram" = 2, 
                                   "Match/Ungram" = 2)) + 
  scale_colour_manual(values = c("NoMatch/Gram" = viridis_pal()(10)[7], 
                                 "Match/Gram" = viridis_pal()(10)[7], 
                                 "NoMatch/Ungram" = viridis_pal()(10)[2], 
                                 "Match/Ungram" = viridis_pal()(10)[2])) +
  manuscript.spr.plot.theme + guides(shape = guide_legend(nrow = 1, 
                                                          byrow = TRUE)) + 
  annotate("rect", xmin = 5.5, xmax = 8.5, ymin = -Inf, ymax = Inf, alpha = 0.2,
           fill = "grey")

## Save the plots --------------------------------------------------------------
ggsave(plot.manuscript.all, 
       file = here(figs.dir, "exp2.pdf"),
       width = spr.rt.width, height = spr.rt.height)
ggsave(plot.manuscript.all, 
       file = here(figs.dir, "exp2.eps"),
       width = spr.rt.width, height = spr.rt.height, device = cairo_ps)
agg_png(filename = here(figs.dir, "exp2.png"),
        width = spr.rt.width, height = spr.rt.height, units = "in",
        res = 216)
print(plot.manuscript.all)
dev.off()

## Mean and bootstrapped CIs for the effects of interest -----------------------
## Effect 1: Attraction in Ungrammatical Sentences
## Effect 2: Attraction in Grammatical Sentences
## Effect 3: Grammaticality Effect
## -----------------------------------------------
## Using a specific seed makes this reproducible
## we did not use a specific seed for the original analysis so the exact CIs
## will not be identical to the ones reported in the paper.
set.seed(1234) 
exp2.reformatted.for.cis <- exp2.data.saves %>%
  mutate(Experiment = factor(rep("Exp2", times = length(SubjID)))) %>%
  separate(Condition, into = c("SubjPhi", "Match", "Grammaticality")) %>%
  unite(., "Condition", Match, Grammaticality) %>%
  pivot_wider(names_from = Condition, values_from = meanRT)

## Effect 1: Attraction in Ungrammatical Sentences
exp2.attr.ungram <-  exp2.reformatted.for.cis %>%
  mutate(IntrusionU = Match_Ungram - NoMatch_Ungram) %>%
  group_by(Experiment, SubExperiment, SubjPhi, Region) %>%
  summarise(MeanEffect = mean(IntrusionU),
            SDEffect = sd(IntrusionU),
            N = n(),
            VarEffect = (SDEffect^2)/N,
            StudyWeightEffect = 1 / VarEffect,
            CIEffect = paste(bootES(IntrusionU)$bounds, collapse = "/")) %>%
  mutate(Exp = factor(paste(Experiment, SubExperiment, sep = "."))) %>%
  separate(CIEffect, into = c("CI.min", "CI.max"), sep = "/", convert= TRUE) %>%
  mutate(EffectType = rep("01_Attraction_Ungrammatical", 
                          times = length(Exp))) %>%
  ungroup()


## Effect 2: Attraction in Grammatical Sentences
exp2.attr.gram <-  exp2.reformatted.for.cis %>%
  mutate(IntrusionU = Match_Gram - NoMatch_Gram) %>%
  group_by(Experiment, SubExperiment, SubjPhi, Region) %>%
  summarise(MeanEffect = mean(IntrusionU),
            SDEffect = sd(IntrusionU),
            N = n(),
            VarEffect = (SDEffect^2)/N,
            StudyWeightEffect = 1 / VarEffect,
            CIEffect = paste(bootES(IntrusionU)$bounds, collapse = "/")) %>%
  mutate(Exp = factor(paste(Experiment, SubExperiment, sep = "."))) %>%
  separate(CIEffect, into = c("CI.min", "CI.max"), sep = "/", convert= TRUE) %>%
  mutate(EffectType = rep("02_Attraction_Grammatical", times = length(Exp))) %>%
  ungroup()

## Effect 3: Grammaticality Effect
exp2.grammaticality <-  exp2.reformatted.for.cis %>%
  mutate(GrammaticalityEffect = (Match_Ungram + NoMatch_Ungram) - 
           (Match_Gram + NoMatch_Gram)) %>%
  group_by(Experiment, SubExperiment, SubjPhi, Region) %>%
  summarise(MeanEffect = mean(GrammaticalityEffect),
            SDEffect = sd(GrammaticalityEffect),
            N = n(),
            VarEffect = (SDEffect^2)/N,
            StudyWeightEffect = 1 / VarEffect,
            CIEffect = paste(bootES(GrammaticalityEffect)$bounds, 
                             collapse = "/")) %>%
  mutate(Exp = factor(paste(Experiment, SubExperiment, sep = "."))) %>%
  separate(CIEffect, into = c("CI.min", "CI.max"), sep = "/", convert= TRUE) %>%
  mutate(EffectType = rep("03_Grammaticality", times = length(Exp))) %>%
  ungroup()

## Put all effects together
exp2.effects <- bind_rows(exp2.attr.ungram, exp2.attr.gram, exp2.grammaticality)

## LaTeX tables for Effects and their Bootstrapped 95% CIs ---------------------
exp2.effects %>%
  filter(Region %in% rois) %>%
  mutate(across(where(is.factor), as.character)) %>%
  group_by(SubExperiment) %>%
  mutate(SE = sqrt(VarEffect),
         Mean = paste0(round(MeanEffect, 0), " (", round(CI.min, 0), ", ", 
                       round(CI.max, 0), ")")) %>%
  ungroup() %>%
  select(Region, SubExperiment, EffectType, SubjPhi, Mean) %>%
  droplevels() %>%
  group_by(SubExperiment, EffectType) %>%
  arrange(SubExperiment, .by_group = TRUE) %>%
  ungroup() %>%
  group_by(EffectType, SubjPhi) %>%
  arrange(SubExperiment, EffectType, desc(SubjPhi)) %>%
  ungroup() %>%
  pivot_wider(names_from = Region, values_from = Mean) %>%
  stargazer::stargazer(type = "latex", summary = FALSE, rownames = FALSE,
                       out=here(tbls.dir, "exp2-cis.tex"))

## Save the relevant data structures for future manipulation -------------------
save(exp2.data.ok, exp2.data.saves, exp2.data.gdave, exp2.effects,
     file = here(meta.dir, "Experiment2-DataForMetaAnalysis.RData"))

## Clean environment -----------------------------------------------------------
rm(list = ls())
