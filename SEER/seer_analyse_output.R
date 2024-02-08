
library(data.table)
library(ggplot2)

#results directory
dir.res <- "..."

rangersf_tuning <- readRDS(file.path(dir.res,"registry_rangersf_5_full_tuningTRUE"))
xgboost_tuning <- readRDS(file.path(dir.res,"registry_xgboost_5_full_tuningTRUE"))

cox_notuning <- readRDS(file.path(dir.res,"registry_cox_5_full_tuningFALSE"))
rangersf_notuning <- readRDS(file.path(dir.res,"registry_rangersf_5_full_tuningFALSE"))
xgboost_notuning <- readRDS(file.path(dir.res,"registry_xgboost_5_full_tuningFALSE"))


create.bs.dt <- function(data.list, tunedYN){
  
  times <- seq(0,156,1)
  
  dt.training <- data.table("times"=times)
  dt.test <- data.table("times"=times)
  dt.all <- data.table("times"=times)
  
  dt.training.long <- melt.data.table(cbind(dt.training,data.list$bs.training), id.vars = "times", variable.name = "sample", value.name = "bs")
  dt.training.long[, "bs.mean" := mean(bs), by = times][, "bs.sd" := sd(bs), by = times]
  dt.training.long[, dataset := "training"][, sample := gsub("bootstrap_", "training_", sample)]
  
  dt.test.long <- melt.data.table(cbind(dt.test,data.list$bs.test), id.vars = "times", variable.name = "sample", value.name = "bs")
  dt.test.long[, "bs.mean" := mean(bs), by = times][, "bs.sd" := sd(bs), by = times]
  dt.test.long[, dataset := "test"][, sample := gsub("bootstrap_", "test_", sample)]
  
  dt.all.long <- melt.data.table(cbind(dt.all,data.list$bs.all), id.vars = "times", variable.name = "sample", value.name = "bs")
  dt.all.long[, "bs.mean" := mean(bs), by = times][, "bs.sd" := sd(bs), by = times]
  dt.all.long[, dataset := "all"][, sample := gsub("bootstrap_", "all_", sample)]
  
  dt.long <- rbind(dt.training.long, dt.test.long, dt.all.long)
  dt.long[, method := data.list$method]
  dt.long[, tuned := ifelse(tunedYN, "Yes", "No")]
  
  return(dt.long)
}

#cox.dt.long.t <- create.bs.dt(cox_tuning)
rangersf.dt.long.t <- create.bs.dt(rangersf_tuning, TRUE)
xgboost.dt.long.t <- create.bs.dt(xgboost_tuning, TRUE)

cox.dt.long.nt <- create.bs.dt(cox_notuning, FALSE)
rangersf.dt.long.nt <- create.bs.dt(rangersf_notuning, FALSE)
xgboost.dt.long.nt <- create.bs.dt(xgboost_notuning, FALSE)

### single plot
##dt.long.t <- rbind(cox.dt.long.t, rangersf.dt.long.t, xgboost.dt.long.t)
#dt.long.t <- rbind(rangersf.dt.long.t, xgboost.dt.long.t)
#dt.long.nt <- rbind(cox.dt.long.nt, rangersf.dt.long.nt, xgboost.dt.long.nt)

#dt.long.t[, tuned := "Yes"]
#dt.long.nt[, tuned := "No"]

#dt.long <- rbind(dt.long.t, dt.long.nt)
#dt.long[method == "rangersf", method := "Random Forest"][method == "cox", method := "Cox"]


#ggplot(dt.long.t) +
#  geom_line(aes(x = times, y = bs, group = sample, linetype = dataset)) +
#  facet_wrap(facets = ~method) +
#  lims(x = c(0,114), y = c(0,0.2)) +
#  theme_classic()

#ggplot(dt.long.t) +
#  geom_line(aes(x = times, y = bs.mean, linetype = dataset)) +
#  facet_wrap(facets = ~method) +
#  lims(x = c(0,114), y = c(0,0.2)) +
#  theme_classic()

#ggplot(dt.long.nt) +
#  geom_line(aes(x = times, y = bs, group = sample, linetype = dataset), alpha = 0.3, show.legend = FALSE) +
#  geom_line(aes(x = times, y = bs.mean, linetype = dataset), size = 0.3) +
#  facet_wrap(facets = ~ method) +
#  lims(x = c(0,114), y = c(0,0.2)) +
#  labs(x = "time", y = "brier score") +
#  theme_classic() #+ 
#  #theme(axis.text = element_text(size = 2),
#  #      axis.line = element_line(size = 1),
#  #      title = element_text(size = 3),
#  #      legend.text = element_text(size = 2),
#  #      strip.text = element_text(size = 3))

#ggplot(dt.long) +
#  geom_line(aes(x = times, y = bs, group = sample, linetype = dataset), alpha = 0.4, size = 0.08, show.legend = FALSE) +
#  geom_line(aes(x = times, y = bs.mean, linetype = dataset), size = 0.2) +
#  lims(x = c(0,114), y = c(0,0.2)) +
#  labs(x = "time", y = "brier score") +
#  facet_wrap(~ method + tuned, labeller = 
#               labeller(
#                 method = ~ paste("Method: ", .),
#                 tuned = ~ paste("Tuned: ", .),
#                 .multi_line = FALSE
#               )
#  ) +
#  theme_classic()

### individual plots
xgboost.dt.long <- rbind(xgboost.dt.long.nt, xgboost.dt.long.t)
xgboost.dt.long[, method := "XGBoost"]
rangersf.dt.long <- rbind(rangersf.dt.long.nt, rangersf.dt.long.t)
rangersf.dt.long[, method := "Random Forest"]

theme.settings <- theme(line = element_line(size = 1),
                        text = element_text(size = 10, family = "serif"),
                        rect = element_rect(size = 1),
                        plot.title = element_text(size = rel(3.5), hjust = 0.5),
                        legend.title = element_text(size = rel(2.75)),
                        legend.text = element_text(size = rel(2.75)),
                        legend.position = "bottom",
                        axis.title = element_text(size = rel(2.75)),
                        axis.text = element_text(size = rel(2.75)),
                        #theme(axis.text.y = element_text(hjust = 0)),
                        strip.text = element_text(size = rel(2.75)),
                        strip.background = element_blank(),
                        panel.grid.major.y = element_line(color = "grey", size = 0.2))

## Cox
ggplot(cox.dt.long.nt) +
  #geom_line(aes(x = times, y = bs, group = sample, linetype = dataset, color = dataset), alpha = 0.45, size = 0.2, show.legend = FALSE) +
  geom_line(aes(x = times, y = bs.mean, linetype = dataset, color = dataset), size = 1.3) +
  scale_linetype_manual(name = "Dataset:", values = c("solid","dashed","dotted"), labels = c("combined","test","training")) +
  scale_color_manual(name = "Dataset:", values = c("#00788C","#B07C4F","#76A165"), labels = c("combined","test","training")) +
  lims(x = c(0,114), y = c(0,0.10)) +
  labs(x = "Time [months]", y = "Brier Score", title = "Cox proportional hazards model") + # with title
  #labs(x = "Time", y = "Brier score") + # no title
  theme_classic() +
  theme.settings

## xgboost
ggplot(xgboost.dt.long) +
  #geom_line(aes(x = times, y = bs, group = sample, linetype = dataset, color = dataset), alpha = 0.45, size = 0.2, show.legend = FALSE) +
  geom_line(aes(x = times, y = bs.mean, linetype = dataset, color = dataset), size = 1.3) +
  scale_linetype_manual(name = "Dataset:", values = c("solid","dashed","dotted"), labels = c("combined","test","training")) +
  scale_color_manual(name = "Dataset:", values = c("#00788C","#B07C4F","#76A165"), labels = c("combined","test","training")) +
  lims(x = c(0,114), y = c(0,0.15)) +
  labs(x = "Time [months]", y = "Brier score", title = "XGBoost") +
  facet_wrap(~ tuned, labeller = 
               labeller(
                 tuned = ~ paste("Tuned: ", .),
                 .multi_line = FALSE
               )
  ) +
  theme_classic() +
  theme.settings
  

## rangersf
ggplot(rangersf.dt.long) +
  #geom_line(aes(x = times, y = bs, group = sample, linetype = dataset, color = dataset), alpha = 0.45, size = 0.2, show.legend = FALSE) +
  geom_line(aes(x = times, y = bs.mean, linetype = dataset, color = dataset), size = 1.3) +
  scale_linetype_manual(name = "Dataset:", values = c("solid","dashed","dotted"), labels = c("combined","test","training")) +
  scale_color_manual(name = "Dataset:", values = c("#00788C","#B07C4F","#76A165"), labels = c("combined","test","training")) +
  lims(x = c(0,114), y = c(0,0.15)) +
  labs(x = "Time [months]", y = "Brier score", title = "Random forest") +
  facet_wrap(~ tuned, labeller = 
               labeller(
                 tuned = ~ paste("Tuned: ", .),
                 .multi_line = FALSE
               )
  ) +
  theme_classic() +
  theme.settings