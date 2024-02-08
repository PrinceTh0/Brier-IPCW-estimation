
#load packages
library(ggplot2)
library(ggpubr)
library(xtable)

#path to data
data.path <- "..."


#check value of R2, censoring rate
check.settings <- function(sim.parameter, data.path, sim.parameter.type, other = NULL){
  
  require(data.table)
  require(ggplot2)
  require(ggpubr)
  
  file.list <- list.files(data.path)
  if(is.null(other)){
    files <- file.list[grepl(paste0("_",sim.parameter,"_"),file.list)]
  }else if(!is.null(other)){
    files <- file.list[grepl(paste0(other),file.list[grepl(paste0("_",sim.parameter,"_"),file.list)])]
  }

  parameter.mean.dt <- data.table()
  parameter.sd.dt <- data.table()
  parameter.plot.dt <- data.table()
  n.samples.dt <- data.table()

  variables <- c("%cens_data","%cens_training","%cens_test","R2")
  
  for(file in files){
    
    coll.data <- readRDS(file = file.path(data.path, file))
    
    if(is.null(other)){
      sim.parameter.value <- gsub(paste0(sim.parameter,"_"),"",gsub("registry_","",file))
    }else if(!is.null(other)){
      sim.parameter.value <- gsub(other,"",gsub(paste0(sim.parameter,"_"),"",gsub("registry_","",file)))
    }
    
    if(sim.parameter.type == "numeric"){
      n.samples <- data.table("sim.parameter" = sim.parameter, "sim.parameter.value" = as.numeric(sim.parameter.value), "n.samples" = coll.data$length.output)
    }else{
      n.samples <- data.table("sim.parameter" = sim.parameter, "sim.parameter.value" = sim.parameter.value, "n.samples" = coll.data$length.output)
    }
    n.samples.dt <- rbind(n.samples, n.samples.dt)
    
    parameter.mean <- coll.data$sample.parameters[, lapply(.SD, mean), .SD = variables][, setnames(.SD, variables, paste0(variables,"_mean"))]
    if(sim.parameter.type == "numeric"){
      parameter.mean[, (sim.parameter) := as.numeric(sim.parameter.value)]
    }else{
      parameter.mean[, (sim.parameter) := sim.parameter.value]
    }
    parameter.mean.dt <- rbind(parameter.mean.dt,parameter.mean)
    
    parameter.sd <- coll.data$sample.parameters[, lapply(.SD, sd), .SD = variables][, setnames(.SD, variables, paste0(variables,"_sd"))]
    if(sim.parameter.type == "numeric"){
      parameter.sd[, (sim.parameter) := as.numeric(sim.parameter.value)]
    }else{
      parameter.sd[, (sim.parameter) := sim.parameter.value]
    }
    parameter.sd.dt <- rbind(parameter.sd.dt,parameter.sd)
    
    parameter.plot <- coll.data$sample.parameters[, ..variables]
    if(sim.parameter.type == "numeric"){
      parameter.plot[, (sim.parameter) := as.numeric(sim.parameter.value)]
    }else{
      parameter.plot[, (sim.parameter) := sim.parameter.value]
    }
    parameter.plot.dt <- rbind(parameter.plot.dt, parameter.plot)
  
  }
  
  parameter.plot.dt[, (sim.parameter) := lapply(.SD, function(x){as.factor(x)}), .SD = (sim.parameter)]
  
  for(var in variables){

    y <- sym(var)
  
    plot.temp <- ggplot(parameter.plot.dt) +
      geom_boxplot(aes(y=!!y)) +
      #geom_boxplot(aes(x=parameter.plot.dt[[sim.parameter]],y=`%cens_training`)) +
      facet_wrap(sym(sim.parameter), scales = "free_x", labeller = label_both) +
      labs(x = NULL, y = var) +
      scale_x_discrete(breaks = NULL) +
      scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.1), labels = seq(0,1,0.1)) +
      theme_classic()
    
    assign(paste0("plot.",gsub("%","",var)), plot.temp)
    
  }
  
  plot.grid <- ggarrange(plot.cens_data,plot.cens_training,plot.cens_test,plot.R2)
  
  return(list("settings.mean" = parameter.mean.dt,
              "settings.sd" = parameter.sd.dt,
              "plot.grid" = plot.grid,
              "plot.cens.data" = plot.cens_data,
              "plot.cens.training" = plot.cens_training,
              "plot.cens.test" = plot.cens_test,
              "plot.R2" = plot.R2,
              "n.samples" = n.samples.dt))
}


#check.n.per.ds <- check.settings("n.per.ds",data.path,"numeric")

#check.n_training <- check.settings("n_training",data.path,"numeric")

#check.n_test <- check.settings("n_test",data.path,"numeric")

#check.offset <- check.settings("offset",data.path,"numeric")

#check.rate <- check.settings("rate",data.path,"numeric")

#check.cens_estimator <- check.settings("cens_estimator",data.path,"character")

#check.estimator <- check.settings("estimator",data.path,"character")

#check.n.per.ds.cox <- check.settings("n.per.ds",data.path,"numeric", "_cens_estimator_cox")
#check.n.per.ds.km <- check.settings("n.per.ds",data.path,"numeric", "_cens_estimator_km")


facet.plot <- function(sim.parameter, data.path, sim.parameter.type, other = NULL){
  
  require(data.table)
  require(ggplot2)
  
  file.list <- list.files(data.path)
  if(is.null(other)){
    files <- file.list[grepl(paste0("_",sim.parameter,"_"),file.list)]
  }else if(!is.null(other)){
    files <- file.list[grepl(paste0(other),file.list[grepl(paste0("_",sim.parameter,"_"),file.list)])]
  }
  
  wse.dt <- data.table()
  wse.wide.dt <- data.table()
  bs.dt <- data.table()
  times.obs.dt <- data.table()
  
  plot.list <- list()
  
  for(file in files){
    
    coll.data <- readRDS(file = file.path(data.path, file))
    
    if(is.null(other)){
      sim.parameter.value <- gsub(paste0(sim.parameter,"_"),"",gsub("registry_","",file))
    }else if(!is.null(other)){
      sim.parameter.value <- gsub(other,"",gsub(paste0(sim.parameter,"_"),"",gsub("registry_","",file)))
    }
    
    wse.training <- data.table("wse"=coll.data$wse.training,"dataset"="training")
    wse.test <- data.table("wse"=coll.data$wse.test,"dataset"="test")
    wse.all <- data.table("wse"=coll.data$wse.all,"dataset"="all")
    
    wse <- rbind(wse.training,wse.test,wse.all)
    
    if(sim.parameter.type == "numeric"){
      wse[, (sim.parameter) := as.numeric(sim.parameter.value)]
    }else{
      wse[, (sim.parameter) := sim.parameter.value]
    }
    wse.dt <- rbind(wse.dt, wse)
    
    wse.wide <- data.table("training"=coll.data$wse.training,
                              "test"=coll.data$wse.test,
                              "all"=coll.data$wse.all)
    
    if(sim.parameter.type == "numeric"){
      wse.wide[, (sim.parameter) := as.numeric(sim.parameter.value)]
    }else{
      wse.wide[, (sim.parameter) := sim.parameter.value]
    }
    
    p <- ggplot(wse.wide) +
      geom_point(aes(x=test, y=training)) +
      lims(x=c(0,max(c(wse.wide$training,wse.wide$test))), y=c(0,max(c(wse.wide$training,wse.wide$test)))) +
      ggtitle(label = paste0(sym(sim.parameter)," = ", sim.parameter.value)) +
      coord_fixed() +
      theme_classic()
    
    append(plot.list, p)
    
    wse.wide.dt <- rbind(wse.wide.dt, wse.wide)
    
    bs.obs.training <- copy(coll.data$brier.training)
    bs.obs.test <- copy(coll.data$brier.test)
    bs.obs.all <- copy(coll.data$brier.all)
    
    bs.obs.training[, training_bs_mean := apply(.SD, 1, function(x){mean(x, na.rm = T)})]
    bs.obs.test[, test_bs_mean := apply(.SD, 1, function(x){mean(x, na.rm = T)})]
    bs.obs.all[, all_bs_mean := apply(.SD, 1, function(x){mean(x, na.rm = T)})]
    
    bs.obs.training[, training_bs_sd := apply(.SD, 1, function(x){sd(x, na.rm = T)})]
    bs.obs.test[, test_bs_sd := apply(.SD, 1, function(x){sd(x, na.rm = T)})]
    bs.obs.all[, all_bs_sd := apply(.SD, 1, function(x){sd(x, na.rm = T)})]
    
    bs.obs <- cbind(setnames(coll.data$ref.times, "ref_times"), bs.obs.training, bs.obs.test, bs.obs.all)
    
    bs.uncensored <- copy(coll.data$brier.uncensored)
    bs.uncensored <- bs.uncensored[, uncensored_bs_mean := apply(.SD, 1, function(x){mean(x, na.rm = T)})]
    bs.uncensored <- bs.uncensored[, uncensored_bs_sd := apply(.SD, 1, function(x){sd(x, na.rm = T)})][,.(uncensored_bs_mean, uncensored_bs_sd)]
    
    bs.mean <- cbind(bs.obs, bs.uncensored)
    bs.mean <- melt(setnames(bs.mean[,c("ref_times","training_bs_mean","test_bs_mean","all_bs_mean","uncensored_bs_mean")], c("ref_times","training","test","all","uncensored")), id.vars = "ref_times", variable.name = "dataset", value.name = "bs")
    
    bs.sd <- cbind(bs.obs, bs.uncensored)
    bs.sd <- melt(setnames(bs.sd[,c("ref_times","training_bs_sd","test_bs_sd","all_bs_sd","uncensored_bs_sd")], c("ref_times","training","test","all","uncensored")), id.vars = "ref_times", variable.name = "dataset", value.name = "bs_sd")
    
    bs <- merge.data.table(bs.mean, bs.sd, by = c("ref_times","dataset"))
    
    if(sim.parameter.type == "numeric"){
      bs[, (sim.parameter) := as.numeric(sim.parameter.value)]
    }else{
      bs[, (sim.parameter) := sim.parameter.value]
    }
    bs.dt <- rbind(bs.dt, bs)
    
    times.obs.training <- melt.data.table(coll.data$obs.times.training, id.vars = NULL, variable.name = "dataset", value.name = "Ttilde")[, dataset := "training"][order(Ttilde)]
    times.obs.test <- melt.data.table(coll.data$obs.times.test, id.vars = NULL, variable.name = "dataset", value.name = "Ttilde")[, dataset := "test"][order(Ttilde)]
    
    times.obs <- rbind(times.obs.training, times.obs.test)
    times.obs[, dataset := as.factor(dataset)]
    if(sim.parameter.type == "numeric"){
      times.obs[, (sim.parameter) := as.numeric(sim.parameter.value)]
    }else{
      times.obs[, (sim.parameter) := sim.parameter.value]
    }
    
    times.obs[, q75 := quantile(Ttilde, 0.75), by = c(sim.parameter, "dataset")]
    times.obs[, q95 := quantile(Ttilde, 0.95), by = c(sim.parameter, "dataset")]
    times.obs[, q99 := quantile(Ttilde, 0.99), by = c(sim.parameter, "dataset")]
    
    times.obs <- unique(times.obs[, -c("Ttilde")])
    times.obs <- melt(times.obs, by = c(sim.parameter,"dataset"), id.vars = c(sim.parameter,"dataset"))
    times.obs.dt <- rbind(times.obs.dt, times.obs)
    
    #bs.dt <- merge.data.table(bs.dt, times.obs.dt, by = c(sim.parameter,"dataset"), all.x = T)
    
  }
  
  #newlevels <- unique(wse.dt[[sim.parameter]])[order(unique(wse.dt[[sim.parameter]]))]
  wse.dt[, (sim.parameter) := lapply(.SD, function(x){as.factor(x)}), .SD = (sim.parameter)]
  wse.wide.dt[, (sim.parameter) := lapply(.SD, function(x){as.factor(x)}), .SD = (sim.parameter)]
  bs.dt[, (sim.parameter) := lapply(.SD, function(x){as.factor(x)}), .SD = (sim.parameter)]
  times.obs.dt[, (sim.parameter) := lapply(.SD, function(x){as.factor(x)}), .SD = (sim.parameter)]
  
  plot.wse <- ggplot(wse.dt) +
    geom_violin(aes(x=factor(dataset,ordered=T,levels=c("training","test","all")),y=wse, fill = dataset), show.legend = F, draw_quantiles = T, size = 0.2) +
    labs(x="fit dataset", y = "wse") +
    scale_fill_manual(values = c("#0072b2","#d55e00","#f0e442")) +
    lims(y=c(0,0.5)) +
    facet_wrap(sym(sim.parameter), scales = "free_x", labeller = label_both) +
    theme_classic()
  
  plot.wse.scatter <- ggplot(wse.wide.dt) +
    geom_point(aes(x=test, y=training, color = wse.wide.dt[[sim.parameter]]), show.legend = F) +
    facet_wrap(sym(sim.parameter), scales = "free", labeller = label_both) +
    #lims(x = c(0,max(wse.wide.dt.min.max$max_test,wse.wide.dt.min.max$max_training)), y = c(0,max(wse.wide.dt.min.max$max_test,wse.wide.dt.min.max$max_training))) +
    #lims(x=c(0,0.5), y=c(0,0.5)) +
    theme_classic()
  
  plot.bs <- ggplot(bs.dt, aes(x = ref_times)) +
    geom_line(aes(y = bs, color = dataset)) +
    geom_ribbon(aes(y = bs, ymin = pmax(0,(bs - bs_sd)), ymax = bs + bs_sd, fill = dataset), alpha = 0.3) +
    scale_color_manual(values = c("#f0e442","#d55e00","#0072b2","#009e73")) +
    scale_fill_manual(values = c("#f0e442","#d55e00","#0072b2","#009e73")) +
    labs(x = "time", y = "brier score") +
    facet_wrap(sym(sim.parameter), scales = "free_x", labeller = label_both) +
    geom_vline(data = times.obs.dt, aes(xintercept = value, linetype = variable, color = dataset)) +
    scale_linetype_manual(values = c("solid","dotdash","dotted"), name = "time quantiles") +
    lims(x=c(0,7),y=c(0,NA)) +
    theme_classic()
  
  return(list("wse" = plot.wse, 
              "bs" = plot.bs, 
              "wse.scatter" = plot.wse.scatter, 
              "wse.dt" = wse.dt, 
              "wse.wide.dt" = wse.wide.dt, 
              "bs.dt" = bs.dt
              ))
  
}

##set theme
theme.settings <- theme(line = element_line(size = 1),
                        text = element_text(size = 10, family = "serif"),
                        rect = element_rect(size = 1),
                        plot.title = element_text(size = rel(3), hjust = 0.5),
                        legend.title = element_text(size = rel(2.75)),
                        legend.text = element_text(size = rel(2.75)),
                        axis.title = element_text(size = rel(2.75)),
                        axis.text = element_text(size = rel(2.75)),
                        strip.text = element_text(size = rel(3)),
                        strip.background = element_blank(),
                        panel.grid.major.y = element_line(color = "grey", size = 0.2))


# n per ds
plot.n.per.ds <- facet.plot("n.per.ds", data.path, "numeric")

plot.n.per.ds$wse.dt[, n.total := as.factor(2*as.numeric(levels(n.per.ds))[n.per.ds])]

wse.table.dt.o1.a <- plot.n.per.ds$wse.dt[, .(wse.median = quantile(wse, 0.5, na.rm = TRUE), wse.iqr = IQR(wse, na.rm = TRUE)), by = .(n.total, dataset)][, n.total := as.numeric(levels(n.total))[n.total]][dataset == "all", dataset := "combined"]
setnames(wse.table.dt.o1.a, names(wse.table.dt.o1.a),c("n","dataset","median","IQR"))
setorder(wse.table.dt.o1.a,n,dataset)
print(xtable(wse.table.dt.o1.a, caption = c("numerical results for the Weibull-Cox scenario (ii) with varying n"), align = c("c","l","l","c","c"), digits = c(0,0,0,6,6)), include.rownames = FALSE)

p.n.per.ds <- ggplot(plot.n.per.ds$wse.dt) +
  geom_violin(aes(x=n.total,y=log(wse), fill = dataset), show.legend = T, draw_quantiles = c(0.5)) +
  scale_fill_manual(values = c("#00788C","#B07C4F","#76A165"), labels = c("combined", "test", "training"), name = "Fitting dataset:") +
  labs(x="Total size of the dataset", y="log(WSE)") +
  #lims(y=c(0,0.5)) +
  theme_classic() +
  theme.settings + 
  theme(plot.margin = unit(c(0.7,0.7,0.7,0.7), "cm"))


# n training
plot.n_training <- facet.plot("n.training", data.path, "numeric")

wse.table.dt.o1.b <- plot.n_training$wse.dt[, .(wse.median = quantile(wse, 0.5, na.rm = TRUE), wse.iqr = IQR(wse, na.rm = TRUE)), by = .(n.training, dataset)][, n.training := as.numeric(levels(n.training))[n.training]][dataset == "all", dataset := "combined"]
setnames(wse.table.dt.o1.b, names(wse.table.dt.o1.b),c("n.training","dataset","median","IQR"))
setorder(wse.table.dt.o1.b,n.training,dataset)
print(xtable(wse.table.dt.o1.b, caption = c("numerical results for the Weibull-Cox scenario (ii) with varying n.training"), align = c("c","l","l","c","c"), digits = c(0,0,0,6,6)), include.rownames = FALSE)


p.n.training <- ggplot(plot.n_training$wse.dt) +
  geom_violin(aes(x=n.training,y=log(wse), fill = dataset), show.legend = T, draw_quantiles = c(0.5)) +
  scale_fill_manual(values = c("#00788C","#B07C4F","#76A165"), labels = c("combined", "test", "training"), name = "Fitting dataset:") +
  labs(x="Size of the training set", y="log(WSE)") +
  #lims(y=c(0,0.5)) +
  theme_classic() +
  theme.settings + 
  theme(plot.margin = unit(c(0.7,0.7,0.7,0.7), "cm"))


# n test
plot.n_test <- facet.plot("n.test", data.path, "numeric")

wse.table.dt.o1.c <- plot.n_test$wse.dt[, .(wse.median = quantile(wse, 0.5, na.rm = TRUE), wse.iqr = IQR(wse, na.rm = TRUE)), by = .(n.test, dataset)][, n.test := as.numeric(levels(n.test))[n.test]][dataset == "all", dataset := "combined"]
setnames(wse.table.dt.o1.c, names(wse.table.dt.o1.c),c("n.test","dataset","median","IQR"))
setorder(wse.table.dt.o1.c,n.test,dataset)
print(xtable(wse.table.dt.o1.c, caption = c("numerical results for the Weibull-Cox scenario (ii) with varying n.test"), align = c("c","l","l","c","c"), digits = c(0,0,0,6,6)), include.rownames = FALSE)

p.n.test <- ggplot(plot.n_test$wse.dt) +
  geom_violin(aes(x=n.test,y=log(wse), fill = dataset), show.legend = T, draw_quantiles = c(0.5)) +
  scale_fill_manual(values = c("#00788C","#B07C4F","#76A165"), labels = c("combined", "test", "training"), name = "Fitting dataset:") +
  labs(x="Size of the test set", y="log(WSE)") +
  #lims(y=c(0,0.15)) +
  theme_classic() +
  theme.settings + 
  theme(plot.margin = unit(c(0.7,0.7,0.7,0.7), "cm"))


## censoring rate
plot.rate <- facet.plot("rate", data.path, "numeric")

wse.table.dt.o1.d1 <- plot.rate$wse.dt[, .(wse.median = quantile(wse, 0.5, na.rm = TRUE), wse.iqr = IQR(wse, na.rm = TRUE)), by = .(rate, dataset)][, rate := as.numeric(levels(rate))[rate]][order(rate)][dataset == "all", dataset := "combined"]
wse.table.dt.o1.d1[rate == "0.219", rate := "0.2"][rate == "0.864", rate := "0.5"][rate == "3.013", rate := "0.8"][, rate := as.factor(rate)]
setnames(wse.table.dt.o1.d1, names(wse.table.dt.o1.d1),c("censoring rate","dataset","median","IQR"))
setorder(wse.table.dt.o1.d1,`censoring rate`,dataset)
print(xtable(wse.table.dt.o1.d1, caption = c("numerical results for the baseline scenario (i) with varying censoring rate"), align = c("c","l","l","c","c"), digits = c(0,0,0,6,6)), include.rownames = FALSE)

p.censoring.rate <- ggplot(plot.rate$wse.dt[rate == "0.219", cens.rate := "0.2"][rate == "0.864", cens.rate := "0.5"][rate == "3.013", cens.rate := "0.8"][, cens.rate := as.factor(cens.rate)]) +
  geom_violin(aes(x=cens.rate,y=log(wse), fill = dataset), show.legend = T, draw_quantiles = c(0.5)) +
  scale_fill_manual(values = c("#00788C","#B07C4F","#76A165"), labels = c("combined", "test", "training"), name = "Fitting dataset:") +
  labs(x="Censoring rate", y="log(WSE)") +
  #lims(y=c(0,0.1)) +
  theme_classic() +
  theme.settings + 
  theme(plot.margin = unit(c(0.7,0.7,0.7,0.7), "cm"))

ggarrange(p.n.per.ds, p.n.training, p.n.test, p.censoring.rate, common.legend = TRUE, legend = "bottom", labels = c("(a)","(b)","(c)","(d)"), hjust = 0, font.label = list(size = 20, face = "bold"))


# censoring rate (Weibull)
plot.offset <- facet.plot("offset", data.path, "numeric")

wse.table.dt.o1.d2 <- plot.offset$wse.dt[, .(wse.median = quantile(wse, 0.5, na.rm = TRUE), wse.iqr = IQR(wse, na.rm = TRUE)), by = .(offset, dataset)][, offset := as.numeric(levels(offset))[offset]][order(offset)][dataset == "all", dataset := "combined"]
wse.table.dt.o1.d2[offset == "0.8", offset := "0.2"][offset == "-0.002", offset := "0.5"][offset == "-0.803", offset := "0.8"][, offset := as.factor(offset)]
setnames(wse.table.dt.o1.d2, names(wse.table.dt.o1.d2),c("censoring rate","dataset","median","IQR"))
setorder(wse.table.dt.o1.d2,`censoring rate`,dataset)
print(xtable(wse.table.dt.o1.d2, caption = c("numerical results for the Weibull-Cox scenario (ii) with varying censoring rate"), align = c("c","l","l","c","c"), digits = c(0,0,0,6,6)), include.rownames = FALSE)

p.offset <- ggplot(plot.offset$wse.dt[offset == "0.8", cens.rate := "0.2"][offset == "-0.002", cens.rate := "0.5"][offset == "-0.803", cens.rate := "0.8"][, cens.rate := as.factor(cens.rate)]) +
  geom_violin(aes(x=cens.rate,y=log(wse), fill = dataset), show.legend = T, draw_quantiles = c(0.5)) +
  scale_fill_manual(values = c("#00788C","#B07C4F","#76A165"), labels = c("combined", "test", "training"), name = "Fitting dataset:") +
  labs(x="Censoring rate", y="log(WSE)") +
  #lims(y=c(0,0.1)) +
  theme_classic() +
  theme.settings + 
  theme(plot.margin = unit(c(0.7,0.7,0.7,0.7), "cm"))

ggarrange(p.n.per.ds, p.n.training, p.n.test, p.offset, common.legend = TRUE, legend = "bottom", labels = c("(a)","(b)","(c)","(d)"), hjust = 0, font.label = list(size = 20, face = "bold"))



#plot.cens_estimator <- facet.plot("cens_estimator", data.path, "character")

plot.n.per.ds.cox <- facet.plot("n.per.ds", data.path, "numeric",  "_cens_estimator_cox")
plot.n.per.ds.km <- facet.plot("n.per.ds", data.path, "numeric",  "_cens_estimator_km")

## model misspecification
miss.wse.dt.cox <- plot.n.per.ds.cox$wse.dt
miss.wse.dt.km <- plot.n.per.ds.km$wse.dt

miss.wse.dt.cox[, estimator := "cox"]
miss.wse.dt.km[, estimator := "km"]

miss.wse.dt <- rbind(miss.wse.dt.cox, miss.wse.dt.km)
miss.wse.dt[, n := as.factor(2*as.numeric(as.character(n.per.ds)))]

wse.table.dt.o2 <- miss.wse.dt[, .(wse.median = quantile(wse, 0.5, na.rm = TRUE), wse.iqr = IQR(wse, na.rm = TRUE)), by = .(n, dataset, estimator)][, n := as.numeric(levels(n))[n]][order(n,dataset)][dataset == "all", dataset := "combined"]
setnames(wse.table.dt.o2, names(wse.table.dt.o2),c("n","dataset","estimator","median","IQR"))
print(xtable(wse.table.dt.o2, caption = c("numerical results for the misspecification scenario (iii)"), align = c("c","l","l","l","c","c"), digits = c(0,0,0,0,6,6)), include.rownames = FALSE)

ggplot(miss.wse.dt) +
  geom_violin(aes(x=n, y=log(wse), fill = dataset, color = estimator, linetype = estimator), show.legend = T, draw_quantiles = c(0.5)) +
  scale_fill_manual(values = c("#00788C","#B07C4F","#76A165"), labels = c("combined", "test", "training"), name = "Fitting dataset:") +
  scale_linetype_manual(values = c("solid", "dotted"), labels = c("Cox", "Kaplan-Meier"), name = "Estimator:") +
  scale_color_manual(values = c("#000000", "#ff0003"), labels = c("Cox", "Kaplan-Meier"), name = "Estimator:") +
  labs(x="Total size of the dataset", y="log(WSE)", color = "Estimator:", linetype = "Estimator:") +
  #lims(y=c(0,0.6)) +
  theme_classic() +
  theme.settings +
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.box = "vertical")


##o3a and o3b
#single plot
plot.estimator <- facet.plot("estimator", data.path, "character")

p.estimator <- ggplot(plot.estimator$wse.dt[estimator == "cox", estimator.type := "Cox"][estimator == "glmnet", estimator.type := "Lasso"][estimator == "rangersf", estimator.type := "Random forest"][estimator == "xgboost", estimator.type := "XGBoost"][, estimator.type := as.factor(estimator.type)]) +
  geom_violin(aes(x=estimator.type,y=wse, fill = dataset), show.legend = T, draw_quantiles = T) +
  scale_fill_manual(values = c("#00788C","#B07C4F","#76A165"), labels = c("combined", "test", "training"), name = "Fitting dataset:") +
  labs(x="estimator", y="WSE") +
  lims(y=c(0,0.1)) +
  theme_classic() +
  theme.settings +
  theme(legend.position = "bottom")

#individual plots
plot.estimator <- facet.plot("estimator", data.path, "character")

wse.table.dt.o3 <- plot.estimator$wse.dt[, .(wse.median = quantile(wse, 0.5, na.rm = TRUE), wse.iqr = IQR(wse, na.rm = TRUE)), by = .(estimator, dataset)][dataset == "all", dataset := "combined"]
setnames(wse.table.dt.o3, names(wse.table.dt.o3),c("estimator","dataset","median","IQR"))
print(xtable(wse.table.dt.o3, caption = c("numerical results for the high-noise ML scenario (v)"), align = c("c","l","l","c","c"), digits = c(0,0,0,6,6)), include.rownames = FALSE)


theme.x.blank <- theme(axis.title.x = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.text.x = element_blank())

p.estimator.cox <- ggplot(plot.estimator$wse.dt[estimator == "cox", estimator.type := "Cox"][estimator.type == "Cox"]) +
  geom_violin(aes(x=dataset,y=wse, fill = dataset), show.legend = T, draw_quantiles = c(0.5)) +
  scale_fill_manual(values = c("#00788C","#B07C4F","#76A165"), labels = c("combined", "test", "training"), name = "Fitting dataset:") +
  labs(x="Fitting dataset", y="WSE", title="Cox") +
  #lims(y=c(0,0.03)) +
  #lims(y=c(0,0.03)) +
  theme_classic() +
  theme.settings +
  theme.x.blank +
  theme(plot.title = element_text(size=rel(3))) + 
  theme(plot.margin = unit(c(0.9,0.9,0.9,0.9), "cm"))

p.estimator.glmnet <- ggplot(plot.estimator$wse.dt[estimator == "glmnet", estimator.type := "Lasso"][estimator.type == "Lasso"]) +
  geom_violin(aes(x=dataset,y=wse, fill = dataset), show.legend = T, draw_quantiles = c(0.5)) +
  scale_fill_manual(values = c("#00788C","#B07C4F","#76A165"), labels = c("combined", "test", "training"), name = "Fitting dataset:") +
  labs(x="Fitting dataset", y="WSE", title="Lasso") +
  #lims(y=c(0,0.125)) +
  #lims(y=c(0,0.08)) +
  theme_classic() +
  theme.settings +
  theme.x.blank +
  theme(plot.title = element_text(size=rel(3))) + 
  theme(plot.margin = unit(c(0.9,0.9,0.9,0.9), "cm"))

p.estimator.rangersf <- ggplot(plot.estimator$wse.dt[estimator == "rangersf", estimator.type := "Random forest"][estimator.type == "Random forest"]) +
  geom_violin(aes(x=dataset,y=wse, fill = dataset), show.legend = T, draw_quantiles = c(0.5)) +
  scale_fill_manual(values = c("#00788C","#B07C4F","#76A165"), labels = c("combined", "test", "training"), name = "Fitting dataset:") +
  labs(x="Fitting dataset", y="WSE", title="Random forest") +
  #lims(y=c(0,0.05)) +
  #lims(y=c(0,0.05)) +
  theme_classic() +
  theme.settings +
  theme.x.blank +
  theme(plot.title = element_text(size=rel(3))) + 
  theme(plot.margin = unit(c(0.9,0.9,0.9,0.9), "cm"))

p.estimator.xgboost <- ggplot(plot.estimator$wse.dt[estimator == "xgboost", estimator.type := "XGBoost"][estimator.type == "XGBoost"]) +
  geom_violin(aes(x=dataset,y=wse, fill = dataset), show.legend = T, draw_quantiles = c(0.5)) +
  scale_fill_manual(values = c("#00788C","#B07C4F","#76A165"), labels = c("combined", "test", "training"), name = "Fitting dataset:") +
  labs(x=NULL, y="WSE", title="XGBoost") +
  #lims(y=c(0,0.4)) +
  #lims(y=c(0,0.06)) +
  theme_classic() +
  theme.settings +
  theme.x.blank +
  theme(plot.title = element_text(size=rel(3))) + 
  theme(plot.margin = unit(c(0.9,0.9,0.9,0.9), "cm"))

ggarrange(p.estimator.cox, p.estimator.glmnet, p.estimator.rangersf, p.estimator.xgboost, common.legend = TRUE, legend = "bottom", labels = c("(a)","(b)","(c)","(d)"), hjust = 0, font.label = list(size = 20, face = "bold"))


### censoring rate (exponential)
#rate.wse.dt <- plot.rate$wse.dt
#rate.wse.dt[rate == "0.218", censoring.rate := "0.2"]
#rate.wse.dt[rate == "0.863", censoring.rate := "0.5"]
#rate.wse.dt[rate == "3.001", censoring.rate := "0.8"]
#rate.wse.dt[,censoring.rate := as.factor(censoring.rate)]

#ggplot(rate.wse.dt) +
#  geom_violin(aes(x=factor(dataset,ordered=T,levels=c("training","test","all")),y=wse, fill = dataset), show.legend = F, draw_quantiles = T) +
#  labs(x="fit dataset", y = "wse") +
#  scale_fill_manual(values = c("#0072b2","#d55e00","#f0e442")) +
#  lims(y=c(0,0.10)) +
#  facet_wrap(censoring.rate ~ ., scales = "free_x", labeller = label_both) +
#  theme_classic()

### censoring rate (Weibull)
#offset.wse.dt <- plot.offset$wse.dt
#offset.wse.dt[offset == "-0.803", censoring.rate := "0.8"]
#offset.wse.dt[offset == "-0.002", censoring.rate := "0.5"]
#offset.wse.dt[offset == "0.8", censoring.rate := "0.2"]
#offset.wse.dt[,censoring.rate := as.factor(censoring.rate)]

#ggplot(offset.wse.dt) +
#  geom_violin(aes(x=factor(dataset,ordered=T,levels=c("training","test","all")),y=wse, fill = dataset), show.legend = F, draw_quantiles = T) +
#  labs(x="fit dataset", y = "wse") +
#  scale_fill_manual(values = c("#0072b2","#d55e00","#f0e442")) +
#  lims(y=c(0,0.15)) +
#  facet_wrap(censoring.rate ~ ., scales = "free_x", labeller = label_both) +
#  theme_classic()
