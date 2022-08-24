source('./source/libs.R')
source('./source/themes.R')
source('./source/palettes.R')


# Box_plot ----------------------------------------------------------------

ind <- readRDS("./data/metrics_ind.rds")
atln <- readRDS("./data/metrics_atln.rds")
pacf <- readRDS("./data/metrics_pacf.rds")


# pre-process before plot
ind <- ind[, .(sname, imrg_run, bias, rmse, mae, POD, FAR, CSI, ocn)]
atln <- atln[, .(sname, imrg_run, bias, rmse, mae, POD, FAR, CSI, ocn)]
pacf <- pacf[, .(sname, imrg_run, bias, rmse, mae, POD, FAR, CSI, ocn)]


pacf <- pacf %>% 
  mutate(sname2 = sname) %>% 
  tidyr::separate(col = sname, into = c("lat","lon"), sep = "_")

pacf$ocn <- with(pacf, ifelse(lon < 0, "east_pacf", "west_pacf"))
pacf[, `:=`(sname = sname2, lat = NULL, lon = NULL, sname2 = NULL)]


met_all <- rbind(ind, atln, pacf)

# ind_alt_pacf_stat <- rbind(ind_stat, atln_stat, pacf_stat2)
# ind_alt_pacf_stat[, i.tot_mon := NULL]

# plot
colset_4 <-  c("#D35C37", "#BF9A77", "#D6C6B9", "#97B8C2")
mycol_continent5 <- c( "#00B0F6", "#A3A500", "#00BF7D", "#e07b39", 
                       "#80391e") 

met_all_plot <- melt(met_all, c("sname", "imrg_run", "ocn"))

levels(met_all_plot$variable) <- c("BIAS", "RMSE", "MAE",  "POD",  "FAR", "CSI")
levels(met_all_plot$imrg_run) <- c("IMERG-E", "IMERG-L", "IMERG-F")
levels(met_all_plot$ocn) <- c("Indian", "Atlantic", "East Pacific", "West Pacific")

ggplot(met_all_plot, aes(ocn, value, fill = imrg_run)) + 
  geom_boxplot() + 
  facet_wrap(~variable, scales = "free_y", ncol = 3) + 
  scale_fill_manual(values = c("808000",  "#D35C37", "#6590bb")) + 
  #scale_fill_manual(values = mycol_continent5[c(4, 3, 2, 1)]) + 
  #scale_fill_manual(values = c("#b64925", "808000", "#6590bb")) + 
  labs(x = "", y = "") + 
  theme_small + # for presentation slides
  theme(legend.position = "bottom",
    legend.title = element_blank()) +
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'),
        axis.text.x = element_text(angle = 30, hjust = 0.8, vjust = 0.9))
#strip.text.x = element_text(size = 12))

ggsave("results/paper_fig/boxplot_metrices.png",
       width = 7.2, height = 5.5, units = "in", dpi = 600)

#################################################################

### mean 

ind_mean <- met_all[imrg_run == "imrg_f" & ocn == "ind"]
summary(ind_mean)

atln_mean <- met_all[imrg_run == "imrg_f" & ocn == "atln"]
summary(atln_mean)

east_pacf_mean <- met_all[imrg_run == "imrg_f" & ocn == "east_pacf"]
summary(east_pacf_mean)

west_pacf_mean <- met_all[imrg_run == "imrg_f" & ocn == "west_pacf"]
summary(west_pacf_mean)
