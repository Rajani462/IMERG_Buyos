source('./source/libs.R')
source('./source/themes.R')
source('./source/palettes.R')


# import the datsets ----------------------------------------------------------------

ind <- readRDS("./data/metrics_ind.rds")
atln <- readRDS("./data/metrics_atln.rds")
east_pacf <- readRDS("./data/metrics_east_pacf.rds")
west_pacf <- readRDS("./data/metrics_west_pacf.rds")


# pre-process datsets ----------------------------------------------------------------

ind <- ind[, .(sname, imrg_run, bias, rmse, mae, POD, cor, FAR, CSI, ocn)]
atln <- atln[, .(sname, imrg_run, bias, rmse, mae, cor, POD, FAR, CSI, ocn)]
east_pacf <- east_pacf[, .(sname, imrg_run, bias, rmse, mae, cor, POD, FAR, CSI, ocn)]
west_pacf <- west_pacf[, .(sname, imrg_run, bias, rmse, mae, cor, POD, FAR, CSI, ocn)]


met_all <- rbind(ind, atln, east_pacf, west_pacf)

# plot ----------------------------------------------------------------

colset_4 <-  c("#D35C37", "#BF9A77", "#D6C6B9", "#97B8C2")
mycol_continent5 <- c( "#00B0F6", "#A3A500", "#00BF7D", "#e07b39", 
                       "#80391e") 

met_all_plot <- melt(met_all, c("sname", "imrg_run", "ocn"))

levels(met_all_plot$variable) <- c("BIAS", "RMSE", "MAE", "COR", "POD",  "FAR", "CSI")
levels(met_all_plot$imrg_run) <- c("IMERG-E", "IMERG-L", "IMERG-F")
levels(met_all_plot$ocn) <- c("Indian", "Atlantic", "East Pacific", "West Pacific")

ggplot(met_all_plot[variable != "COR"], aes(ocn, value, fill = imrg_run)) + 
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


### with correlation

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


ggsave("results/boxplot_metrices.png",
       width = 7.2, height = 5.5, units = "in", dpi = 600)


### plot for poster presentation

ggplot(met_all_plot[variable == "BIAS" | variable == "COR"| variable == "CSI"], aes(ocn, value, fill = imrg_run)) + 
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

ggsave("results/poster_boxplot_metrices.png",
       width = 7.2, height = 5.5, units = "in", dpi = 600)


#################################################################

