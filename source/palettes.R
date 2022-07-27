# Various palettes and a function to test them


### Current Palettes and color sets

colset_bright <- c("#6a3d9a", "#375E97", "#008DCB", "#31A9B8", 
                   "#486B00", "#258039", "#A2C523", "#FFCE38", 
                   "#F0810F", "#FA6775", "#D61800", "#9B4F0F")

colset_bright_qual <- colset_bright[c(11, 2, 6, 8, 3, 7, 9, 1, 5, 12, 4, 10)]
palette_bright <- colorRampPalette(colset_bright)
palette_bright_qual <- colorRampPalette(colset_bright_qual)

colset_mid <- c( "#4D648D", "#337BAE", "#97B8C2",  "#739F3D", "#ACBD78",  
                 "#F4CC70", "#EBB582",  "#BF9A77",
                 "#E38B75", "#CE5A57",  "#D24136", "#785A46" )

colset_mid_qual <- colset_mid[c(11, 2, 4, 6,  1, 8, 10, 5, 7, 3, 9, 12)]
palette_mid <- colorRampPalette(colset_mid)
palette_mid_qual <- colorRampPalette(colset_mid_qual)

colset_light_qual <- c("#8dd3c7",   "#fdb462", "#bebada", "#fb8072", 
                       "#80b1d3",  "#b3de69", "#ffed6f","#bc80bd",      
                       "#d9d9d9",  "#fccde5","#ccebc5", "#a1d6e2")

palette_light_qual <- colorRampPalette(colset_light_qual)
palette_light_gr_bu <- colorRampPalette(c( "#ACBD78", "grey80","#97B8C2"))

### VUV Colors
# dark green R:10 G:111 B:136 #0A6F88
# light green R:21 G:169 B:198 #15A9C6
# pea green R:88 G:181 B:46 #58B52E
# red R:224 G:14 B:28 #E00E1C
# purple R:119 G:0 B:127 #77007F
# orange R:237 G:136 B:16 #ED8810
palette_vuv <- colorRampPalette(c( "#0A6F88", "#15A9C6", "#58B52E", "#E00E1C", "#77007F", "#ED8810"))

### Some palettes from https://www.canva.com/learn/100-color-combinations/

palettes_bright <- list(
  colset_cheer_brights = c("#C73721", "#31A9B8", "#F5BE41", "#258039"),
  colset_pool_party = c("#344D90", "#5CC5EF", "#FFB745", "#E7552C"),
  colset_beyond_bw = c("#31A2AC", "#AF1C1C", "#F0EFF0", "#2F2F28"),
  colset_sleek_modern = c("#2F2E33", "#D5D6D2", "#FFFFFF", "#3A5199"),
  colset_bold_culture = c("#4C3F54", "#D13525", "#F2C057", "#486824"),
  colset_modern_urban = c("#217CA3", "#E29930", "#32384D", "#211F30"), 
  colset_sun_sky = c("#F9BA32", "#426E86", "#F8F1E5", "#2F3131"),
  colset_subdued_prof = c("#90AFC5", "#336B87", "#2A3132", "#763626"),
  colset_urban_oasis = c("#506D2F", "#2A2922", "#F3EBDD", "#7D5642")
)

palettes_mid <- list(
  colset_golden_afternoon = c("#882426", "#CDBEA7", "#323030", "#C29545"),
  colset_orange_accent = c("#756867", "#D5D6D2", "#353C3F", "#FF8D3F"),
  colset_misty_greens = c("#04202C", "#304040", "#5B7065", "#8E9B97","#C9D1C8"),
  colset_spicy_neutrals = c("#AF4425", "#662E1C", "#EBDCB2", "#C9A66B"),
  colset_warm_naturals = c("#2E2300", "#6E6702", "#C05805", "#DB9501"),
  colset_autumn_vermont = c("#8D230F", "#1E434C", "#9B4F0F", "#C99E10"),
  colset_greens_blues = c("#324851", "#86AC41", "#34675C", "#7DA3A1"),
  colset_surf_turf = c("#F4CC70", "#DE7A22", "#20948B", "#6AB187")
)

palettes_light <- list(
  colset_sunkissed_village = c("#D24136", "#EB8A3E", "#EBB582", "#785A46"),
  colset_warm_cool = c("#444C5C", "#CE5A57", "#78A5A3", "#E1B164"),
  colset_muted_antique = c("#A4CABC", "#EAB364", "#B2473E", "#ACBD78"),
  colset_retro_relaxing = c("#D35C37", "#BF9A77", "#D6C6B9", "#97B8C2")
)

### A function to test palettes

test_palette <- function(palette_name, n = 4){
  
  vars <- 1:n
  test <- data.table(var = as.factor(c(rep(vars, 250))), x = rnorm(250*length(vars)), y = rnorm(250*length(vars)))
  
  par(mfrow = c(2,1))
  plot(1:n, rep(1, n), col = palette_name(n), cex = 4, pch = 16)
  plot(1:n, rep(1, n), col = ColToGrey(palette_name(n)), cex = 4, pch = 16, main = "grayscale")
  par(mfrow = c(1,1))
  
  
  a <- ggplot(test, aes(x, y, col = var)) +
    geom_point(size = 2) + 
    scale_color_manual(values = palette_name(n)) + 
    theme_bw()
  
  b <- ggplot(test, aes(y = y, fill = var)) +
    geom_boxplot() + 
    scale_fill_manual(values =  palette_name(n)) + 
    theme_bw()
  
  c <- ggplot(test, aes(x = x, y = y, col = var)) +
    geom_smooth(se = F) + 
    scale_color_manual(values =  palette_name(n)) + 
    theme_bw()
  
  d <- ggplot(test, aes(x = round(x*2), y = round(y*2), fill = var)) +
    geom_tile() + 
    scale_fill_manual(values =  palette_name(n)) + 
    theme_bw()
  
  e <- ggplot(test, aes(x, y, col = var)) +
    geom_point(size = 2) + 
    scale_color_manual(values = ColToGrey(palette_name(n))) + 
    labs(title = "Colors in grayscale") +
    theme_bw()
  
  f <- ggplot(test, aes(y = y, fill = var)) +
    geom_boxplot() + 
    scale_fill_manual(values = ColToGrey(palette_name(n))) + 
    labs(title = "Colors in grayscale") +
    theme_bw()
  
  g <- suppressWarnings(ggplot(test, aes(x = x, y = y, col = var)) +
                          geom_smooth(se = F) + 
                          scale_color_manual(values =  ColToGrey(palette_name(n))) + 
                          labs(title = "Colors in grayscale") +
                          theme_bw())
  
  h <- ggplot(test, aes(x = round(x*2), y = round(y*2), fill = var)) +
    geom_tile() + 
    scale_fill_manual(values =  ColToGrey(palette_name(n))) + 
    labs(title = "Colors in grayscale") +
    theme_bw() 
  
  ggarrange(a, b, c, d, e, f, g, h, ncol = 2, nrow = 2)
}

### Older Palettes

rgb_palette_RdBu <- colorRampPalette(rev(c('#d73027','#f46d43','#fdae61','#fee090','#fef0d9','#e0f3f8','#abd9e9','#74add1','#4575b4')), space = "rgb")
gradient_RdBu <- rgb_palette_RdBu(100)
my_drought_col <- colorRampPalette(c('#8c510a','#d8b365','#f6e8c3','#f5f5f5','skyblue1','skyblue3','skyblue4'), interpolate = "spline", space = "rgb")
my_purples_uneven <- colorRampPalette(c(rep('#edf8fb',6),'#bfd3e6','#9ebcda','#8c96c6','#8c6bb1'), space = "rgb")
my_reds <- colorRampPalette(c('#fef0d9','#fdd49e','#fdbb84','#fc8d59','#ef6548','#d7301f','#990000'), space = "rgb")
rgb_palette_Qualitative_1 <- colorRampPalette(c("#4575b4", "#78c679", "#f46d43", "#74add1", "#807dba", "#fee090", "#d9f0a3", "#d73027",     "#abd9e9", "#fdae61", "#fa9fb5", "#ffed6f"), space = "rgb")
rgb_palette_Qualitative_2 <- colorRampPalette(c("#8dd3c7", "#ffffb3", "#bebada", "#fb8072", "#80b1d3", "#fdb462", "#b3de69", "#fccde5",     "#d9d9d9", "#bc80bd", "#ccebc5", "#ffed6f"))
rgb_palette_Qualitative_3 <- colorRampPalette(c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00",     "#cab2d6", "#6a3d9a", "#ffed6f", "#b15928"))


###Rajani_Palettes for Continents (GPM_review)
mycol_continent5 <- c( "#00B0F6", "#A3A500", "#00BF7D", "#e07b39", 
                      "#80391e")  

mycol_continent6<- c( "#00B0F6", "#A3A500", "#00BF7D", "#edb879", "#e07b39", 
                           "#80391e")

mycol_imrg <- c("#edb879", "#e07b39", "#80391e", "#A3A500")
