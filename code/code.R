#R code to reproduce statistics and visuals

#Libraries----------
library(tidyverse)
library(tmap)
library(sf)
library(cowplot)
library(ggpubr)

#Tract-level data-------------
dta <- read_csv("tract_data/ida_data_tract.csv") 

#Sums-------------
vars <- c("dmg_wind_total_tract_1971",
          "dmg_wind_total_tract_2021",
          "dmg_wind_total_tract_2071",
          
          "affected_wind_over5k_tract_1971",
          "affected_wind_over5k_tract_2021",
          "affected_wind_over5k_tract_2071",
          
          "dmg_wind_total_residential_tract_1971",
          "dmg_wind_total_residential_tract_2021",
          "dmg_wind_total_residential_tract_2071",
          
          "dmg_wind_total_commercial_tract_1971",
          "dmg_wind_total_commercial_tract_2021",
          "dmg_wind_total_commercial_tract_2071",
          
          "dmg_wind_total_industrial_tract_1971",
          "dmg_wind_total_industrial_tract_2021",
          "dmg_wind_total_industrial_tract_2071",
          
          "dmg_wind_total_public_tract_1971",
          "dmg_wind_total_public_tract_2021",
          "dmg_wind_total_public_tract_2071",
          
          #surge
          "dmg_surge_total_tract_1971",
          "dmg_surge_total_tract_2021",
          "dmg_surge_total_tract_2071",
          
          "affected_surge_over5k_tract_1971",
          "affected_surge_over5k_tract_2021",
          "affected_surge_over5k_tract_2071",
          
          "dmg_surge_total_residential_tract_1971",
          "dmg_surge_total_residential_tract_2021",
          "dmg_surge_total_residential_tract_2071",
          
          "dmg_surge_total_commercial_tract_1971",
          "dmg_surge_total_commercial_tract_2021",
          "dmg_surge_total_commercial_tract_2071",
          
          "dmg_surge_total_industrial_tract_1971",
          "dmg_surge_total_industrial_tract_2021",
          "dmg_surge_total_industrial_tract_2071",
          
          "dmg_surge_total_public_tract_1971",
          "dmg_surge_total_public_tract_2021",
          "dmg_surge_total_public_tract_2071")

sums <- sapply(vars, function(x) sum(dta[[x]], na.rm = T))
list2env(as.list(sums), envir = .GlobalEnv)

#percent change function
p_change <- function(old, new) {
  x = ((new - old) / old) * 100 
  x = round(x, 0) %>%
    str_c("%")
  
  return(x)
}
##Total damage under 1971, 2021, 2071 scenarios-----

tibble(
  hazard = c("wind", "surge"),
  dmg1971 = c(dmg_wind_total_tract_1971, dmg_surge_total_tract_1971),
  dmg2021 = c(dmg_wind_total_tract_2021, dmg_surge_total_tract_2021),
  dmg2071 = c(dmg_wind_total_tract_2071, dmg_surge_total_tract_2071),
  change2021_1971 = dmg2021 - dmg1971,
  '2021 - 1971 (%)' = p_change(dmg1971, dmg2021),
  change2021_2071 = dmg2071 - dmg2021,
  '2071-2021 (%)' = p_change(dmg2021, dmg2071),
  change2071_1971 = dmg2071 - dmg1971,
  '2071-1971 (%)' = p_change(dmg1971, dmg2071)
)

##Number of buildings affected under 1971, 2021, 2071 scenarios----
tibble(
  hazard = c("wind", "surge"),
  affected1971 = c(affected_wind_over5k_tract_1971, 
                   affected_surge_over5k_tract_1971),
  affected2021 = c(affected_wind_over5k_tract_2021, 
                   affected_surge_over5k_tract_2021),
  affected2071 = c(affected_wind_over5k_tract_2071, 
                   affected_surge_over5k_tract_2071),
  change2021_1971 = affected2021 - affected1971,
  '2021 - 1971 (%)' = p_change(affected1971, affected2021),
  change2071_2021 = affected2071 - affected2021,
  '2071-2021 (%)' = p_change(affected2021, affected2071),
  change2071_1971 = affected2071 - affected1971,
  '2071-1971 (%)' = p_change(affected1971, affected2071)
)

##Total damage per structure under 1971, 2021, and 2071 scenarios-------------
tibble(
  year = c(1971, 2021, 2071),
  res_prop_damage_wind = c(
    dmg_wind_total_residential_tract_1971 / dmg_wind_total_tract_1971,
    dmg_wind_total_residential_tract_2021 / dmg_wind_total_tract_2021,
    dmg_wind_total_residential_tract_2071 / dmg_wind_total_tract_2071
  ),
  
  res_prop_damage_surge = c(
    dmg_surge_total_residential_tract_1971 / dmg_surge_total_tract_1971,
    dmg_surge_total_residential_tract_2021 / dmg_surge_total_tract_2021,
    dmg_surge_total_residential_tract_2071 / dmg_surge_total_tract_2071
  )
)


#Figures-------------
##Bar charts: total damage, # buildings affected, and damage per building ------
# Plot settings
width = .935
col_color = "grey10"
bar_colors <- c("#D5EAFF", "#5BADFF", "#0052A4")

#label positions
x1 = 0.7
x2 = 1.32
x3 = 1.7
x4 = 2.32

#function to label y-axis
fun_labels <- function(x) {
  ifelse(x >= 1e9, paste0(x / 1e9, " B"),
         ifelse(x >= 1e6, paste0(x / 1e6, " M"), 
                ifelse(x > 1e3, paste0(x/ 1e3, " K"), x)))}

#function to add percent change labels
annot_fun <- function(p_change, x_position, y_value, color) {
  annotate(
    "text",
    x = x_position, y = y_value,
    label = p_change,
    family = "serif",
    size = 3, 
    color = color
  )
}

plot_settings <- list(
  theme_classic(),
  theme(text = element_text(family = "serif")), 
  xlab(""),
  scale_fill_manual(
    name = "",
    values = bar_colors
  )
)
####Total damage----------
#labels

damage_surge2021to1971 <- p_change(dmg_surge_total_tract_2021, 
                                   dmg_surge_total_tract_1971)

damage_surge2021to2071 <- p_change(dmg_surge_total_tract_2021,
                                   dmg_surge_total_tract_2071)

damage_wind2021to1971 <- p_change(dmg_wind_total_tract_2021,
                                  dmg_wind_total_tract_1971)

damage_wind2021to2071 <- p_change(dmg_wind_total_tract_2021,
                                  dmg_wind_total_tract_2071)


damage_visual <-
  tibble(
    year = factor(c(1971, 2021, 2071, 1971, 2021, 2071)),
    hazard = c("Wind", "Wind", "Wind", "Surge", "Surge", "Surge") %>%
      factor(levels = c("Wind", "Surge")),
    damage = c(
      dmg_wind_total_tract_1971,
      dmg_wind_total_tract_2021,
      dmg_wind_total_tract_2071,
      
      dmg_surge_total_tract_1971,
      dmg_surge_total_tract_2021,
      dmg_surge_total_tract_2071
    )
  ) %>%
  ggplot(aes(hazard, damage, fill = year)) +
  geom_col(position = position_dodge(), width = width, col = col_color) +
  
  coord_cartesian(ylim = c(0, 8e9)) +
  ylab("Total Damage in $") +
  scale_y_continuous(expand = c(0,0), labels = fun_labels) +
  
  annot_fun(damage_wind2021to1971, x1, 1.7e9, "black") +
  annot_fun(str_c("+", damage_wind2021to2071), x2, 5.8e9, "white") + 
  annot_fun(damage_surge2021to1971, x3, 2e9, "black") +
  annot_fun(str_c("+", damage_surge2021to2071), x4, 2.8e9, "white") +
  plot_settings


damage_visual

####Number of buildings affected-------------
n_wind2021to1971 <- p_change(affected_wind_over5k_tract_2021, 
                             affected_wind_over5k_tract_1971)

n_wind2021to2071 <-p_change(affected_wind_over5k_tract_2021, 
                            affected_wind_over5k_tract_2071)

n_surge2021to1971 <- p_change(affected_surge_over5k_tract_2021, 
                              affected_surge_over5k_tract_1971)

n_surge2021to2071 <- p_change(affected_surge_over5k_tract_2021,
                              affected_surge_over5k_tract_2071)

buildings_affected_visual <-
  tibble(
    year = factor(c(1971, 2021, 2071, 1971, 2021, 2071)),
    hazard = c("Wind", 
               "Wind", 
               "Wind", 
               "Surge", 
               "Surge", 
               "Surge") %>%
      factor(levels = c("Wind", "Surge")),
    damage = c(
      affected_wind_over5k_tract_1971,
      affected_wind_over5k_tract_2021,
      affected_wind_over5k_tract_2071,
      
      affected_surge_over5k_tract_1971,
      affected_surge_over5k_tract_2021,
      affected_surge_over5k_tract_2071
    )
  ) %>%
  ggplot(aes(hazard, damage, fill = year)) +
  geom_col(position = position_dodge(), width = width, col = col_color) +
  coord_cartesian(ylim = c(0, 130000)) +
  ylab("Number of Buildings Affected") +
  scale_y_continuous(expand = c(0,0), labels = fun_labels) +
  
  annot_fun(n_wind2021to1971, x1, 55e3, "black") +
  annot_fun(str_c("+", n_wind2021to2071), x2, 106e3, "white") +
  annot_fun(n_surge2021to1971, x3, 12e3, "black") +
  annot_fun(str_c("+", n_surge2021to2071), x4, 20e3, "white") +
  plot_settings

buildings_affected_visual

####Damage per building -----------
wind_damage_per_b2021to1971 <-
  p_change((dmg_wind_total_tract_2021 / affected_wind_over5k_tract_2021),
           (dmg_wind_total_tract_1971 / affected_wind_over5k_tract_1971))

wind_damage_per_b2021to2071 <-
  p_change((dmg_wind_total_tract_2021 / affected_wind_over5k_tract_2021),
           (dmg_wind_total_tract_2071 / affected_wind_over5k_tract_2071))

surge_damage_per_b2021to1971 <-
  p_change((dmg_surge_total_tract_2021 / affected_surge_over5k_tract_2021),
           (dmg_surge_total_tract_1971 / affected_surge_over5k_tract_1971))

surge_damage_per_b2021to2071 <-
  p_change((dmg_surge_total_tract_2021 / affected_surge_over5k_tract_2021),
           (dmg_surge_total_tract_2071 / affected_surge_over5k_tract_2071))

damage_per_building <-
  tibble(
    year = c(1971, 2021, 2071, 1971, 2021, 2071) %>%
      factor(),
    hazard = c("Wind", "Wind", "Wind", 
               "Surge","Surge","Surge") %>%
      factor(levels = c("Wind", "Surge")),
    dmg_per_building = c(
      dmg_wind_total_tract_1971 / affected_wind_over5k_tract_1971,
      dmg_wind_total_tract_2021 / affected_wind_over5k_tract_2021,
      dmg_wind_total_tract_2071 / affected_wind_over5k_tract_2071,
      
      dmg_surge_total_tract_1971 / affected_surge_over5k_tract_1971,
      dmg_surge_total_tract_2021 / affected_surge_over5k_tract_2021,
      dmg_surge_total_tract_2071 / affected_surge_over5k_tract_2071
    )
  ) %>%
  ggplot(aes(hazard, dmg_per_building, fill = year)) +
  geom_col(position = position_dodge(), width = width, col = col_color) +
  coord_cartesian(ylim = c(0, 160e3)) +
  ylab("Total Damage per Building") +
  scale_y_continuous(expand = c(0,0), labels = fun_labels) +
  
  annot_fun(wind_damage_per_b2021to1971, x1, 25e3, "black") +
  annot_fun(str_c("+", wind_damage_per_b2021to2071), x2, 45e3, "white") +
  annot_fun(surge_damage_per_b2021to1971, x3, 116e3, "black") +
  annot_fun(surge_damage_per_b2021to2071, x4, 115e3, "white") +
  plot_settings

damage_per_building

#### Arrange-------------
combined_damage_figure <- #legend on the left
  ggdraw() +
  draw_plot(damage_visual + 
              theme(legend.position = "none"),
            x = 0, y = 0.45, width = .5, height = .5) +
  draw_plot(buildings_affected_visual + 
              theme(legend.position = "none"), 
            x = .5, y = 0.45, width = .5, height = .5) +
  draw_plot(damage_per_building +
              theme(legend.key.spacing.y = unit(1, units = "mm")), 
            x = .2, y = 0, width = .65, height = .45)

combined_damage_figure

ggsave(
  combined_damage_figure,
  file = "figures/total_damage.png",
  height = 6,
  width = 6.6
)

##Bar charts: damage by structure type------------
###Labels----------
#wind labels
wind_res2021to1971 <- p_change(dmg_wind_total_residential_tract_2021, 
                               dmg_wind_total_residential_tract_1971)

wind_res2021to2071 <- p_change(dmg_wind_total_residential_tract_2021, 
                               dmg_wind_total_residential_tract_2071) 

wind_com2021to1971 <- p_change(dmg_wind_total_commercial_tract_2021, 
                               dmg_wind_total_commercial_tract_1971)

wind_com2021to2071 <- p_change(dmg_wind_total_commercial_tract_2021, 
                               dmg_wind_total_commercial_tract_2071) 

wind_ind2021to1971 <- p_change(dmg_wind_total_industrial_tract_2021, 
                               dmg_wind_total_industrial_tract_1971)

wind_ind2021to2071 <- p_change(dmg_wind_total_industrial_tract_2021, 
                               dmg_wind_total_industrial_tract_2071) 

wind_pub2021to1971 <- p_change(dmg_wind_total_public_tract_2021, 
                               dmg_wind_total_public_tract_1971)

wind_pub2021to2071 <- p_change(dmg_wind_total_public_tract_2021, 
                               dmg_wind_total_public_tract_2071) 

#surge labels
surge_res2021to1971 <- p_change(dmg_surge_total_residential_tract_2021, 
                                dmg_surge_total_residential_tract_1971)

surge_res2021to2071 <- p_change(dmg_surge_total_residential_tract_2021, 
                                dmg_surge_total_residential_tract_2071) 

surge_com2021to1971 <- p_change(dmg_surge_total_commercial_tract_2021, 
                                dmg_surge_total_commercial_tract_1971)

surge_com2021to2071 <- p_change(dmg_surge_total_commercial_tract_2021, 
                                dmg_surge_total_commercial_tract_2071) 

surge_ind2021to1971 <- p_change(dmg_surge_total_industrial_tract_2021, 
                                dmg_surge_total_industrial_tract_1971)

surge_ind2021to2071 <- p_change(dmg_surge_total_industrial_tract_2021, 
                                dmg_surge_total_industrial_tract_2071) 

surge_pub2021to1971 <- p_change(dmg_surge_total_public_tract_2021, 
                                dmg_surge_total_public_tract_1971)

surge_pub2021to2071 <- p_change(dmg_surge_total_public_tract_2021, 
                                dmg_surge_total_public_tract_2071) 

annot_fun2 <- function(p_change, x_position, y_value, color) {
  annotate(
    "text",
    x = x_position, y = y_value,
    label = p_change,
    family = "serif",
    size = 2.75, 
    color = color
  )
}

###Wind---------------------
####Residential + Commercial---------------
wind_res_com <-
  tibble(
    structure_type = c(rep("Residential", 3), rep("Commercial", 3)) %>%
      factor(levels = c("Residential", "Commercial")),
    year = rep(c("1971", "2021", "2071"), 2) %>%
      factor(levels = c("1971", "2021", "2071")),
    total = c(dmg_wind_total_residential_tract_1971,
              dmg_wind_total_residential_tract_2021,
              dmg_wind_total_residential_tract_2071,
              
              dmg_wind_total_commercial_tract_1971,
              dmg_wind_total_commercial_tract_2021,
              dmg_wind_total_commercial_tract_2071)
  ) %>%
  ggplot(aes(structure_type, total, fill = year)) +
  geom_col(position = position_dodge(), col = "grey10", width = width) +
  labs(
    title = "Wind (N = 113,474)",
    y = "Damage in $"
  ) +
  coord_cartesian(ylim = c(0, 5e9)) +
  scale_y_continuous(
    breaks = seq(0, 5e9, by = 1e9),
    labels = fun_labels,
    expand = c(0,0)
  ) +
  annot_fun2(wind_res2021to1971, x1, 1e9, "black") +
  annot_fun2(str_c("+", wind_res2021to2071), x2, 4e9, "white") +
  annot_fun2(wind_com2021to1971, x3, 24.25e7, "black") +
  annot_fun2(str_c("+", wind_com2021to2071), x4, .75e9, "white") +
  plot_settings +
  theme(plot.title = element_text(size = 12))

wind_res_com

####Public + Industrial------------------
wind_ind_pub <-
  tibble(
    structure_type = c(rep("Industrial", 3), rep("Public", 3)) %>%
      factor(levels = c("Industrial", "Public")),
    year = rep(c("1971", "2021", "2071"), 2) %>%
      factor(levels = c("1971", "2021", "2071")),
    total = c(dmg_wind_total_industrial_tract_1971,
              dmg_wind_total_industrial_tract_2021,
              dmg_wind_total_industrial_tract_2071,
              
              dmg_wind_total_public_tract_1971,
              dmg_wind_total_public_tract_2021,
              dmg_wind_total_public_tract_2071)
  ) %>%
  ggplot(aes(structure_type, total, fill = year)) +
  geom_col(position = position_dodge(), col = "grey10", width = width) +
  labs(
    title = " ",
    y = "Damage in $"
  ) +
  coord_cartesian(ylim = c(0, 400e6)) +
  scale_y_continuous(
    breaks = seq(0, 400e6, by = 100e6),
    labels = fun_labels,
    expand = c(0,0)
  ) +
  annot_fun2(wind_ind2021to1971, x1, 5e7, "black") +
  annot_fun2(str_c("+", wind_ind2021to2071), x2, 2.5e8, "white") +
  annot_fun2(wind_pub2021to1971, x3, 7e7, "black") +
  annot_fun2(str_c("+", wind_pub2021to2071), x4, 2.8e8, "white") +
  plot_settings +
  theme(plot.title = element_text(size = 12))

wind_ind_pub

###Surge---------------
####Residential + Commercial---------------
surge_res_com <-
  tibble(
    structure_type = c(rep("Residential", 3), rep("Commercial", 3)) %>%
      factor(levels = c("Residential", "Commercial")),
    year = rep(c("1971", "2021", "2071"), 2) %>%
      factor(levels = c("1971", "2021", "2071")),
    total = c(dmg_surge_total_residential_tract_1971,
              dmg_surge_total_residential_tract_2021,
              dmg_surge_total_residential_tract_2071,
              
              dmg_surge_total_commercial_tract_1971,
              dmg_surge_total_commercial_tract_2021,
              dmg_surge_total_commercial_tract_2071)
  ) %>%
  ggplot(aes(structure_type, total, fill = year)) +
  geom_col(position = position_dodge(), col = "grey10", width = width) +
  labs(
    title = "Surge (N = 26,394)",
    y = "Damage in $"
  ) +
  coord_cartesian(ylim = c(0, 3e9)) +
  scale_y_continuous(
    breaks = seq(0, 3e9, by = 500e6),
    labels = fun_labels,
    expand = c(0,0)
  ) +
  annot_fun2(surge_res2021to1971, x1, 17e8, "black") +
  annot_fun2(str_c("+", surge_res2021to2071), x2, 2.4e9, "white") +
  annot_fun2(surge_com2021to1971, x3, 2.5e8, "black") +
  annot_fun2(str_c("+", surge_com2021to2071), x4, 3e8, "white") +
  plot_settings +
  theme(plot.title = element_text(size = 12))

surge_res_com
####Industrial + Public---------------
surge_ind_pub <-
  tibble(
    structure_type = c(rep("Industrial", 3), rep("Public", 3)) %>%
      factor(levels = c("Industrial", "Public")),
    year = rep(c("1971", "2021", "2071"), 2) %>%
      factor(levels = c("1971", "2021", "2071")),
    total = c(dmg_surge_total_industrial_tract_1971,
              dmg_surge_total_industrial_tract_2021,
              dmg_surge_total_industrial_tract_2071,
              
              dmg_surge_total_public_tract_1971,
              dmg_surge_total_public_tract_2021,
              dmg_surge_total_public_tract_2071)
  ) %>%
  ggplot(aes(structure_type, total, fill = year)) +
  geom_col(position = position_dodge(), col = "grey10", width = width) +
  labs(
    title = " ",
    y = "Damage in $"
  ) +
  coord_cartesian(ylim = c(0, 60e6)) +
  scale_y_continuous(
    breaks = seq(0, 60e6, by = 10e6),
    labels = fun_labels,
    expand = c(0,0)
  ) +
  annot_fun2(surge_ind2021to1971, x1, 35e6, "black") +
  annot_fun2(str_c("+", surge_ind2021to2071), x2, 4.25e7, "white") +
  annot_fun2(surge_pub2021to1971, x3, 2.7e7, "black") +
  annot_fun2(str_c("+", surge_pub2021to2071), x4, 3.1e7, "white") +
  plot_settings +
  theme(plot.title = element_text(size = 12)) 

surge_ind_pub

###Arrange--------------
damage_by_structure <- ggarrange(wind_res_com, wind_ind_pub,
                                 surge_res_com, surge_ind_pub, 
                                 common.legend = T, 
                                 legend = "bottom")

damage_by_structure

ggsave(
  plot = damage_by_structure,
  "figures/damage_by_structure_type.png",
  width = 6,
  height = 4.5)

#Maps--------------
##Map Data----------------
st_layers("tract_data/map_layers.gpkg")

tract_shp <- 
  st_read("tract_data/map_layers.gpkg", layer = "tract_shp") 

nola <- 
  st_read("tract_data/map_layers.gpkg", layer = "nola_shp")

gulf_coast <- 
  st_read("tract_data/map_layers.gpkg", 
          layer = "gulf_coast_shp")

pont_cswy <- 
  st_read("tract_data/map_layers.gpkg", 
          layer = "pont_cswy_shp")

ms <- 
  st_read("tract_data/map_layers.gpkg", layer = "ms_cropped")

ida_track <- 
  st_read("tract_data/map_layers.gpkg", layer = "ida_track")

major_water <- 
  st_read("tract_data/map_layers.gpkg", 
          layer = "major_water_shp")

###Bounding boxes--------------
#main map 
box <- tract_shp %>%
  filter(
    str_sub(tractid, 1, 5) == "22029" | #left top
      str_sub(tractid, 1, 5) == "22045" |
      str_sub(tractid, 1, 5) == "22118" |
      str_sub(tractid, 1, 5) == "22075"
  ) %>%
  st_bbox()

box[1] <- box[1] - 100000 #x min
box[4] <- box[4] - 200000 # ymax
box_poly <- st_as_sfc(box)

#inset map 
zoom_box <- st_bbox(nola)

zoom_box[1] <- zoom_box[1] - 50000 # xmin
zoom_box[3] <- zoom_box[3] - 50000 # xmax
zoom_box[2] <- zoom_box[2] + 5000 # ymin
zoom_box[4] <- zoom_box[4] - 20000 # ymax

zoom_box_poly <- st_as_sfc(zoom_box)

###Merge to tract-level data-------------------
dta_sf <-
  tract_shp %>%
  right_join(dta, by = "tractid") %>%
  st_crop(box_poly) %>%
  mutate(
    across(c(dmg_cc_wind_total_tract_1971to2021_ratio_mlt,
             dmg_cc_wind_total_tract_2021to2071_ratio_mlt),
           ~ factor(.x, levels = c("No Damage",
                                   "Low Damage",
                                   "0 to 24%",
                                   "25 to 49%",
                                   "50 to 99%",
                                   "100% or more"))),
    
    across(c(dmg_cc_surge_total_tract_1971to2021_ratio_mlt,
             dmg_cc_surge_total_tract_2021to2071_ratio_mlt),
           ~ factor(.x, levels = c("No Damage",
                                   "Low Damage",
                                   "-5.0 to -0.1%",
                                   "0.0 to 4.9%",
                                   "5.0 to 9.9%",
                                   "10.0% or more"))),
    across(c(dmg_wind_total_tract_1971_mlt7,
             dmg_wind_total_tract_2021_mlt7,
             dmg_wind_total_tract_2071_mlt7,
             
             dmg_surge_total_tract_1971_mlt7,
             dmg_surge_total_tract_2021_mlt7,
             dmg_surge_total_tract_2071_mlt7),
           ~ factor(.x, levels = c("0",
                                   "1 - 350K",
                                   "350K - 1M",
                                   "1M - 10M",
                                   "10M - 50M",
                                   "> 50M")))
  )

tract_shp_crop <-
  tract_shp %>%
  st_crop(box_poly)

##Attributed:Non-Attributed Damage Ratio Maps------------
###Settings---------
main_map_colors = c("grey90",
                    "grey70",
                    "#ffffcc",
                    "#c7e9b4",
                    "#41b6c4",
                    "#0c2c84")

water_color <- "#C2EBFA"
outline_color = "grey50"
outline_w = .5
nola_color = "navy"
nola_outline_color = "black"
nola_outline_w = 1
ida_track_color = "#d8b365"
ida_lwd = 1.5
ida_dot = .3
text_size = .6

basemap <-
  tm_shape(box_poly, is.main = T) +
  tm_shape(tract_shp_crop, bbox = box) +
  tm_fill(main_map_colors[1]) +
  tm_borders(col = outline_color, lwd = outline_w) 

inset_basemap <-
  tm_shape(zoom_box_poly, is.main = T) +
  tm_shape(tract_shp_crop, bbox = box) +
  tm_fill(main_map_colors[1]) +
  tm_borders(col = outline_color, lwd = outline_w) 

map_elements <-
  tm_layout(text.fontfamily = "serif") +
  tm_shape(major_water %>% st_union(), bbox = box) +
  tm_fill(water_color) +
  tm_borders(col = outline_color, lwd = outline_w) +
  tm_shape(pont_cswy) +
  tm_lines(col = water_color, lwd = 2) +
  tm_shape(gulf_coast) +
  tm_fill(water_color, col = outline_color, lwd = outline_w) +
  tm_shape(zoom_box_poly) +
  tm_borders(nola_outline_color, lwd = nola_outline_w) +
  tm_shape(ms) + tm_fill("grey97", col = outline_color, lwd = outline_w) +
  tm_shape(ida_track) + tm_lines(ida_track_color, lwd = ida_lwd) 

inset_map_elements <-
  tm_title(" ") +
  tm_shape(major_water %>% st_union(), bbox = box) +
  tm_fill(water_color) +
  tm_borders(col = outline_color,
             lwd = outline_w) +
  tm_shape(pont_cswy) +
  tm_lines(col = water_color, lwd = 2) +
  tm_shape(gulf_coast) +
  tm_fill(water_color) +
  tm_borders(col = outline_color,
             lwd = outline_w) +
  tm_title_in("Greater New Orleans Area",
              bg = T, bg.color = "white",
              frame.color = "black", frame.lwd = outline_w,
              size = .75, 
              fontfamily = "serif",
              position = tm_pos_in(0.0025, 1)) +
  tm_options(component.autoscale = F)

### Functions-----------
pretty_main_ratio_map <- function(var) {
  basemap +
    tm_shape(dta_sf) +
    tm_fill(var,
            fill.scale = tm_scale_categorical(values = main_map_colors)) +
    tm_borders(col = outline_color, lwd = outline_w) + 
    tm_compass(position = tm_pos_in(0, 0.2), 
               size = 1.5, color.dark = "grey30") +
    map_elements +
    tm_layout(legend.show = F, frame = F) 
}
pretty_inset_ratio_map <- function(var) {
  inset_basemap +
    tm_shape(dta_sf) +
    tm_fill(var,
            fill.scale = tm_scale_categorical(values = main_map_colors),
            fill.legend = tm_legend(frame = F, 
                                    title = "Percent Change",
                                    title.fontfamily = "serif", 
                                    title.size = text_size + .2,
                                    text.fontfamily = "serif",
                                    text.size = text_size,
                                    margins = c(.5, 0, 0, 0)
            )
    ) +
    tm_borders(col = outline_color, lwd = outline_w) + 
    inset_map_elements +
    tm_add_legend(
      title = "\n",
      labels = " Hurricane Ida Track",
      size = text_size,
      col = ida_track_color,
      lwd = ida_lwd,
      type = "lines"
    ) +
    tm_layout(
      legend.stack = "horizontal",
      legend.text.fontfamily = "serif",
      legend.text.size = text_size,
      frame.color = nola_outline_color,
      frame.lwd = outline_w
    )
}

pretty_arranged_ratio_map <- 
  function(pretty_main1971to2021, pretty_inset1971to2021,
           pretty_main2021to2071, pretty_inset2021to2071) {
    tmap_arrange(pretty_main1971to2021 + 
                   tm_title_in("a) 1971 to 2021", 
                               position = c(0.025, .95), 
                               bg = T,
                               bg.color = "white", 
                               frame.color ="black", size = 1), 
                 pretty_inset1971to2021 + 
                   tm_layout(asp = 1.8),
                 pretty_main2021to2071 +
                   tm_title_in("b) 2021 to 2071", 
                               position = c(0.025, .95),
                               bg = T,
                               bg.color = "white", 
                               frame.color ="black", size = 1),
                 pretty_inset2021to2071 + 
                   tm_layout(asp = 1.8),
                 
                 ncol = 2,
                 widths = c(.65,.35)
    )
  }

### Create maps--------
map_main_wind_1971to2021 <-
  pretty_main_ratio_map("dmg_cc_wind_total_tract_1971to2021_ratio_mlt")

map_inset_wind_1971to2021 <-
  pretty_inset_ratio_map("dmg_cc_wind_total_tract_1971to2021_ratio_mlt")

map_main_wind_2021to2071 <-
  pretty_main_ratio_map("dmg_cc_wind_total_tract_2021to2071_ratio_mlt")

map_inset_wind_2021to2071 <-
  pretty_inset_ratio_map("dmg_cc_wind_total_tract_2021to2071_ratio_mlt")

#surge maps
map_main_surge_1971to2021 <-
  pretty_main_ratio_map("dmg_cc_surge_total_tract_1971to2021_ratio_mlt")

map_inset_surge_1971to2021 <-
  pretty_inset_ratio_map("dmg_cc_surge_total_tract_1971to2021_ratio_mlt")

map_main_surge_2021to2071 <-
  pretty_main_ratio_map("dmg_cc_surge_total_tract_2021to2071_ratio_mlt")

map_inset_surge_2021to2071 <-
  pretty_inset_ratio_map("dmg_cc_surge_total_tract_2021to2071_ratio_mlt")

#arrange
wind_ratio_combined <-
  pretty_arranged_ratio_map(map_main_wind_1971to2021, 
                            map_inset_wind_1971to2021,
                            map_main_wind_2021to2071,
                            map_inset_wind_2021to2071)

surge_ratio_combined <-
  pretty_arranged_ratio_map(map_main_surge_1971to2021, 
                            map_inset_surge_1971to2021,
                            map_main_surge_2021to2071,
                            map_inset_surge_2021to2071)
### Save------------
tmap_save(
  wind_ratio_combined,
  "figures/wind_ratio_combined_maps.png",
  height = 6.8,
  width = 6.3
)

tmap_save(
  surge_ratio_combined,
  "figures/surge_ratio_combined_maps.png",
  height = 6.8,
  width = 6.3
)

##Supplemental Maps---------------------
###Settings--------
supplemental_map_colors = c("grey90",
                            "#ffffcc", 
                            "#c7e9b4",
                            "#7fcdbb",
                            "#41b6c4",
                            "#0c2c84")



###Functions------------
pretty_main_sup_map <- function(var) {
  basemap +
    tm_shape(dta_sf) +
    tm_fill(var,
            fill.scale = 
              tm_scale_categorical(values = supplemental_map_colors)) +
    tm_borders(col = outline_color, lwd = outline_w) + 
    tm_compass(position = tm_pos_in(0, 0.225), 
               size = 1.5, color.dark = "grey30") +
    tm_layout(legend.show = F, frame = F, asp = 1) +
    map_elements
}

pretty_inset_sup_map <- function(var) {
  inset_basemap +
    tm_shape(dta_sf) +
    tm_fill(var,
            fill.scale = 
              tm_scale_categorical(values = supplemental_map_colors),
            fill.legend = tm_legend(frame = F, 
                                    title = "Total Damage",
                                    title.fontfamily = "serif", 
                                    title.size = text_size + .2,
                                    text.fontfamily = "serif",
                                    text.size = text_size - .05,
                                    margins = c(0, 0, .5, 0)
            )) +
    tm_borders(col = outline_color, lwd = outline_w) + 
    inset_map_elements +
    tm_add_legend(
      title = "\n",
      labels = " Hurricane Ida Track",
      size = text_size,
      col = ida_track_color,
      lwd = ida_lwd,
      type = "lines"
    ) +
    tm_layout(
      legend.stack = "horizontal",
      legend.text.fontfamily = "serif",
      legend.text.size = text_size - .05,
      frame.color = nola_outline_color,
      frame.lwd = outline_w
    )
}

pretty_arranged_sup_map <- 
  function(pretty_main1971, pretty_inset1971,
           pretty_main2021, pretty_inset2021,
           pretty_main2071, pretty_inset2071) {
    tmap_arrange(pretty_main1971 + tm_title_in("a) 1971", 
                                               bg = T, bg.color = "white", 
                                               frame.color ="black", size = 1),
                 pretty_inset1971,
                 pretty_main2021 + tm_title_in("b) 2021", 
                                               bg = T, bg.color = "white", 
                                               frame.color ="black", size = 1),
                 pretty_inset2021,
                 pretty_main2071 + tm_title_in("c) 2071", 
                                               bg = T,
                                               bg.color = "white", 
                                               frame.color ="black", size = 1),
                 pretty_inset2071,
                 nrow = 3,
                 widths = c(.65,.35)
    )
  }

###Create maps------------
# wind
map_main_wind_1971 <-
  pretty_main_sup_map("dmg_wind_total_tract_1971_mlt7")

map_inset_wind_1971 <- 
  pretty_inset_sup_map("dmg_wind_total_tract_1971_mlt7")

map_main_wind_2021 <-
  pretty_main_sup_map("dmg_wind_total_tract_2021_mlt7")

map_inset_wind_2021 <-
  pretty_inset_sup_map("dmg_wind_total_tract_2021_mlt7")

map_main_wind_2071 <-
  pretty_main_sup_map("dmg_wind_total_tract_2071_mlt7")

map_inset_wind_2071 <-
  pretty_inset_sup_map("dmg_wind_total_tract_2071_mlt7")

# surge
map_main_surge_1971 <-
  pretty_main_sup_map("dmg_surge_total_tract_1971_mlt7")

map_inset_surge_1971 <- 
  pretty_inset_sup_map("dmg_surge_total_tract_1971_mlt7")

map_main_surge_2021 <-
  pretty_main_sup_map("dmg_surge_total_tract_2021_mlt7")

map_inset_surge_2021 <-
  pretty_inset_sup_map("dmg_surge_total_tract_2021_mlt7")

map_main_surge_2071 <-
  pretty_main_sup_map("dmg_surge_total_tract_2071_mlt7")

map_inset_surge_2071 <-
  pretty_inset_sup_map("dmg_surge_total_tract_2071_mlt7")

#arrange
wind_total_damage_combined <-
  pretty_arranged_sup_map(map_main_wind_1971, map_inset_wind_1971,
                          map_main_wind_2021, map_inset_wind_2021,
                          map_main_wind_2071, map_inset_wind_2071)
surge_total_damage_combined <-
  pretty_arranged_sup_map(map_main_surge_1971, map_inset_surge_1971,
                          map_main_surge_2021, map_inset_surge_2021,
                          map_main_surge_2071, map_inset_surge_2071)

###Save----------
tmap_save(
  wind_total_damage_combined,
  "figures/supplemental_wind_total_damage.png",
  height = 8.4,
  width = 5.17
)

tmap_save(
  surge_total_damage_combined,
  "figures/supplemental_surge_total_damage.png",
  height = 8.4,
  width = 5.17
)




