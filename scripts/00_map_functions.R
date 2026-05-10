
#Functions to create Ida damage and percent change maps-------------------------

##Total damage map functions (raw values) --------------------------------------
fun_main_damage_map <- 
  function(var) {
    basemap +
      tm_shape(dta_sf) +
      tm_fill(var,
              fill.scale = 
                tm_scale_categorical(values = damage_map_colors)) +
      tm_borders(col = outline_color, lwd = outline_w) + 
      tm_compass(position = tm_pos_in(0, 0.225), 
                 size = 1.5, color.dark = "grey30") +
      tm_layout(legend.show = F, frame = F, asp = 1) +
      map_elements
}

fun_inset_damage_map <- 
  function(var) {
    inset_basemap +
      tm_shape(dta_sf) +
      tm_fill(var,
              fill.scale = 
                tm_scale_categorical(values = damage_map_colors),
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

fun_arrange_maps <- 
  function(pretty_main1971to2021, 
           pretty_inset1971to2021,
           pretty_main2021to2071, 
           pretty_inset2021to2071) {
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

fun_arrange_maps <- 
  function(pretty_main1971, 
           pretty_inset1971,
           pretty_main2021, 
           pretty_inset2021,
           pretty_main2071, 
           pretty_inset2071) {
    tmap_arrange(pretty_main1971 + 
                   tm_title_in("a) 1971", bg = T, bg.color = "white", 
                               frame.color ="black", size = 1),
                 pretty_inset1971,
                 pretty_main2021 + 
                   tm_title_in("b) 2021", bg = T, bg.color = "white", 
                               frame.color ="black", size = 1),
                 pretty_inset2021,
                 pretty_main2071 + 
                   tm_title_in("c) 2071", bg = T, bg.color = "white", 
                               frame.color ="black", size = 1),
                 pretty_inset2071,
                 nrow = 3,
                 widths = c(.65,.35)
    )
  }


##Total damage percent change maps---------------------------------------------
fun_main_damage_pchange_map <- 
  function(var) {
    basemap +
      tm_shape(dta_sf) +
      tm_fill(var,
              fill.scale = tm_scale_categorical(values = damage_pchange_map_colors)) +
      tm_borders(col = outline_color, lwd = outline_w) + 
      tm_compass(position = tm_pos_in(0, 0.2), 
                 size = 1.5, color.dark = "grey30") +
      map_elements +
      tm_layout(legend.show = F, frame = F) 
  }
fun_inset_damage_pchange_map <- 
  function(var) {
    inset_basemap +
      tm_shape(dta_sf) +
      tm_fill(var,
              fill.scale = tm_scale_categorical(values = damage_pchange_map_colors),
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

fun_arrange_pchange_map <- 
  function(pretty_main1971to2021, 
           pretty_inset1971to2021,
           pretty_main2021to2071, 
           pretty_inset2021to2071) {
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