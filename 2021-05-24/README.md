Mario Kart 64 🍄
================

## Double-Dumbbell Plot by Cédric Scherer

``` r
library(tidyverse)
library(ggtext)
library(here)
library(ragg)

theme_set(theme_minimal(base_family = "Atlantis", base_size = 13))

theme_update(
  plot.margin = margin(25, 15, 15, 25),
  plot.background = element_rect(color = "#FFFCFC", fill = "#FFFCFC"),
  panel.grid.major.x = element_line(color = "grey94"),
  panel.grid.major.y = element_blank(),
  panel.grid.minor = element_blank(),
  axis.text = element_text(family = "Hydrophilia Iced"),
  axis.text.x = element_text(color = "grey40"),
  axis.text.y = element_blank(),
  axis.title = element_blank(),
  axis.ticks = element_blank(),
  legend.position = c(.07, .31), 
  legend.title = element_text(color = "grey40", family = "Overpass", angle = 90, hjust = .5),
  legend.text = element_text(color = "grey40", family = "Hydrophilia Iced", size = 12),
  legend.box = "horizontal",
  legend.box.just = "bottom",
  legend.margin = margin(0, 0, 0, 0),
  legend.spacing = unit(.6, "lines"),
  plot.title = element_text(family = "Atlantis Headline", face = "bold", size = 17.45),
  plot.subtitle = element_textbox_simple(family = "Overpass", color = "grey40", size = 10.8,
                                         lineheight = 1.3, margin = margin(t = 5, b = 30)),
  plot.caption = element_text(family = "Overpass", color = "grey55", 
                              size = 10.5, margin = margin(t = 20, b = 0, r = 15))
)

## data
df_records <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-25/records.csv')
df_rank <- 
  df_records %>% 
  filter(type == "Three Lap") %>%  
  group_by(track) %>% 
  filter(time == min(time)) %>% 
  ungroup %>% 
  arrange(-time) %>% 
  mutate(track = fct_inorder(track, time))

df_records_three <-
  df_records %>% 
  filter(type == "Three Lap") %>% 
  mutate(year = lubridate::year(date)) %>% 
  mutate(track = factor(track, levels = levels(df_rank$track)))

df_connect <- 
  df_records_three %>% 
  group_by(track, type, shortcut) %>% 
  summarize(no = min(time), yes = max(time)) %>% 
  pivot_longer(cols = -c(track, type, shortcut),
               names_to = "record", values_to = "time") %>% 
  filter((shortcut == "No" & record == "no") | (shortcut == "Yes" & record == "yes")) %>% 
  pivot_wider(id_cols = c(track), values_from = time, names_from = record)

df_longdist <- 
  df_records_three %>% 
  filter(shortcut == "No") %>% 
  group_by(track) %>% 
  filter(time == min(time) | time == max(time)) %>% 
  mutate(group = if_else(time == min(time), "min", "max")) %>% 
  group_by(track, group) %>%
  arrange(time) %>% 
  slice(1) %>% 
  group_by(track) %>% 
  mutate(year = max(year)) %>% 
  pivot_wider(id_cols = c(track, year), values_from = time, names_from = group) %>% 
  mutate(diff = max - min) 

df_shortcut <- 
  df_records_three %>% 
  filter(shortcut == "Yes") %>% 
  group_by(track) %>% 
  filter(time == min(time) | time == max(time)) %>% 
  mutate(group = if_else(time == min(time), "min", "max")) %>% 
  group_by(track, group) %>%
  arrange(time) %>% 
  slice(1) %>% 
  group_by(track) %>% 
  mutate(year = max(year)) %>% 
  pivot_wider(id_cols = c(track, year), values_from = time, names_from = group) %>% 
  mutate(diff = max - min)

## plot
df_shortcut %>% 
  ggplot(aes(min, track)) +
  ## dotted line connection shortcut yes/no
  geom_linerange(data = df_connect, aes(xmin = yes, xmax = no, y = track), 
                 inherit.aes = FALSE, color = "grey75", linetype = "11") +
  ## segment shortcut yes
  geom_linerange(aes(xmin = min, xmax = max, color = diff), size = 2) +
  ## segment shortcut no
  geom_linerange(data = df_longdist, aes(xmin = min, xmax = max, color = diff), size = 2) +
  geom_linerange(data = df_longdist, aes(xmin = min, xmax = max), color = "#FFFCFC", size = .8) +
  ## point shortcut yes – first record
  geom_point(aes(x = max), size = 7, color = "#FFFCFC", fill = "grey65", shape = 21, stroke = .7) +
  ## point shortcut yes – latest record
  geom_point(size = 7, color = "#FFFCFC", fill = "#6b7ea6", shape = 21, stroke = .7) +
  geom_point(aes(fill = year), size = 7, color = "#FFFCFC", shape = 21, stroke = .7) +
  ## point shortcut no – first record
  geom_point(data = df_longdist, aes(fill = year), size = 5.6, shape = 21, 
             color = "#FFFCFC", stroke = .5) +
  geom_point(data = df_longdist, size = 3, color = "#FFFCFC") +
  ## point shortcut no – latest record
  geom_point(data = df_longdist, aes(x = max), size = 5.6, shape = 21, 
             fill = "grey65", color = "#FFFCFC", stroke = .5) +
  geom_point(data = df_longdist, aes(x = max), size = 3, color = "#FFFCFC") +
  ## labels tracks
  geom_label(aes(label = track), family = "Atlantis", size = 6.6, hjust = 1, nudge_x = -7,
             label.size = 0, fill = "#FFFCFC") +
  geom_label(data = filter(df_longdist, !track %in% unique(df_shortcut$track)), 
             aes(label = track), family = "Atlantis", size = 6.6, hjust = 1, nudge_x = -7,
             label.size = 0, fill = "#FFFCFC") +
  ## labels dots shortcut yes
  geom_text(data = filter(df_shortcut, track == "Wario Stadium"),
             aes(label = "Most recent record\nwith shortcuts"), 
             family = "Overpass", size = 3.5, color = "#4a5a7b", 
             lineheight = .8, vjust = 0, nudge_y = .4) +
  geom_text(data = filter(df_shortcut, track == "Wario Stadium"),
             aes(x = max, label = "First record\nwith shortcuts"), 
             family = "Overpass", size = 3.5, color = "grey50", 
             lineheight = .8, vjust = 0, nudge_y = .4) +
  ## labels dots shortcut no
  geom_text(data = filter(df_longdist, track == "Wario Stadium"),
             aes(label = "Most recent record\nw/o shortcuts"), 
             family = "Overpass", size = 3.5, color = "#4a5a7b", lineheight = .8, 
             vjust = 0, nudge_x = -7, nudge_y = .4) +
  geom_text(data = filter(df_longdist, track == "Wario Stadium"),
             aes(x = max, label = "First record\nw/o shortcuts"), 
             family = "Overpass", size = 3.5, color = "grey50", lineheight = .8, 
             vjust = 0, nudge_x = 7, nudge_y = .4) +
  coord_cartesian(xlim = c(-60, 400)) +
  scale_x_continuous(breaks = seq(0, 400, by = 50), 
                     labels = function(x) ifelse(x == 0, paste(x, "seconds"), paste(x)),
                     sec.axis = dup_axis(),
                     expand = c(.02, .02)) +
  scale_y_discrete(expand = c(.07, .07)) +
  scale_fill_gradient(low = "#b4d1d2", high = "#242c3c", name = "Year of Record") +
  rcartocolor::scale_color_carto_c(palette = "RedOr", limits = c(0, 250),
                                   name = "Time difference between first and most recent record") +
  guides(fill = guide_legend(title.position = "left"),
         color = guide_colorbar(barwidth = unit(.45, "lines"), barheight = unit(22, "lines"),
                                title.position = "left")) +
  labs(title = "Let's-a-Go!  You  May  Still  Have  Chances  to  Grab  a  New  World  Record  for  Mario  Kart  64",
       subtitle = "Most world records for Mario Kart 64 were achieved pretty recently (13 in 2020, 10 in 2021). On several tracks, the players considerably improved the time needed to complete three laps when they used shortcuts (*Choco Mountain*, *D.K.'s Jungle Parkway*, *Frappe Snowland*, *Luigi Raceway*, *Rainbow Road*, *Royal Raceway*, *Toad's Turnpike*, *Wario Stadium*, and *Yoshi Valley*). Actually, for three out of these tracks the previous records were more than halved since 2020 (*Luigi Raceway*, *Rainbow Road*, and *Toad's Turnpike*). Four other tracks still have no records for races with shortcuts (*Moo Moo Farm*, *Koopa Troopa Beach*, *Banshee Boardwalk*, and *Bowser's Castle*). Are there none or did nobody find them yet? Pretty unrealistic given the fact that since more than 24 years the game is played all around the world—but maybe you're able to find one and obtain a new world record?",
       caption = "Visualization: Cédric Scherer  •  Data: mkwrs.com/mk64")

ggsave(here::here("plots", "2021_22", "2021_22_MarioKart.png"),
       width = 7990, height = 5200, res = 500, device = agg_png, limitsize = FALSE)
```

![](https://raw.githubusercontent.com/Z3tt/TidyTuesday/master/plots/2021_22/2021_22_MarioKart.png)


##### TidyTuesday 2021-05-27 #####
###         Mario Kart         ###
### Author: Patrizia Maier     ###

``` r
# get packages 
library(tidyverse)
library(lubridate)
library(Rokemon)
library(ggtext)
library(ggrepel)
library(extrafont)
# import_pokefont() # only once
# font_import() # only once 
loadfonts(device = "win", quiet = TRUE) # every time 
# windowsFonts() # to check available options 



# get tidy tuesday data 
tuesdata <- tidytuesdayR::tt_load('2021-05-25')
records <- tuesdata$records
drivers <- tuesdata$drivers


# tidy data 
temp <- records %>% 
  filter(type=="Three Lap") %>% 
  mutate(year=year(date),
         shortcut=fct_recode(shortcut, sc="Yes", nosc="No")) %>% 
  group_by(track, shortcut) %>% 
  summarize(first_rec_time=as.character(max(time)),
            first_rec_player=as.character(player[which.max(time)]),
            first_rec_year=as.character(year[which.max(time)]),
            latest_rec_time=as.character(min(time)),
            latest_rec_player=as.character(player[which.min(time)]),
            latest_rec_year=as.character(year[which.min(time)])) %>%
  pivot_longer(
    cols=-c(track, shortcut),
    names_to = c("record", "category"),
    names_sep = "_rec_",
    values_to = "value"
  ) %>% 
  pivot_wider(
    names_from = "category"
  ) %>% 
  mutate(shortcut=as.factor(shortcut),
         record=as.factor(record),
         shortcut_record=interaction(shortcut, record, sep="_"),
         time=as.numeric(time),
         year=as.integer(year),
         cup=as.factor(case_when(track %in% c("Luigi Raceway", "Moo Moo Farm", "Koopa Troopa Beach", "Kalimari Desert")  ~ "Mushroom Cup",
                                 track %in% c("Toad's Turnpike", "Frappe Snowland", "Choco Mountain", "Mario Raceway") ~ "Flower Cup",
                                 track %in% c("Wario Stadium", "Sherbet Land", "Royal Raceway", "Bowser's Castle") ~ "Star Cup",
                                 track %in% c("D.K.'s Jungle Parkway" , "Yoshi Valley" , "Banshee Boardwalk", "Rainbow Road") ~ "Special Cup"
                                 )),
         cup=fct_relevel(cup, "Mushroom Cup", "Flower Cup", "Star Cup", "Special Cup"))
                       

# plotting 
ggplot(temp, aes(x=time, y=track, color=record, shape=shortcut)) + 
  geom_point(size=4) +
  facet_wrap(~ cup, nrow=4, strip.position = "right", scales = "free_y", labeller = label_wrap_gen(width=8)) + 
  geom_text_repel(aes(label=player), size=2.5, color="black") +
  scale_y_discrete(limits=rev) + 
  scale_color_manual(values=c("#c51921", "#2527bf"), labels=c("first record", "latest record")) +  
  scale_shape_manual(values=c(19, 17), labels=c("no shortcut", "shortcut")) +
  coord_cartesian(clip="off") + 
  theme_minimal() + 
  guides(colour = guide_legend(order = 1), shape = guide_legend(order = 2)) + 
  theme(plot.title=element_markdown(size=20),
        plot.subtitle=element_text(size=10),
        plot.title.position = "plot",
        plot.caption = element_text(size=7),
        plot.caption.position = "plot",
        plot.margin = unit(c(1,1,0.5,1), "cm"),
        plot.background = element_rect(fill="#c1bebf"),
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(colour = "#726d6f", linetype = 2),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(0.5, "lines"),
        strip.placement = "outside",
        strip.background = element_rect(fill = "#d7751a"),
        strip.text.y = element_text(angle = 0, color = "white"),
        legend.position = "top",
        legend.title = element_blank(),
        text = element_text(family = "pokemon-font" , size=9), 
        axis.text = element_text(size=8)) + 
  labs(title="<span style = 'color:#e3e31c;'>Mario Kart</span><span style = 'color:#e21d26;'> 64</span>",
       subtitle="\nWorld records for Three Laps in seconds",
       caption="Dataviz: Patrizia Maier | Data: Mario Kart Word Records & Benedikt Claus",
       x="",
       y="")

# save
ggsave("C:/Users/Patrizia/Data/Data Science/TidyTuesday/mario_kart_2021.png",
       width = 12, height = 8)
``` 
