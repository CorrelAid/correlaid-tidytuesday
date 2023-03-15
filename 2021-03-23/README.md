UN VOTES 🇺🇳
================

# Animated Graph No. 1

**By Patrizia Maier**


```r
# get packages 
library(tidyverse)
library(rnaturalearth)
library(tmap)
library(gifski)
library(extrafont)
# font_import() # only once 
loadfonts(device = "win", quiet = TRUE) # every time 
```

```r
# get tidy tuesday data 
tuesdata <- tidytuesdayR::tt_load('2021-03-23')
unvotes <- tuesdata$unvotes
roll_calls <- tuesdata$roll_calls
issues <- tuesdata$issues
```

````r
# get map data 
world <- ne_countries(returnclass='sf') %>% 
  select("iso_a2", "geometry")
````  

````r
# combine data 
temp <- unvotes %>%
  left_join(roll_calls, by="rcid") %>%
  left_join(issues, by="rcid") %>% 
  filter(importantvote==1)

data <- world %>% left_join(temp %>% 
                              filter(issue=="Nuclear weapons and nuclear material") %>%
                              group_by(date), 
                            by=c("iso_a2"="country_code")) %>%
  drop_na(date) %>%
  arrange(date) %>%
  mutate(vote=factor(vote, levels=c("yes", "abstain", "no")))
  
``````

````r
# create map
tmap_style("beaver")
animation <- tm_shape(data) + 
  tm_fill(col="vote", legend.show=TRUE, palette="-RdYlGn") + 
  tm_facets(by="date", free.coords=FALSE, drop.units=TRUE,  drop.NA.facets=TRUE, nrow = 1, ncol = 1) +
  tm_layout(frame=F,
            main.title="UN Votes \non Important Issues of Nuclear Weapons and Material", 
            main.title.position="left",
            main.title.size=2,
            main.title.color="black", 
            legend.position=c("left", "bottom"),
            legend.title.size=1.5,
            legend.text.size=1.25,
            panel.label.size=1.5,
            panel.label.bg.color="#5b92e5",
            panel.label.color="white",
            fontfamily="Bahnschrift") + 
  tm_shape(data) + 
  tm_borders(col="black") + 
  tm_logo("https://upload.wikimedia.org/wikipedia/commons/thumb/e/ee/UN_emblem_blue.svg/1000px-UN_emblem_blue.svg.png",
          position=c("center", "bottom"),
          height=4)+ 
  tm_credits("by @PatriziaMaier for #TidyTuesday     \nData: Harvard's Dataverse", 
             position=c("right", "bottom"),
             size=0.8, 
             bg.color="#5b92e5",
             bg.alpha=0.95,
             col="white") 

````


![](README_files/figure-gfm/un_votes.gif)

# Static No.1

**By Lukas Warode**

![](README_files/figure-gfm/all_plots.png)

# Shiny App

**By Long Nguyen**

https://long39ng.shinyapps.io/unvotes-agreement-map/ 

![](README_files/figure-gfm/un-votes-shiny-app-long.png)
