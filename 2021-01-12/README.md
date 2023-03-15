Art Collections 🎨
================

# Plot No. 1 

**By Long Nguyen**

```r
knitr::opts_chunk$set(echo = TRUE, collapse = TRUE, comment = "#>",
                      fig.path = "figs/", dpi = 300,
                      dev.args = list(bg = "transparent"))
knitr::knit_hooks$set(optipng = knitr::hook_optipng)
```

```r
library(tidyverse)
library(magick)
library(furrr)
```

```r
artwork <- rio::import("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-12/artwork.csv",
                       setclass = "tibble")
plan(multisession)
artwork <- artwork %>% 
  mutate(median_col = future_map_chr(
    thumbnailUrl,
    possibly(~ .x %>% 
               image_read() %>% 
               image_quantize(1, "rgb") %>% 
               # Credit: https://chichacha.netlify.app/2019/01/19/extracting-colours-from-your-images-with-image-quantization/
               imager::magick2cimg() %>% 
               as.data.frame(wide = "c") %>% 
               slice(1) %>% 
               mutate(hex = rgb(c.1, c.2, c.3)) %>% 
               pull(hex),
             otherwise = NA_character_))
  )
saveRDS(artwork, here::here("2021_03_art_collections/artwork.RDS"))
```

```r
artwork <- readRDS(here::here("2021_03_art_collections/artwork.RDS"))
artwork_by_year <- artwork %>% 
  drop_na(year, median_col) %>% 
  mutate(id = fct_reorder(factor(id), year)) %>% 
  arrange(id)
artwork_by_year %>%
  mutate(year = if_else(id %in% {artwork_by_year %>% 
                          filter(year >= 1700, year %% 100 == 0) %>% 
                          group_by(year) %>% 
                          slice_min(id) %>% 
                          pull(id)},
                        year,
                        NA_integer_)) %>% 
  ggplot(aes(id, 1, fill = median_col)) +
  geom_tile() +
  geom_text(aes(label = year), nudge_y = -.55, family = "Manrope Medium") +
  scale_fill_identity() +
  coord_cartesian(clip = "off") +
  theme_void(base_family = "Manrope Extra Bold") +
  theme(plot.margin = margin(15, 40, 15, 40)) +
  labs(title = "Median colours of artwork in the Tate art museum",
       caption = "Data: Tate")
```



![](https://raw.githubusercontent.com/long39ng/puig_orgaladhad/main/2021_03_art_collections/figs/stripes-plot-1.png)


