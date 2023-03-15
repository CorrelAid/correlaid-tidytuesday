Wealth and income over time 🏚️
================


# Plot No. 1

**By Martin Wong**

```r
library(tidyverse)
library(tidytuesdayR)
library(CGPfunctions)
library(patchwork)

```
```r
tt <- tt_load("2021-02-09")
```
```r
income_mean <- tt$income_mean
income_aggregate <- tt$income_aggregate
income_aggregate_all <- income_aggregate %>%
  filter(race == "All Races") %>%
  filter(year == 1967 | year==1977 | year==1987  | year==1997 | year==2007 | year == 2019 )

income_aggregate_all$year <- factor(income_aggregate_all$year,levels = c("1967", "1977", "1987", "1997", "2007", "2019"), labels = c("1967", "1977", "1987", "1997", "2007", "2019"), ordered = TRUE)

income_mean_pivot <- income_mean %>%
  filter(year == 1987 | year == 2019 ) %>%
  filter(dollar_type == "2019 Dollars") %>%
  filter(income_dollars != "EMPTY") %>%
  filter(race!="Asian Alone" & race!="Black Alone or in Combination")  %>%
  pivot_wider(names_from = year, values_from = income_dollars)

income_mean_growth <- income_mean_pivot %>%
mutate(income_change=((`2019`-`1987`)/`1987`)*100)


theme_set(theme_minimal())
```
```r
p1=newggslopegraph(
  income_aggregate_all,
  year,income_share,
  income_quintile, 
  Title = "Share of aggregate income (US)",
  SubTitle = "By income qunintile",
  LineColor = c("Fourth" = "gray", "Highest" = "green", "Top 5%" = "red", "Third" = "gray", "Second" = "gray","Lowest" =
                  "gray"),
  XTextSize = 10,
  TitleTextSize = 14,
  Caption = NULL,
  
)

theme_set(theme_minimal())
```
```r
p2 = ggplot(income_mean_growth,aes(x = reorder(income_quintile, -income_change),y=income_change,fill=race))+
  geom_col(width=0.6)+ 
  coord_flip()+
  facet_wrap(~ race, nrow = 1)+
  theme(strip.text = element_text(
    hjust = 0, size = 11))+
  theme(panel.grid.major.y = element_blank())+
  theme(panel.grid.minor.x = element_blank())+
  scale_fill_brewer(palette="Set1")+
  theme(legend.position = "none")+
  labs(
    x =  "Income quintile" ,
    y ="Change in average income (%)",
    title="Change in average income between 1987 and 2019 (US)",
    caption="Source: US Census"
  )+
  theme(plot.title = element_text(face = "bold",
                                  margin = margin(10, 0, 10, 0),
                                  size = 14))+
  theme(axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)))
 ```       
        
```r
p1/p2

ggsave("20210209_income_inequality_2.png", width = 9, height = 7, dpi = 800)
```

![](README_files/figure-gfm/income_inequality_martin_wong.png)
