Canadian Wind Turbines 💨
================

# TidyTuesday 2020-10-27

If you want to join the next CorrelAid TidyTuesday Meetup, make sure to
sign up to our
[Newsletter](https://correlaid.us12.list-manage.com/subscribe?u=b294bf2834adf5d89bdd2dd5a&id=915f3f3eff)
or reach out to us on [Twitter](https://twitter.com/CorrelAid)\!

``` r
library(tidyverse)
library(tidytuesdayR)
#library(fontawesome) # for knitting, install with devtools::install_github("rstudio/fontawesome")

tt <- tt_load("2020-10-27")
```

    ## 
    ##  Downloading file 1 of 1: `wind-turbine.csv`

``` r
df_wind <- tt$`wind-turbine`
```

\`\`\`{r} explore-data, fig.width = 30, fig.height = 30 df\_wind\_red
\<- df\_wind %\>% dplyr::select(-project\_name, -turbine\_identifier,
-turbine\_number\_in\_project, -manufacturer, -model,
-commissioning\_date, -notes)

GGally::ggpairs(df\_wind\_red) \`\``  `
