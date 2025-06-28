## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
# # R output pre blocks are styled by default to indicate output
# knitr::opts_chunk$set(comment = NA)

## ----eval=FALSE---------------------------------------------------------------
# install.packages("geofacet")
# # or from github:
# # remotes::install_github("hafen/geofacet")

## -----------------------------------------------------------------------------
library(geofacet)
library(ggplot2)

## -----------------------------------------------------------------------------
head(state_ranks)

## ----state_rank, fig.width=12, fig.height=7-----------------------------------
ggplot(state_ranks, aes(variable, rank, fill = variable)) +
  geom_col() +
  coord_flip() +
  theme_bw() +
  facet_geo(~ state)

## -----------------------------------------------------------------------------
head(us_state_grid2)

## ----state_unemp, fig.width=12, fig.height=8----------------------------------
ggplot(state_unemp, aes(year, rate)) +
  geom_line() +
  facet_geo(~ state, grid = "us_state_grid2", label = "name") +
  scale_x_continuous(labels = function(x) paste0("'", substr(x, 3, 4))) +
  labs(title = "Seasonally Adjusted US Unemployment Rate 2000-2016",
    caption = "Data Source: bls.gov",
    x = "Year",
    y = "Unemployment Rate (%)") +
  theme(strip.text.x = element_text(size = 6))

## ----eu_gdp, fig.width=10, fig.height=8---------------------------------------
ggplot(eu_gdp, aes(year, gdp_pc)) +
  geom_line(color = "steelblue") +
  facet_geo(~ name, grid = "eu_grid1", scales = "free_y") +
  scale_x_continuous(labels = function(x) paste0("'", substr(x, 3, 4))) +
  ylab("GDP Per Capita in Relation to EU Index (100)") +
  theme_bw()

## -----------------------------------------------------------------------------
get_grid_names()

## ----message=FALSE------------------------------------------------------------
my_grid <- us_state_grid1
my_grid$col[my_grid$code == "WI"] <- 7
grid_preview(my_grid)

## ----aus_pop, fig.width=8, fig.height=6.5-------------------------------------
ggplot(aus_pop, aes(age_group, pop / 1e6, fill = age_group)) +
  geom_col() +
  facet_geo(~ code, grid = "aus_grid1") +
  coord_flip() +
  labs(
    title = "Australian Population Breakdown",
    caption = "Data Source: ABS Labour Force Survey, 12 month average",
    y = "Population [Millions]") +
  theme_bw()

## ----sa_pop, fig.width=8, fig.height=6.5--------------------------------------
ggplot(sa_pop_dens, aes(factor(year), density, fill = factor(year))) +
  geom_col() +
  facet_geo(~ province, grid = "sa_prov_grid1") +
  labs(title = "South Africa population density by province",
    caption = "Data Source: Statistics SA Census",
    y = "Population density per square km") +
  theme_bw()

## ----london_afford, fig.width=11.4, fig.height=7.7----------------------------
ggplot(london_afford, aes(x = year, y = starts, fill = year)) +
  geom_col(position = position_dodge()) +
  facet_geo(~ code, grid = "gb_london_boroughs_grid", label = "name") +
  labs(title = "Affordable Housing Starts in London",
    subtitle = "Each Borough, 2015-16 to 2016-17",
    caption = "Source: London Datastore", x = "", y = "")

## ----nhs_scot, fig.width=6.2, fig.height=7.7----------------------------------
ggplot(nhs_scot_dental, aes(x = year, y = percent)) +
  geom_line() +
  facet_geo(~ name, grid = "nhs_scot_grid") +
  scale_x_continuous(breaks = c(2004, 2007, 2010, 2013)) +
  scale_y_continuous(breaks = c(40, 60, 80)) +
  labs(title = "Child Dental Health in Scotland",
    subtitle = "Percentage of P1 children in Scotland with no obvious decay experience.",
    caption = "Source: statistics.gov.scot", x = "", y = "")

## ----india_pop, fig.width=8.6, fig.height=7.7---------------------------------
ggplot(subset(india_pop, type == "state"),
  aes(pop_type, value / 1e6, fill = pop_type)) +
  geom_col() +
  facet_geo(~ name, grid = "india_grid2", label = "code") +
  labs(title = "Indian Population Breakdown",
       caption = "Data Source: Wikipedia",
       x = "",
       y = "Population [Millions]") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))

## ----election1, fig.width=12, fig.height=8------------------------------------
ggplot(election, aes("", pct, fill = candidate)) +
  geom_col(alpha = 0.8, width = 1) +
  scale_fill_manual(values = c("#4e79a7", "#e15759", "#59a14f")) +
  facet_geo(~ state, grid = "us_state_grid2") +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "2016 Election Results",
    caption = "Data Source: 2016 National Popular Vote Tracker",
    x = NULL,
    y = "Percentage of Voters") +
  theme(axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    strip.text.x = element_text(size = 6))

## ----election2, fig.width=12, fig.height=8------------------------------------
ggplot(election, aes(candidate, pct, fill = candidate)) +
  geom_col() +
  scale_fill_manual(values = c("#4e79a7", "#e15759", "#59a14f")) +
  facet_geo(~ state, grid = "us_state_grid2") +
  theme_bw() +
  coord_flip() +
  labs(title = "2016 Election Results",
    caption = "Data Source: 2016 National Popular Vote Tracker",
    x = NULL,
    y = "Percentage of Voters") +
  theme(strip.text.x = element_text(size = 6))

## ----election3, fig.width=12, fig.height=8------------------------------------
ggplot(election, aes(candidate, votes / 1000000, fill = candidate)) +
  geom_col() +
  scale_fill_manual(values = c("#4e79a7", "#e15759", "#59a14f")) +
  facet_geo(~ state, grid = "us_state_grid2") +
  coord_flip() +
  labs(title = "2016 Election Results",
    caption = "Data Source: 2016 National Popular Vote Tracker",
    x = NULL,
    y = "Votes (millions)") +
  theme(strip.text.x = element_text(size = 6))

