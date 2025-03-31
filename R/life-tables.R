# Life tables in R using the tidyverse
# Monica Alexander

rm(list=ls())
library(tidyverse) # data manipulation and ggplot functions
library(kableExtra) # format tables
library(janitor) # to easily clean up column names


# Ontario 2015 female life table ------------------------------------------

# load Ontario 2015 female survivorship data, define 5-year age groups
lt <- read_table(
  "http://www.prdh.umontreal.ca/BDLC/data/ont/fltper_5x5.txt", skip = 2)
lt <- lt |> 
  filter(Year=="2015-2019") |> 
  mutate(x = c(0,1,seq(5, 110, by = 5)),
         n = lead(x, default = Inf)-x)

# let's look at the data
lt |> 
  select(x,n, lx) |>
  kable()

# plot lx and divide through by 100,000,  
# so lx is the proportion of the population surviving at age x
lx_plot <- lt |> 
  mutate(lx = lx/100000) |> 
  ggplot(aes(x, lx)) +
  geom_line() + 
  geom_point() +
  xlab("age") + 
  theme_bw(base_size = 14) + 
  ggtitle("Female Survivorship", "Ontario 2015")
lx_plot

# look at estimated dx values for Ontario in 2015:
lt |> 
  select(x,n, lx, dx) |> 
  kable()

# probabilities of death (qx) and survival (px)
lt |> 
  mutate(px = 1- qx) |> 
  select(x, n, lx, dx, qx, px) |> 
  kable()

# average years lived (ax) and person-years lived (Lx)
lt |> 
  select(x,n, lx, dx, ax, Lx) |> 
  kable()

# visualizing Lx graphically (Figure 1 in article)
Lx_plot <- lt |> 
  mutate(lx = lx/100000) |> 
  ggplot(aes(x, lx)) +
  geom_line() + 
  xlab("age") + 
  theme_bw(base_size = 14) + 
  geom_vline(xintercept = 15, lty = 2, color = "red")+
  geom_vline(xintercept = 30, lty = 2, color = "red")+
  ggtitle("Female Survivorship", "Ontario 2015")
Lx_plot

# the full life table, with person-years lived above age x (Tx)
# and life expectancy (ex) added 
lt |> 
  select(x,n, lx, dx, ax, Lx, Tx, ex) |> 
  kable()


# Make your own life table ------------------------------------------------

# Period life table for females in Quebec in 2015 
# Data from the Canadian Human Mortality Database 
# Read in data from the website, and filter out what we need:
Deaths <- read_table(
  "https://www.prdh.umontreal.ca/BDLC/data/ont/Deaths_5x1.txt",
  skip = 2, col_types = 'ccddd'
)
Pop <- read_table(
  "https://www.prdh.umontreal.ca/BDLC/data/ont/Population5.txt",
  skip = 2, col_types = 'ccddd'
)

Deaths <- Deaths |>
  mutate(year = as.numeric(substr(Year, 1, 4))) |> 
  select(year, Age, Total) |> 
  clean_names() |> 
  rename(D = total)
Pop <- Pop |>
  mutate(year = as.numeric(substr(Year, 1, 4))) |> 
  select(year, Age, Total) |> 
  clean_names() |> 
  rename(N = total)

# join population and deaths
d <- Deaths |>
  left_join(Pop, by = c("year", "age"))

# convert `age` from character to numeric and
# aggregate ages 95+ because older ages have varying data quality
d <- d |>
  mutate(x = as.numeric(str_remove(age, "-.*|\\+"))) |>
  mutate(x = ifelse(x >= 95, 95, x)) |>
  group_by(year, x) |>
  summarise(
    D = sum(D),
    N = sum(N),
    .groups = "drop"
  ) |>
  mutate(
    age = case_when(
      x==0 ~ "0",
      x==1 ~ "1-4",
      x==95 ~ "95+",
      TRUE ~ paste0(x, "-", x+4)
    )
  ) |>
  select(year, age, x, N, D)

# Use `tidyverse` to calculate the columns in the life table, 
# based on the equations presented in previous sections 
# Set radix l0 = 1 and filter to just include the year 2015 
lt_2015 <- d |> 
  filter(year==2015) |> 
  mutate(
    n = lead(x, default = Inf) - x,
    Mx = D/N,
    ax = case_when(
      x==0 ~ 0.07 + 1.7*Mx,
      x==1 ~ 1.5,
      x==95 ~ 1/Mx,
      TRUE ~ 2.5
    ),
    qx = case_when(
      x==95 ~ 1,  # set to 1 for open interval
      TRUE ~ n * Mx / (1 + (n - ax)* Mx)
    ),
    px = 1 - qx,
    lx = lag(cumprod(px), default = 1),
    dx = lx - lead(lx, default = 0),
    Lx = case_when(
      x==95 ~ lx/Mx,
      TRUE ~ n * lead(lx, default = 0) + (ax* dx)
    ),
    Tx = rev(cumsum(rev(Lx))),
    ex = Tx / lx
  )

# Create Table 2
library(kableExtra)
lt_2015 |>
  select(Age = age, x, n, Mx, ax, qx, px, lx, dx, Lx, Tx, ex) %>%
  kable(format = "latex", booktabs = T,
        digits = c(NA, 0, 0, 6, 3, 6, 6, 6, 6, 3, 3, 3))


# Extend this to calculate life tables for every year 
# using the `group_by` function: 
lt_all_years <- d |> 
  group_by(year) |> 
  mutate(
    n = lead(x, default = Inf) - x,
    Mx = D/N,
    ax = case_when(
      x==0 ~ 0.07 + 1.7*Mx,
      x==1 ~ 1.5,
      x==95 ~ 1/Mx,
      TRUE ~ 2.5
    ),
    qx = case_when(
      x==95 ~ 1,  # set to 1 for open interval
      TRUE ~ n * Mx / (1 + (n - ax)* Mx)
    ),
    px = 1 - qx,
    lx = lag(cumprod(px), default = 1),
    dx = lx - lead(lx, default = 0),
    Lx = case_when(
      x==95 ~ lx/Mx,
      TRUE ~ n * lead(lx, default = 0) + (ax* dx)
    ),
    Tx = rev(cumsum(rev(Lx))),
    ex = Tx / lx
  )

head(lt_all_years)

# Plot lx over time
# As mortality improves, the drop in lx after the first year of life 
# becomes less noticeable, and the curve becomes more 'rectangular'
quebec_lx_plot <- lt_all_years |> 
  ggplot(aes(x, lx, color = year, group = year)) + 
  geom_line()+
  ggtitle("Survivorship over time, Quebec")
quebec_lx_plot

# Plot life expectancy at birth over time
quebec_ex_plot <- lt_all_years |> 
  filter(x==0) |>  
  ggplot(aes(year, ex)) + 
  geom_line() + 
  ggtitle("Life expectancy at birth, Quebec")
quebec_ex_plot

# save plots
ggsave("./plots/ontario_lx_plot.png", lx_plot)
ggsave("./plots/ontario_lived_plot.png", Lx_plot)
ggsave("./plots/quebec_lx_plot.png", quebec_lx_plot)
ggsave("./plots/quebec_ex_plot.png", quebec_ex_plot)

# Cohort Data Analysis 
# pull in date from the lexis deaths which divide deaths by cohort
clt <- "https://www.prdh.umontreal.ca/BDLC/data/ont/Deaths_lexis.txt" |>
  read_table(skip = 1, col_types = cols()) |>
  # filter to a single cohort
  filter(Cohort == 1921) |>
  # redo ages so the format matches the previous life tables,
  # 0, 1-4, 5-9, 10-14, ..., 95-99, 100-14 
  mutate(Age = as.numeric(Age)) |>
  mutate(age = cut(
    Age, breaks = c(0, 1, seq(5, 100, by = 5), Inf),
    include.lowest = TRUE, right = FALSE,
    labels = c("0", "1-4", str_c(seq(5, 100, by = 5), "-", seq(9, 104, by = 5)))
    )) |>
  # group by ages
  group_by(age) |>
  # calculate total deaths
  summarize(dx = sum(Male), .groups = "drop") |>
  # calculate lx as a sum of the total deaths observed
  # this ignores changes in population due to migration
  mutate(lx = rev(cumsum(rev(dx))))

clt |>
  # qx is simply dx over lx
  mutate(qx = dx/lx) |>
  # Need to assume ax so we assume similar to previous life table
  mutate(
    ax = case_when(
      age=="0" ~ 0.07,
      age=="1-4" ~ 1.5,
      age=="100-104" ~ 2.4,
      TRUE ~ 2.5
    )) |>
  # with ax assumed we can calculate Lx
  mutate(Lx = (lx-dx) + ax * dx) |>
  # and the rest of the life table is pretty straightforward
  mutate(Mx = dx/Lx) |>
  mutate(px = 1-qx) |>
  mutate(Tx = rev(cumsum(rev(Lx)))) |>
  mutate(ex = Tx / lx) %>% View()


