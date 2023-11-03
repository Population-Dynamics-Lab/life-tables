# Life tables in R using the tidyverse
# Monica Alexander

rm(list=ls())
library(tidyverse) # data manipulation and ggplot functions
library(kableExtra) # format tables
library(janitor) # to easily clean up column names


# Ontario 2015 female life table ------------------------------------------

# load Ontario 2015 female survivorship data, define 5-year age groups
lt <- read_table("http://www.prdh.umontreal.ca/BDLC/data/ont/fltper_5x5.txt", skip = 2)
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
  xlab("age") + 
  theme_bw(base_size = 14) + 
  ggtitle("Survivorship for Ontario, 2015")
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

# visualizing Lx graphically
Lx_plot <- lt |> 
  mutate(lx = lx/100000) |> 
  ggplot(aes(x, lx)) +
  geom_line() + 
  xlab("age") + 
  theme_bw(base_size = 14) + 
  geom_vline(xintercept = 15, lty = 2, color = "red")+
  geom_vline(xintercept = 30, lty = 2, color = "red")+
  ggtitle("Survivorship for Ontario, 2015")
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
Mx <- read_table(
  "http://www.prdh.umontreal.ca/BDLC/data/que/Mx_5x5.txt", 
  skip = 2, col_types = 'ccddd')

d <- Mx |> 
  mutate(year = as.numeric(substr(Year, 1, 4))) |> 
  select(year, Age, Total) |> 
  clean_names() |> 
  rename(Mx = total)
head(d)

# `age` is a character 
# let's make an age `x` and interval length `n` column:
d <- d |> 
  mutate(x = as.numeric(str_remove(age, "-.*|\\+")),
         n = lead(x, default = Inf) - x) |> 
  filter(x<105) |> # remove older ages that have varying data availability
  select(year, age, x, n, Mx)
head(d)

# Use `tidyverse` to calculate the columns in the life table, 
# based on the equations presented in previous sections 
# Set radix l0 = 1 and filter to just include the year 2015 
lt_2015 <- d |> 
  filter(year==2015) |> 
  mutate(
    ax = case_when(
      x==0 ~ 0.07 + 1.7*Mx,
      x==1 ~ 1.5,
      x==110 ~ 1/Mx,
      TRUE ~ 2.5
    ),
    qx = n * Mx / (1 + (n - ax)* Mx),
    px = 1 - qx,
    lx = lag(cumprod(px), default = 1),
    dx = lx - lead(lx, default = 0),
    Lx = n * lead(lx, default = 0) + (ax* dx),
    Tx = rev(cumsum(rev(Lx))),
    ex = Tx / lx
  ) 

head(lt_2015)


# Extend this to calculate life tables for every year 
# using the `group_by` function: 
lt_all_years <- d |> 
  group_by(year) |> 
  mutate(
    ax = case_when(
      x==0 ~ 0.07 + 1.7*Mx,
      x==1 ~ 1.5,
      x==110 ~ 1/Mx,
      TRUE ~ 2.5
    ),
    qx = n * Mx / (1 + (n - ax)* Mx),
    px = 1 - qx,
    lx = lag(cumprod(px), default = 1),
    dx = lx - lead(lx, default = 0),
    Lx = n * lead(lx, default = 0) + (ax* dx),
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
