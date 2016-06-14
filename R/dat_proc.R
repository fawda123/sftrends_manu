######
# some additional data processing for the figure

######
# a smoother chlorophyll model for d7 

data(diat_dat)
data(delt_dat)

# resvar label lookup
lablk <- list(
  shrt = c('chl', 'sio2'),
  lngs = c(
    expression(paste('ln-chlorophyll a (ug ', L^-1, ')')),
    expression(paste('ln-silicon dioxide (mg ', L^-1, ')'))
    )
  )

# flovar label lookup
stalk <- list(
  shrt = c('sal'),
  lngs = c('Salinity')
  )

i <- 4

# data, response variable label
dat <- diat_dat[i, ]$data[[1]]
resvar <- diat_dat[i, ]$resvar
flovar <- diat_dat[i, ]$flovar
sta <- diat_dat[i, ]$Site_Code
reslab <- with(lablk, lngs[shrt == resvar])
flolab <- with(stalk, shrt[lngs == flovar])

# prep data as tidal object
tomod <- select(dat, Date, resval, floval, lim) %>% 
  rename(
    res = resval, 
    flo = floval
  ) %>% 
  data.frame %>% 
  tidal(., 
    reslab = reslab, 
    flolab = expression(paste('ln-flow (standardized)'))
  )

# salinity data at D7 to pred
topred <- filter(delt_dat, Site_Code == 'D7') %>% 
    mutate(flo = log(1 + sal)) %>% # salinity is only variable with zeroes
    rename(date = Date) %>% 
    select(date, flo) %>% 
    filter(date >= min(tomod$date) & date <= max(tomod$date)) %>% 
    na.omit %>% 
    data.frame

# create model and exit
mod <- wrtds(tomod, tau = c(0.1, 0.5, 0.9), wins = list(0.5, 20, 0.5), flo_div = 30, min_obs = 150)

# get predictions, norms from obs flow data
out <- mod %>% 
  respred(dat_pred = topred) %>% 
  resnorm

chld7 <- out
save(chld7, file = 'data/chld7.RData', compress = 'xz')
