
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(WRTDStidal)
library(gridExtra)
data("mods_nolag")

# model subset and plot subsets
row <- 7
mod <- mods_nolag$mod[[row]]
ylab <-  expression(paste("DIN (mg ", L^-1, ")"))
ylims <- as.Date(c('1976-01-01', '2013-12-31'))
xlims <- c(0, 6)

annuals <- prdnrmplot(tmp, annuals = TRUE, plot = F) %>%
  .$nrms %>% 
  filter(taus == 0.5)
seasnls <- prdnrmplot(tmp, annuals = FALSE, plot = F) %>%
  .$nrms %>% 
  filter(taus == 0.5)
residus <- select(mod, date, res, fit0.5) %>% 
  mutate(resfit = exp(res) - exp(fit0.5)) %>% 
  na.omit()

# default themes
pthms <- theme_bw() + 
  theme(
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  )

# plots
p1 <- ggplot(na.omit(mod), aes(x = date, y = exp(res))) + 
  geom_line() + 
  theme_bw() + 
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  ) +
  scale_y_continuous('DIN', limits = c(0, 4)) + 
  scale_x_date('Time', limits = ylims)

p2 <- ggplot(annuals, aes(x = date, y = exp(nrms_value))) + 
  geom_line() + 
  pthms +
  scale_y_continuous(limits = c(0, 4)) + 
  scale_x_date('Time', limits = ylims)

p3 <- ggplot(seasnls, aes(x = date, y = exp(nrms_value))) + 
  geom_line() + 
  pthms +
  scale_x_date('Time', limits = ylims)

p4 <- ggplot(residus, aes(x = date, y = resfit)) + 
  geom_point() + 
  geom_hline(aes(yintercept = 0)) + 
  pthms +
  scale_x_date('Time', limits = ylims)

p5 <- dynaplot(mod, mo = 1, logspace = F, years = c(1976, 2000, 2013), col_vec = 'black') + 
  pthms +
  theme(legend.position = 'none', strip.text.x = element_blank()) + 
  scale_x_continuous('Flow')

ht <- 1.75
wd <- 4
tiff('figs/schematic1.tif', height = ht, width = wd, units = 'in', compression = 'lzw', res = 400, family = 'serif')
p1
dev.off()
tiff('figs/schematic2.tif', height = ht, width = wd, units = 'in', compression = 'lzw', res = 400, family = 'serif')
p2
dev.off()
tiff('figs/schematic3.tif', height = ht, width = wd, units = 'in', compression = 'lzw', res = 400, family = 'serif')
p3
dev.off()
tiff('figs/schematic4.tif', height = ht, width = wd, units = 'in', compression = 'lzw', res = 400, family = 'serif')
p4
dev.off()
tiff('figs/schematic5.tif', height = ht, width = wd, units = 'in', compression = 'lzw', res = 400, family = 'serif')
p5
dev.off()

