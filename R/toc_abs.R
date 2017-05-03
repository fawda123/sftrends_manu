data(h2dat)

nomod <-  filter(h2dat, resvar == 'no23') %>% 
  .$data %>% 
  .[[1]]

# globals
ylab1 <- expression(paste(NO [2] ^ '-', ' / ', NO [3] ^ '2-', ' (mg ', L^-1, ')'))
flolab <- expression(paste('ln-Flow (', m^3, ' ', s^-1, ')'))

# default theme
mytheme <- theme_minimal() + 
  theme(
    plot.background = element_rect(fill='transparent', 
      colour = NA),
    panel.background = element_rect(fill='transparent', 
      colour = NA),
    legend.position = 'none',
    legend.background = element_rect(fill='transparent', 
      colour = NA),
    legend.key = element_rect(fill = 'transparent', 
      colour = NA),
    axis.text.x = element_text(size = 16), 
    axis.title.x = element_blank(),
    axis.text.y = element_text(size = 16),
    axis.title.y = element_text(size = 18), 
    axis.line.x = element_line(), 
    axis.line.y = element_line(), 
    axis.ticks.x = element_line(),
    axis.ticks.y = element_line(),
    axis.ticks.length = unit(.1, "cm"), 
    panel.grid.major = element_line(colour = "#7BCCC4"),
    panel.grid.minor = element_blank(), 
    plot.margin = unit(c(3, 3, 3, 3), 'pt')
  )   
col_vec <- RColorBrewer::brewer.pal(9, 'Blues')[c(9, 8, 7)]
xlims <- range(nomod$date)

# observed time series
toplo1 <- select(nomod, date, res) %>% 
  na.omit()
p1 <- ggplot(toplo1, aes(x = date, y = exp(res))) + 
  geom_line() + 
  mytheme + 
  scale_y_continuous(ylab1, limits = c(0, 4)) +
  scale_x_date(limits = xlims) 

# prdnrmplots
p3 <- prdnrmplot(nomod, logspace = F, min_mo = 10) + 
  mytheme + 
  scale_y_continuous(ylab1) +
  scale_x_date(limits = xlims) 

# align widths of plots in first column, first two
pA <- ggplot_gtable(ggplot_build(p1))
pC <- ggplot_gtable(ggplot_build(p3))

maxWidth = grid::unit.pmax(pA$widths[2:3], pC$widths[2:3])

# Set the widths
pA$widths[2:3] <- maxWidth
pC$widths[2:3] <- maxWidth

tiff('ignore/toc1.tif', height = 2.5, width = 4.3, units = 'in', compression = 'lzw', res = 2400, family = 'serif')
grid.arrange(pA)
dev.off()

tiff('ignore/toc2.tif', height = 2.5, width = 4.3, units = 'in', compression = 'lzw', res = 2400, family = 'serif')
grid.arrange(pC)
dev.off()