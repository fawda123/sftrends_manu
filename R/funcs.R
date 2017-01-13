######
# get legend from an existing ggplot object
g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

######
# some stupid function for formatting p-values, no asterisks
# one value at a time
p_form <- function(x, tex_out = F){
  if(x < 0.005) out <-'< 0.005'
  else{
    if(x < 0.05 & x >= 0.005) out <- as.character(format(round(x, 3), nsmall = 3))
    else out <- 'ns'
    }
  if(tex_out == F) return(out)
  else{
    if(out == '< 0.005') '$p<$ 0.005'
    else{
      if(out == 'ns') '$p = $ ns'
      else paste('$p = $', out, sep = ' ')
      }
    }
  }

######
# another stupid function for formatting p-values as asterisks
p_ast <- function(x){
  
  sig_cats <- c('**', '*', '')
  sig_vals <- c(-Inf, 0.005, 0.05, Inf)
  
  out <- cut(x, breaks = sig_vals, labels = sig_cats, right = FALSE)
  out <- as.character(out)
  
  return(out)
  
}

######
# scientific notation from R to LaTeX
# x is numeric to convert
# pow is minimum exponent (positive or negative) to use for notation
# digits numeric for rounding
# showdollar logical if output is enclosed in dollar for latex
# from https://dankelley.github.io/r/2015/03/22/scinot.html
scinot <- function(x, pow = 2, digits = 2, showDollar = TRUE)
{
  
  x <- as.numeric(x)
  
  # only do this is x is not equal to zero
  if(x != 0){
    
    sign <- ""
    if (x < 0) {
        sign <- "-"
        x <- -x
    }
    exponent <- floor(log10(x))
    if (exponent & abs(exponent) > pow) {
        xx <- round(x / 10^exponent, digits=digits)
        e <- paste("\\times 10^{", as.integer(exponent), "}", sep="")
    } else {
        xx <- round(x, digits=digits)
        e <- ""
    }
    
    out <- paste(sign, xx, e, sep="")
    
  } else {
    
    out <- x
    
  }
  
  # add dollars if true
  if (showDollar) out <- paste("$", out, "$", sep="")
  
  return(out)
  
}

# wrapper for as.character to english function
eng <- function(chr){
  
  out <- english(chr)
  out <- as.character(out)
  
  return(out)
  
}

######
# trend maps
# 
# library(dplyr)
# library(ggplot2)
# library(WRTDStidal)
# library(maptools)
# library(tidyr)
# library(purrr)
# library(RColorBrewer)
# library(scales)
# library(gridExtra)
# library(ggrepel)
# source('R/funcs.R')
#
# uses delt_dat, mods_nolag, delt_map
#
trnd_map <- function(res = c('din', 'nh', 'no23'), mods,
  mobrks = list(c(1, 2, 3, 4), c(5, 6, 7, 8), c(9, 10, 11, 12)),
  yrbrks = c(-Inf, 1988, 2000, Inf),
  molabs = c('JFMA', 'MJJA', 'SOND'),
  yrlabs = c('1976-1988', '1989-2000', '2001-2012'),
  buffx = 0.00025,
  buffy = 0.001,
  cols =  c('#D53E4F', '#4DAF4A'),
  sz_rng = c(2, 11),
  shps = c(24, 25),
  ndivs = 6,
  strp_fl = 'lightgrey',
  bar = FALSE, 
  stat_labs = TRUE,
  leg = TRUE, 
  leg_lab = 'Percent change'){

  # load required data
  data(delt_dat)
  data(delt_map)

  # response labels 
  reslabs <- list(
    shrt = c('din', 'nh', 'no23'),
    expr = c(
      'DIN', 
      expression(paste(NH [4] ^ '+')),
      expression(paste(NO [2] ^ '-', ' / ', NO [3] ^ '2-'))
    )
  )
  
  # make response labels
  mods <- mutate(mods, reslabs = factor(resvar, levels = reslabs$shrt, labels = reslabs$expr))
  
  ##
  # get a bounding box for the stations
  statmeta <- select(delt_dat, Site_Code, Latitude, Longitude) %>% 
    unique
  
  # bounding box
  lims<- select(statmeta, -Site_Code) %>% 
    apply(., 2, function(x) range(x, na.rm = TRUE))
  lims <- c(
    (1 + buffx) * lims[1, 2], 
    (1 - buffy) * lims[1, 1], 
    (1 - buffx) * lims[2, 2], 
    (1 + buffy) * lims[2, 1]
    )    
  names(lims) <- c('left', 'bottom', 'right', 'top')
  
  ##
  # get trends using wrtdstrnd function

  # need placeholders for null molabs
  if(is.null(molabs)){
    
    molabstmp <- letters[1:length(mobrks)]
    
    # get trends
    trnds <- mutate(mods, 
      trnd = map(data, function(x){
        wrtdstrnd(x, mobrks, yrbrks, molabstmp, yrlabs)
        })
      )
  
  }
  
  # need placeholders for null yrlabs
  if(is.null(yrlabs)){
    
    yrlabstmp <- letters[1:(length(yrbrks) - 1)]
    
    # get trends
    trnds <- mutate(mods, 
      trnd = map(data, function(x){
        wrtdstrnd(x, mobrks, yrbrks, molabs, yrlabstmp)
        })
      ) 

  }
  
  # unnest trend summary
  trnds <-select(trnds, -data) %>% 
      unnest %>% 
      left_join(., statmeta, by = 'Site_Code') %>% 
      data.frame
  
  ##
  # create some plots
  
  # convert delta map to ggplot compatible object
  delt_map <- fortify(delt_map)
  
  # base delta map
  pbase <- ggplot(delt_map, aes(x = long, y = lat)) + 
    geom_polygon(aes(group = group, fill = hole), colour = "cornflowerblue") +
    theme_bw() + 
    theme(
      plot.background = element_rect(fill='transparent', 
        colour = NA),
      panel.background = element_rect(fill='transparent', 
        colour = NA),
      legend.background = element_rect(fill='transparent', 
        colour = NA),
      legend.key = element_rect(fill = 'transparent', 
        colour = NA),
      axis.ticks.x = element_line(),
      axis.ticks.y = element_line(),
      axis.ticks.length = unit(.1, "cm"), 
      panel.grid.minor = element_blank(), 
      axis.title.x = element_blank(), 
      axis.title.y = element_blank(), 
      legend.position = 'none', 
      plot.margin = grid::unit(c(3, 3, 12, 3), 'pt'), 
      axis.text.x = element_text(size = 8), 
      axis.text.y = element_text(size = 8)
    ) +
    coord_fixed(ratio = 1, xlim = lims[c(1, 3)], ylim = lims[c(2, 4)])

  # some formatting for the variable to plot
  toplo <- filter(trnds, resvar %in% res & cat %in% c(molabs, yrlabs))
  toplo$shp <- shps[1]
  toplo[toplo$chg < 0, 'shp'] <- shps[2]
  toplo$shp <- factor(toplo$shp)
  toplo$sz <- scales::rescale(abs(toplo$chg), to = sz_rng)
  
  # block out site labels depending on molabs or ylabs as NULL
  if(is.null(molabs))
    toplo[!toplo$cat %in% yrlabs[1] | !toplo$resvar %in% res[1], 'Site_Code'] <- NA
  if(is.null(yrlabs))
    toplo[!toplo$cat %in% molabs[1] & toplo$resvar %in% res[1], 'Site_Code'] <- NA
  
  # barplots of true
  if(bar){
    
    # barplot data
    toplobr <- filter(trnds, resvar %in% res & cat %in% c(molabs, yrlabs))
    toplobr$cat_grp <- 'yr'
    toplobr$cat_grp[grepl('[A-Z]', toplobr$cat)] <- 'mo'
    toplobr$cat_grp <- factor(toplobr$cat_grp, levels  = c('yr', 'mo'))
    
    # barplot
    pbar <- ggplot(toplobr, aes(x = Site_Code, y = chg, fill = cat)) + 
      geom_bar(stat = 'identity', position = 'dodge') + 
      facet_grid(resvar ~ cat_grp, labeller = label_parsed) +
      theme(strip.background=element_rect(fill = strp_fl)) +
      theme_bw()
    
    return(pbar)
    
  }

  # add trend data to base map
  ptrnd <- pbase +
    geom_point(data = toplo, 
      aes(x = Longitude, y = Latitude, size = sz, fill = shp, shape = shp),
      alpha = 0.9
    )  + 
    scale_shape_manual(values = shps) + 
    scale_size(range = sz_rng) + 
    scale_fill_manual(values=c(cols, "cornflowerblue", "white"), guide="none") +
    facet_grid(reslabs ~ cat, labeller = label_parsed) +
    theme(strip.background=element_rect(fill = 'white', colour = 'white'),
      panel.border = element_rect(colour = "grey")
      )
  
  # add station labels in first panel if T
  if(stat_labs){
    
    ptrnd <- ptrnd +
      geom_label_repel(
        data = toplo, 
        aes(x = Longitude, y = Latitude, label = Site_Code), 
        label.r = unit(0, "lines"),
        box.padding = unit(1, "lines"), 
        point.padding = unit(0, "lines"), 
        force = 2, size = 2,
        fill = strp_fl
      )
    
  }
  
  # add legend if T
  if(leg){
    
    ## manual legend
    
    legtitle <- leg_lab
    legcols <- rep(cols, each = ndivs/2)
    legshps <- rep(shps, each = ndivs/2)
    
    # sizes 
    negs <- with(toplo, sign(chg) == -1)
    dec <- toplo$sz[negs]
    dec <- seq(min(dec), max(dec), length = ndivs/2)
    inc <- toplo$sz[!negs]
    inc <- seq(min(inc), max(inc), length = ndivs/2)
    legszs <- rev(c(rev(dec), inc))
    
    # labels
    dec <- toplo$chg[negs]
    dec <- seq(min(dec), max(dec), length = ndivs/2)
    inc <- toplo$chg[!negs]
    inc <- seq(min(inc), max(inc), length = ndivs/2)
    leglab <- round(c(dec, inc), 1)
    
    # fake legend data to make the plot
    fakedat<- data.frame(
      shp = leglab,
      sz = leglab,
      col = leglab
    )
    
    # legend plot to get legend
    pleg <- ggplot(fakedat, aes(x = shp, y = sz, fill = factor(col), size = factor(sz), shape = factor(shp))) +
      geom_point() +
      scale_fill_manual(legtitle, values = rev(legcols), labels = leglab) +
      scale_size_manual(legtitle, values = rev(legszs), labels = leglab) +
      scale_shape_manual(legtitle, values = rev(legshps), labels = leglab) +
      guides(
        fill = guide_legend(title = legtitle, nrow = 1), 
        size = guide_legend(title = legtitle), 
        shape = guide_legend(title = legtitle)
        ) +
      theme_minimal() + 
      theme(legend.position = 'top', 
      plot.margin = grid::unit(c(18, 3, 3, 3), 'pt')
        )
    pleg <- g_legend(pleg)
    
    # combine the legend and trends map
    grid.arrange(
      arrangeGrob(
        pleg, ptrnd, ncol = 1, 
        heights = c(0.1, 1)
        )
      )
   
  # no legend 
  } else {
    
    ptrnd
    
  }
  
}

# capitalize all words in a string
cap <- function(x) {
  x <- as.character(x)
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
      sep="", collapse=" ")
}

######
# gam equivalent of dynaplot function
# looks at changes in in the chlorophyll-flow relationships by month over different years
# grd is the number of salinity values to use in predictions
dynagam <- function(mod_in, dat_in, grd = 30, years = NULL, alpha = 1,
  size = 1, col_vec = NULL, allflo = FALSE, month = c(1:12), scales = NULL, ncol = NULL, 
  pretty = TRUE, grids = TRUE, use_bw = TRUE, fac_nms = NULL){

  # add year, month columns to dat_in
  dat_in <- mutate(dat_in, 
    month = as.numeric(strftime(date, '%m')), 
    year = as.numeric(strftime(date, '%Y'))
  )
  to_plo <- dat_in
  
  # flo values to predict
  flo_vals <- range(to_plo[, 'flo'], na.rm = TRUE)
  flo_vals <- seq(flo_vals[1], flo_vals[2], length = grd)
  
  # get model predictions across range of flow values
  dynadat <- rep(flo_vals, each = nrow(to_plo)) %>% 
    matrix(., nrow = nrow(to_plo), ncol = grd) %>% 
    cbind(to_plo[, c('dec_time', 'doy')], .) %>%
    gather('split', 'flo', -dec_time, -doy) %>% 
    select(-split) %>% 
    data.frame(., res = predict(mod_in, .)) %>% 
    spread(flo, res) %>% 
    select(-dec_time, -doy)
  
  # merge predictions with year, month data, make long format
  to_plo <- select(to_plo, year, month) %>% 
    cbind(., dynadat) %>% 
    gather('flo', 'res', -year, -month) %>% 
    mutate(flo = as.numeric(as.character(flo)))
  
  # subset years to plot
  if(!is.null(years)){
    
    to_plo <- to_plo[to_plo$year %in% years, ]
    to_plo <- to_plo[to_plo$month %in% month, ]
    
    if(nrow(to_plo) == 0) stop('No data to plot for the date range')
  
  }
    
  # constrain plots to salinity limits for the selected month
  if(!allflo){
    
    #min, max salinity values to plot
    lim_vals <- group_by(data.frame(dat_in), month) %>% 
      summarise(
        Low = quantile(flo, 0.05, na.rm = TRUE),
        High = quantile(flo, 0.95, na.rm = TRUE)
      )
  
    # month flo ranges for plot
    lim_vals <- lim_vals[lim_vals$month %in% month, ]

    # merge limts with months
    to_plo <- left_join(to_plo, lim_vals, by = 'month')
    to_plo <- to_plo[to_plo$month %in% month, ]
    
    # reduce data
    sel_vec <- with(to_plo, 
      flo >= Low &
      flo <= High
      )
    to_plo <- to_plo[sel_vec, !names(to_plo) %in% c('Low', 'High')]
    to_plo <- arrange(to_plo, year, month)
    
  }
  
  # reshape data frame, average by year, month for symmetry
  to_plo <- group_by(to_plo, year, month, flo) %>% 
    summarise(
      res = mean(res, na.rm = TRUE)
    )
  
  # months labels as text
  mo_lab <- data.frame(
    num = seq(1:12), 
    txt = c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')
  )
  mo_lab <- mo_lab[mo_lab$num %in% month, ]
  to_plo$month <- factor(to_plo$month, levels =  mo_lab$num, labels = mo_lab$txt)
  
  # reassign facet names if fac_nms is provided
  if(!is.null(fac_nms)){
    
    if(length(fac_nms) != length(unique(to_plo$month))) stop('fac_nms must have same lengths as months')
  
    to_plo$month <- factor(to_plo$month, labels = fac_nms)
    
  }
  
  
  # make plot
  p <- ggplot(to_plo, aes(x = flo, y = res, group = year)) + 
    facet_wrap(~month, ncol = ncol, scales = scales)
  
  # return bare bones if FALSE
  if(!pretty) return(p + geom_line())
  
  # colors, uses gradcols from WRTDStidal
  cols <- gradcols(col_vec = col_vec)
  
  # use bw theme
  if(use_bw) p <- p + theme_bw()
  
  p <- p + 
    geom_line(size = size, aes(colour = year), alpha = alpha) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0)) +
    theme(
      legend.position = 'top'
    ) +
    scale_colour_gradientn('Year', colours = cols) +
    guides(colour = guide_colourbar(barwidth = 10)) 
  
  # remove grid lines
  if(!grids) 
    p <- p + 
      theme(      
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
  
  return(p)
  
}