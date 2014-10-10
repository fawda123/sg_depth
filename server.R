# packages to use
library(maptools)
library(reshape2) 
library(plyr)
library(ggplot2)
library(scales)
library(RColorBrewer)
library(rgdal)
library(rgeos)
library(gridExtra)
library(sp)

# functions to use
source('funcs.r')

##
# load shapefile data
to_load <- list.files('seagrass_gis', '\\.shp$')
shps <- vector('list', length = length(to_load))
names(shps) <- to_load
for(i in to_load) 
  shps[[i]] <- readShapeSpatial(paste0('seagrass_gis/', i))

# set ggplot theme
theme_set(theme_bw())

# Define server logic required to generate and plot data
shinyServer(function(input, output) {
  
  # dynamic controls
  # pick test pt once pts are selected
  output$reserveControls <- renderUI({

    seg_shp <- shps[[paste0('seg_', input$segment, '.shp')]]
    grid_spc <- input$grid_spc
    grid_seed <- input$grid_seed
    set.seed(grid_seed)
    pts <- grid_est(seg_shp, spacing = grid_spc) 
    
    selectInput(inputId = 'test_point',
                label = h3('Test point'),
                choices = 1:length(pts)
      )
    
    })
  
  output$simplot <- renderPlot({
    
    # plotting code

#     # for debugging
#     segment <- '820'
#     grid_spc <- 0.02
#     grid_seed <- 1234
#     test_point <- 1
#     radius <- 0.04
#  		thresh <- 0.1   	
#     show_all <- F
    
    # input from ui
    segment <- input$segment
    grid_spc <- input$grid_spc
    grid_seed <- input$grid_seed
    test_point <- input$test_point
    radius <- input$radius
  	thresh <- input$thresh
    show_all <- input$show_all
    
    # get data from loaded shapefiles and input segment
    seg_shp <- shps[[paste0('seg_', segment, '.shp')]]
    sgpts_shp <- shps[[grep(paste0('^sgpts.*', segment, '.shp$'), names(shps))]]
    
    # random points  
    set.seed(grid_seed)
    pts <- grid_est(seg_shp, spacing = grid_spc) 
    
    if(show_all){
      
    	# get estimates for each point
      maxd <- list()
      for(i in 1:length(pts)){
        
        eval_pt <- pts[i, ]
        ests <- try({
          buff_pts <- buff_ext(sgpts_shp, eval_pt, buff = radius)
      	  est_pts <- data.frame(buff_pts)
          doc_single <- doc_est(est_pts, thresh = thresh )$ests
          doc_single
        })
      	if('try-error' %in% class(ests)) ests <- NA
        maxd[[i]] <- ests
        
      }
      
			# combine in data frame for plotting
			maxd <- data.frame(pts, zmax_all = do.call('c', maxd))
    	
    	# get values for combined legend
			rngs <- range(maxd$zmax_all, na.rm = T)
			brks <- seq(rngs[1], rngs[2], length = 5)
			labs <- format(round(brks, 1), nsmall = 1, digits =1)
			
    	# unestimable points to plot
			unest <- maxd[is.na(maxd[, 'zmax_all']), ]
			
    	##
    	# plot
    	
			p1 <- ggplot(seg_shp, aes(long, lat)) + 
			  geom_polygon(fill = 'white') +
			  geom_path(color = 'black') +
			  theme_classic() +
			  coord_equal() +
				ylab('Latitude') + 
				xlab('Longitude') +
			  geom_point(
			    data = maxd, 
			    aes(Var1, Var2, size = zmax_all, colour = zmax_all)
			  ) +
# 				geom_point(data = unest,
# 					aes(Var1, Var2), 
# 					size = 3, colour = 'grey',
# 					pch = 1
# 					) +
			  ggtitle('Depth of col (m)') +
    		theme(legend.position = c(0,0), legend.justification = c(0, 0)) + 
				scale_size_continuous(name = "Depth estimate", 
					breaks = brks, 
					labels = labs,
					range = c(1, 12)) + 
				scale_colour_gradient(name = "Depth estimate", 
					breaks = brks, 
					labels = labs) + 
			 	guides(colour = guide_legend())
			    	
			print(p1)
			      
    } else {
    
      # point from random points for buffer
      test_pt <- pts[test_point, ]
  
      # get bathym points around test_pt
      buff_pts <- buff_ext(sgpts_shp, test_pt, buff = radius)
      
      p1 <- ggplot(seg_shp, aes(long, lat)) + 
        geom_polygon(fill = 'white') +
        geom_path(color = 'black') +
        theme_classic() +
        coord_equal() +
    		xlab('Longitude') +
    		ylab('Latitude') +
        geom_point(
          data = data.frame(pts), 
          aes(Var1, Var2), 
          size = 3,
          pch = 1
        ) +
        geom_point(
          data = data.frame(buff_pts),
          aes(coords.x1, coords.x2), 
          colour = 'red', 
          size = 0.3, 
          alpha = 0.7
        ) +
    		geom_point(
    			data = data.frame(test_pt), 
    			aes(Var1, Var2), 
    			size = 3, 
    			pch = 1
    		)
      
    	##
     	# get data used to estimate depth of col
			est_pts <- data.frame(buff_pts)
			
			# data
			dat <- doc_est(est_pts, thresh = thresh)
			
			# actual ests
			act_ests <- dat$ests
			
			# format estimate for plot title
			if(any(is.na(act_ests))){ act_ests <- 'Depth of col: Not estimable'
			} else { 
				act_ests <- paste('Depth of col:', round(act_ests, 1), 'm')
				}
			
			##
			# simple plot of points by depth, all pts and those with seagrass
			to_plo <- dat$data
			to_plo <- melt(to_plo, id.var = 'Depth', 
				measure.var = c('dep_cum', 'sg_cum'))
			to_plo$variable <- factor(to_plo$variable, levels = c('dep_cum', 'sg_cum'), 
			                            labels = c('All', 'Seagrass'))
			
			to_plo2 <- dat$preds
			to_plo2 <- melt(to_plo2, id.var = 'Depth', 
				measure.var = grep('dep_cum|sg_cum', names(to_plo2), value = T)
				)
			to_plo2$variable <- factor(to_plo2$variable, 
				levels = c('dep_cum', 'sg_cum'),
				labels = c('All', 'Seagrass')
			)
			
			cols  <- c('lightgreen', 'lightblue')
			linesz <- 1
			
			p2 <- ggplot(to_plo2, aes(x = Depth, y = value, group = variable,
			                         colour = variable)) +
			  geom_line(size = linesz) +
				geom_point(data = to_plo, aes(x = Depth, y = value, group = variable, 
					colour = variable)) +
			 	ylab('Cumulative points') +
			  xlab('Depth (m)') +
			  scale_colour_manual('Point category', values = rev(cols)) +
			  theme(legend.position = c(0, 1), legend.justification = c(0,1))
			
			##
			# plot slope of cumulative point curves
			
			# treshold label for legend
			thresh_lab <- paste0(round(100 * thresh), '% of all')
			
			to_plo <- dat$preds
			to_plo <- melt(to_plo, id.var = 'Depth', 
				measure.var = grep('dep_slo|sg_slo|Threshold', names(to_plo), value = T)
				)
			to_plo$variable <- factor(to_plo$variable)
			to_plo$variable <- factor(to_plo$variable, 
				levels = c('dep_slo', 'sg_slo', grep('Thresh', levels(to_plo$variable), value = T)),
				labels = c('All', 'Seagrass', thresh_lab)
			)
			
			p3 <- ggplot(to_plo, aes(x = Depth, y = value,
					colour = variable, linetype = variable)) +
				geom_line(size = linesz) +
			 	ylab('CDF Slope') +
			  xlab('Depth (m)') +
				scale_linetype_manual('Slope category', 
					values = c('solid', 'solid', 'dashed')
					) + 
				scale_colour_manual('Slope category', 
			  	values = c(cols[2], cols[1], cols[2])
			  	) +
			  theme(legend.position = c(1, 1), legend.justification = c(1, 1))
			
			##
    	# combine all plots

			grid.arrange(p1,
				arrangeGrob(p2, p3, ncol = 2), 
				ncol = 1, heights = c(1.5, 1.5),
				main = act_ests)
      
    }
    
    },height = 600, width = 700)

    })