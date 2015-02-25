# packages to use
library(maptools)
library(reshape2) 
library(plyr)
library(ggplot2)
library(scales)
library(RColorBrewer)
library(rgdal)
library(gstat)
library(gridExtra)
library(sp)
library(automap)

# functions to use
source('R/funcs.r')

##
# load shapefile data
data(shps)

# set ggplot theme
theme_set(theme_bw())

# Define server logic required to generate and plot data
shinyServer(function(input, output) {
  
  # dynamic controls
  # pick test pt once pts are selected
  output$reserveControls <- renderUI({

    segment <- input$segment
    seg_shp <- shps[[paste0('seg_', gsub('^.*_', '', segment), '.shp')]]
    grid_spc <- input$grid_spc
    grid_seed <- input$grid_seed
    set.seed(grid_seed)
    pts <- grid_est(seg_shp, spacing = grid_spc) 
    
    selectInput(inputId = 'test_point',
                label = h4('Test point'),
                choices = 1:length(pts)
      )
    
    })
  
  output$simplot <- renderPlot({
    
    # plotting code
    
    # input from ui
    segment <- input$segment
    grid_spc <- input$grid_spc
    grid_seed <- input$grid_seed
    test_point <- input$test_point
    point_lab <- input$point_lab
    radius <- input$radius
    show_all <- input$show_all
    show_krige <- input$show_krige
    
    # get data from loaded shapefiles and input segment
    seg_shp <- shps[[paste0('seg_', gsub('^.*_', '', segment), '.shp')]]
    sgpts_shp <- shps[[grep(paste0('^sgpts.*', segment, '.shp$'), names(shps))]]
    
    # random points  
    set.seed(grid_seed)
    pts <- grid_est(seg_shp, spacing = grid_spc)
  
    # all estimates
    if(show_all != 'nope'){
      
    	# get estimates for each point
      maxd <- list()
      for(i in 1:length(pts)){
        
        eval_pt <- pts[i, ]
        ests <- try({
          buff_pts <- buff_ext(sgpts_shp, eval_pt, buff = radius)
      	  est_pts <- data.frame(buff_pts)
          doc_single <- attr(doc_est(est_pts), show_all)
          doc_single
        })
      	if('try-error' %in% class(ests)) ests <- NA
        maxd[[i]] <- ests
        
      }
      
			# combine in data frame for plotting
			maxd <- data.frame(pts, zmax_all = do.call('c', maxd))
    	
      # remove extreme values
      maxd <- na.omit(maxd)
      maxd <- maxd[maxd$zmax_all < quantile(maxd$zmax_all, 0.99), ]
      
      # kriging of all estimates
      if(show_krige){
          
        ##
        # krige the prediction grid
        
        kriged <- sg_krige(maxd, seg_shp)
        
        p1 <- ggplot(seg_shp, aes(long, lat)) + 
    			geom_tile(
        	  data = kriged, 
    			  aes(Var1, Var2, fill = maxd)
          ) +
          geom_polygon(fill = NA) +
  			  geom_path(color = 'black') +
  			  theme_classic() +
  			  coord_equal() +
  				ylab('Latitude') + 
  				xlab('Longitude') +
          scale_fill_gradientn(
            name = 'Seagrass depth (m)', 
            colours = brewer.pal(7, 'Spectral')
            ) + 
      		theme(legend.position = c(0,0), legend.justification = c(0, 0),
            text = element_text(size=20)
            )
        
        print(p1)
       
      # no kriging
      } else {
        
      	# get values for combined legend
  			rngs <- range(maxd$zmax_all, na.rm = T)
  			brks <- seq(rngs[1], rngs[2], length = 4)
  			labs <- format(brks, nsmall = 1, digits =1)
  			
      	##
      	# plot
      	
  			p1 <- ggplot(seg_shp, aes(long, lat)) + 
  			  geom_polygon(fill = 'white') +
  			  geom_path(color = 'black') +
  			  theme_classic() +
  			  coord_equal() +
  				ylab('Latitude') + 
  				xlab('Longitude') +
  			  ggtitle('Depth (m)') +
      		theme(legend.position = c(0,0), legend.justification = c(0, 0),
            text = element_text(size=20)
            ) + 
  				scale_size_continuous(name = "Depth estimate", 
  					breaks = brks, 
  					labels = labs,
  					range = c(4, 18)) + 
  				scale_colour_gradientn(name = "Depth estimate", 
  					breaks = brks, 
  					labels = labs,
            colours = c('purple', 'blue', 'lightblue')
            ) + 
  			 	guides(colour = guide_legend())
  			   
        # plot points with point labels if true
        if(point_lab) {
          maxd <- data.frame(maxd, labs = row.names(maxd))
          p1 <- p1 + geom_text(
  			    data = maxd, 
  			    aes(Var1, Var2, label = labs, 
              size = zmax_all, colour = zmax_all)
          )
        } else { 
          p1 <- p1 + geom_point(
  			    data = maxd, 
  			    aes(Var1, Var2, size = zmax_all, colour = zmax_all)
  			  )
        }
        
  			print(p1)
        
      }
			  
    # individual estimates
    } else {
      
      # point from random points for buffer
      test_pt <- pts[test_point, ]
  
      # get bathym points around test_pt
      # added try exception for reservecontrols
      buff_pts <- try({
        buff_ext(sgpts_shp, test_pt, buff = radius)
      }, silent = T)
      if('try-error' %in% class(buff_pts)) return()
      
      p1 <- ggplot(seg_shp, aes(long, lat)) + 
        geom_polygon(fill = 'white') +
        geom_path(color = 'black') +
        theme_classic() +
        coord_equal() +
    		xlab('Longitude') +
    		ylab('Latitude') +
        geom_point(
          data = data.frame(buff_pts),
          aes(coords.x1, coords.x2), 
          colour = 'red', 
          size = 0.3, 
          alpha = 0.7
        ) + 
        theme(text = element_text(size=20))
      
      # plot points wiht point labels if true
      if(point_lab) {
        pts <- data.frame(pts, labs = row.names(pts))
        p1 <- p1 + geom_text(
          data = pts,
          aes(Var1, Var2, label = labs),
          size = 3
        )
      } else { 
        p1 <- p1 + geom_point(
          data = data.frame(pts), 
          aes(Var1, Var2), 
          size = 3,
          pch = 1
        )}
      
    	##
     	# get data used to estimate depth of col for plotting
			est_pts <- data.frame(buff_pts)
      
			# data
			dat <- doc_est(est_pts)   
      
      # logistic curve plot
      p2 <- plot(dat)
      
      # arrange as grobs
			grid.arrange(p1,
				arrangeGrob(p2, ncol = 1), 
				ncol = 1, heights = c(1.5, 1.5)
			)
      
    }
    
    },height = 900, width = 900)

    })