##function to reorder and relabel categorical variables
func_categorial <- function (x,level_reorder,level_labels){
        data[,x]<<-factor(data[,x],levels=level_reorder,ordered=FALSE,
                          labels=level_labels)
}

##function to reorder and relabel categorical variables and make sure treated as ordinal --> ordered=TRUE
func_ordinal <- function (x,level_reorder,level_labels){
        data[,x]<<-factor(data[,x],levels=level_reorder,ordered=TRUE,
                          labels=level_labels)
}

##function to make sure variable treated as integer as well as constrain it to lower and upper bounds to be defined
func_integer <- function (x,upper_bound,lower_bound){
        data[,x]<-as.integer(as.character(data[,x]))
        data <<- data[which(data[,x]>lower_bound & data[,x]<upper_bound),]
}


######################################################################################
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
        require(grid)
        
        # Make a list from the ... arguments and plotlist
        plots <- c(list(...), plotlist)
        
        numPlots = length(plots)
        
        # If layout is NULL, then use 'cols' to determine layout
        if (is.null(layout)) {
                # Make the panel
                # ncol: Number of columns of plots
                # nrow: Number of rows needed, calculated from # of cols
                layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                                 ncol = cols, nrow = ceiling(numPlots/cols))
        }
        
        if (numPlots==1) {
                print(plots[[1]])
                
        } else {
                # Set up the page
                grid.newpage()
                pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
                
                # Make each plot, in the correct location
                for (i in 1:numPlots) {
                        # Get the i,j matrix positions of the regions that contain this subplot
                        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
                        
                        print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                                        layout.pos.col = matchidx$col))
                }
        }
}
