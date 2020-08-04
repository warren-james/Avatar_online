# get shared legend ggplot 
g_legend<-function(a.gplot){
  # this gets the legend from a plot and allows 
  # you to put it in a grob 
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}