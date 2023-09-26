GGsave = function(filename, 
		  plot=last_plot(), 
		  width=dev.size()[1],
		  height=dev.size()[2]) {

    device = 'png'
    if (str_detect(filename, "pdf")) device = 'pdf'
    ggsave(filename=filename,
	   plot=plot,
	   width=width,
	   height=height,
	   device=device)
}
