#' Chrom Plot
#'
#'
#'
#'



chrom_plot <- function(rt,int, peak_info = NULL)
  {

  peak_frame <- data.frame(rt, int)

  chrom_plot <- ggplot(data = peak_frame, aes(x = rt, y = int)) + geom_line(size = 0.45) +
                theme_classic() + xlab("Rt (mins)") + ylab("Intensity") +
                theme(axis.text.y = element_text(size = 10, face = "bold"),
                      axis.text.x = element_text(size = 10, face = "bold"),
                      axis.title.y = element_text(size = 10, face = "bold"),
                      axis.title.x = element_text(size = 10, face = "bold"))

  if(is.null(peak_info)){print(chrom_plot)}

  if(!is.null(peak_info)){
  plot(chrom_plot + geom_point(data = peak_info, aes(x = rt, y = int), colour = "red"))
  }

}
