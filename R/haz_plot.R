
#' haz_plot
#'
#' @param plot_dat
#' @param add
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
haz_plot <- function(plot_dat,
                     add = FALSE, ...) {

  plotfn <-
    if (add) lines
  else { plot }

  par(mgp = c(2,0.6,0))
  plotfn(
    x = 100*plot_dat$prop,
    type = 'l',
    # ylim = c(0.1, 5),
    ylim = c(0.001, 50),
    xlab = "age & year",
    ylab = "% dying each year",
    log = "y",
    lwd = 1.8,
    yaxt = "n",
    main = paste(min(plot_dat$year),
                 plot_dat$ETH.group[1],
                 plot_dat$sex[1],
                 collapse = ", "), ...)
  axis(side = 2,
       at = c(0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1, 5, 10, 50),
       labels = c(0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1, 5, 10, 50),
       las = 2)
  axis(side = 1,
       seq(0, length.out = max(plot_dat$year)/10,  by = 10),
       line = 3,
       mgp = c(2,0.5,0),
       labels = seq(min(plot_dat$year),
                    length.out = max(plot_dat$year)/10, by = 10))

  invisible(plot_dat)
}
