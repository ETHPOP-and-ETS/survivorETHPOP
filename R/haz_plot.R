
#' haz_plot
#'
#' @param dat
#' @param add
#' @param hz_name
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
haz_plot <- function(dat,
                     hz_name = "death_rate",
                     add = FALSE, ...) {

  plotfn <-
    if (add) lines
  else { plot }

  par(mgp = c(2,0.6,0))
  plotfn(
    x = 100*dat[[hz_name]],
    type = 'l',
    # ylim = c(0.1, 5),
    ylim = c(0.001, 50),
    xlab = "age & year",
    ylab = "% dying each year",
    log = "y",
    lwd = 1.8,
    yaxt = "n",
    main = paste(min(dat$year),
                 dat$ETH.group[1],
                 dat$sex[1],
                 collapse = ", "), ...)
  axis(side = 2,
       at = c(0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1, 5, 10, 50),
       labels = c(0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1, 5, 10, 50),
       las = 2)
  axis(side = 1,
       seq(0, length.out = max(dat$year)/10,  by = 10),
       line = 3,
       mgp = c(2,0.5,0),
       labels = seq(min(dat$year),
                    length.out = max(dat$year)/10, by = 10))

  invisible(dat)
}
