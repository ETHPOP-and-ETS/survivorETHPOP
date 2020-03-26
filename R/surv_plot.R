
#' surv_plot
#'
#' @param dat
#' @param add
#' @param S_name S or S_qx
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
surv_plot <- function(dat,
                      S_name = "S",
                      add = FALSE, ...) {

  plotfn <-
    if (add) lines else plot

  par(mgp = c(2,0.6,0))
  plotfn(
    x = 100*dat[[S_name]],
    type = 'l',
    # ylim = c(0.00, 100),
    xlab = "age & year",
    ylab = "% survived",
    lwd = 1.8,
    # yaxt = "n",
    main = paste(min(dat$year),
                 dat$ETH.group[1],
                 dat$sex[1],
                 collapse = ", "))#, ...)
  # axis(side = 2,
  #      at = c(0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1, 5, 10, 50),
  #      labels = c(0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1, 5, 10, 50),
  #      las = 2)
  axis(side = 1,
       seq(0, length.out = max(dat$year)/10,  by = 10),
       line = 3,
       mgp = c(2,0.5,0),
       labels = seq(min(dat$year),
                    length.out = max(dat$year)/10, by = 10))

  invisible(dat)
}
