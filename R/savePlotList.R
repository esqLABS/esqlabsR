#' @name savePlotList
#' @title Save a list of plots
#'
#' @param plotlist A list of plots (ideally form `sensivitityTimeProfiles()` or
#'   `sensitivitySpiderPlot()`).
#' @param plot.type A string specifying the prefix for plot filename.
#' @param height,width Dimensions for saved plot.
#'
#' @seealso sensivitityTimeProfiles, sensitivitySpiderPlot
#'
#' @examples
#'
#' # first check out examples for `sensivitityTimeProfiles()` and
#' `sensitivitySpiderPlot()`
#'
#' # if you wanted to save those lists of plots, you can do using the current
#' function like following:
#'
#' # savePlotList(ls_profile_plots, plot.type = "Profile_", height = 6, width = 12)
#' # savePlotList(ls_spider_plots, plot.type = "Spider_", height = 6, width = 12)
#'
#' @export

savePlotList <- function(plotlist, plot.type, dpi = 300, height, width) {
  purrr::walk2(
    .x = plotlist,
    .y = seq(1:length(plotlist)),
    .f = ~ ggsave(
      paste0(plot.type, "OutputPath", .y, ".png"),
      plot = .x,
      dpi = dpi,
      height = height,
      width = width
    )
  )
}
