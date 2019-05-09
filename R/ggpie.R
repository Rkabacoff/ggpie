#' Create pie charts
#'
#' Creates a single pie chart or several pie charts
#'
#' @param data A data frame.
#' @param x The variable to be plotted. It will be coerced to a factor.
#' @param by Optional "by" variable (see details).
#' @param offset Distance of pie slice labels from the origin (see details).
#' @param label.size Size of slice label text.
#' @param label.color Color of slice label text.
#' @param facet.label.size Size of the "by" variable labels.
#' @param border.color Color of pie slice borders.
#' @param border.width Width of pie slice borders.
#' @param legend Logical. If TRUE, adds a legend. If FALSE, the legend is omitted and each pie slice is
#' labeled.
#' @param percent Logical. If TRUE, prints percents for each pie slice.
#' @param title Optional title.
#' @param digits Number of decimal digits when printing percents.
#' @param nrow Number of rows for a faceted graph.
#' @import ggplot2
#' @import dplyr
#'
#' @details
#' If a \code{by} variable is present, a faceted graph is created with one pie chart for each
#' level of the \code{by} variable. The \code{nrow} option controls the number of rows
#' in the faceted chart. The \code{facet.label.size}
#' option controls the size of the facet label for each pie chart.
#'
#' The \code{offset} value controls the distance of the pie slice label from the pie's origin.
#' A value of 0.5 will center the label in each slice. A value of 1.0 will place each label on the
#' pie radius. A value larger than 1.0 will place the label outside the pie slice. You may have to
#' vary this value to get the most attractive spacing.
#'
#' @return a ggplot2 graph
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggpie(mpg, class)
#'
#' ggpie(mpg, class, offset=0.8, label.color="white")
#'
#' ggpie(mpg, class, offset=1.3,
#'       title="Auto Class Distribution")
#'
#' ggpie(mpg, class, offset=1.3, legend=FALSE,
#'       title="Auto Class Distribution")
#'
#' ggpie(mpg, class, offset=1.3, percent=FALSE,
#'       legend=FALSE, title="Auto Class Distribution")
#'
#' ggpie(mpg, class, year, offset=1.3, label.size=3,
#'       legend=FALSE, title="Car Class by Year")
#'
#'ggpie(mpg, drv, cyl, nrow=2, label.size=3.5,
#'      title="Car Drive Type by Number of Cylinders") +
#'  scale_fill_brewer(palette="Dark2") +
#'  theme(legend.position = "top")
#'
#' ggpie(mpg, drv, trans, nrow=3, border.color="white", border.width=1.5,
#'       label.color="black", label.size=3, offset=.7,
#'       title="Auto Drive Type by Transmission\n") +
#'   theme(legend.position=c(.8, .2)) +
#'   scale_fill_manual(values=c("yellow", "skyblue", "green"))
ggpie <- function(data,
                  x,
                  by,
                  offset=.5,
                  label.size=4,
                  label.color="black",
                  facet.label.size=11,
                  border.color="black",
                  border.width=.5,
                  legend = TRUE,
                  percent = TRUE,
                  title,
                  digits = 0,
                  nrow=1){

  # import options
  BY = 0
  x <- enquo(x)
  if (!missing(by)) {
    BY = 1
    by <- enquo(by)
  }

  # rescale offset
  offset <-  offset - 0.5

  # make sure x is a factor
  class_name <- paste0(quo_name(x))
  data <- data %>%
    mutate(!!class_name := factor(!!x))

  # group data
  if (BY){
    df <- data %>% group_by(!!by, !!x)
  } else {
    df <- data %>% group_by(!!x)
  }

  # calculate plot statistics
  df <- df %>%
    tally() %>%
    mutate(prop = n/sum(n)*100,
           cs = rev(cumsum(rev(prop))),
           pos = prop/2 + lead(cs, 1),
           pos = if_else(is.na(pos), prop/2, pos)
    )

  # create pie chart
  p <- ggplot(df, aes(x = "" ,
                      y = prop,
                      fill = !!x)) +
    geom_col(width = 1,
             size=border.width,
             col=border.color) +
    coord_polar(theta = "y",
                start = 0 )

  # create slice labels
  if (percent & legend){
    p <- p +
      geom_text(aes(y = pos,
                    label = paste0(round(prop, digits), "%")),
                size=label.size,
                color = label.color,
                data = df,
                show.legend = FALSE,
                nudge_x = offset)
  }
  if (percent & !legend){
    p <- p +
      geom_text(aes(y = pos,
                    label = paste0(!!x, "\n(", round(prop, digits), "%)")),
                size=label.size,
                color = label.color,
                data = df,
                show.legend = FALSE,
                nudge_x = offset)

  }
  if (!percent & legend){
    p <- p

  }
  if (!percent & !legend){
    p <- p +
      geom_text(aes(y = pos,
                    label = !!x),
                size=label.size,
                color = label.color,
                data = df,
                show.legend = FALSE,
                nudge_x = offset)

  }


  # create multiple pies if by variable present
  if (BY) {
    p <- p + facet_wrap(vars(!!by), nrow=nrow)
  }

  # clean up theme, title, and legend
  p <- p + theme_void() +
    theme(strip.text.x = element_text(size = facet.label.size))

  if (!missing(title)){
      # centered title with space after
      p <- p + ggtitle(title) +
        theme(plot.title = element_text(hjust = 0.5))
  }

  if (!legend){
    p <- p + theme(legend.position="none")
  }

  # return pie chart
  p

}

