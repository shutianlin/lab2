
#' Make a boxplot of payments by DRG code
#'
#' @param type type of payments you want, can be 'Average Covered Charges', 'Average Total Payments', or 'Average Medicare Payments'
#'
#' @return A boxplot of payments by DRG code
#' @export
#'
#' @examples pay_boxplot('Average Covered Charges')
pay_boxplot <- function(type) ## write a function to make a boxplot
{
  drg %>%
    ggplot(aes(x = `DRG Definition`,
               y = get(type))) + ## according to the type of payments
    geom_boxplot() + ## create a boxplot
    scale_y_log10() + ## scale y axis
    ylab(paste0(type)) + ## add y axis label
    xlab("DRG Code") + ## add x axis label
    ggtitle(paste0("Boxplot of ", type, " by DRG code")) ## add title
}

#' Calculates statistics over all of the DRG codes for average Medicare payments
#'
#' @param stat name of the statistics you want, can be 'Mean', 'Median', or 'Sd'
#'
#' @return A dataframe that includes DRG codes and the statistics you want for average Medicare payments
#' @export
#'
#' @examples cal_stats('Mean')
cal_stats <- function(stat) {
  drg %>%
    group_by(`DRG Definition`) %>% ## group by DRG codes
    summarize(
      ## calculate statistics
      Mean = mean(`Average Medicare Payments`), ## calculate mean
      Median = median(`Average Medicare Payments`), ## calculate median
      Sd = sd(`Average Medicare Payments`) ## calculate standard deviation
    ) %>%
    select(c(`DRG Definition`, paste0(stat))) ## select the statistics according to the input

}


