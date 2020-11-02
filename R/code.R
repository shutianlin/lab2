
#' Make a boxplot of payments by DRG code
#'
#' @param type type of payments you want, can be 'Average Covered Charges', 'Average Total Payments', or 'Average Medicare Payments'
#'
#' @return A boxplot of payments by DRG code
#' @export
#'
#' @examples pay_boxplot('Average Covered Charges')
pay_boxplot <- function(type)
{
  drg %>%
    ggplot(aes(x = `DRG Definition`,
               y = get(type))) +
    geom_boxplot() +
    scale_y_log10() +
    ylab(paste0(type)) +
    xlab("DRG Code") +
    ggtitle(paste0("Boxplot of ", type, " by DRG code"))
}


#' Calculates statistics over all of the DRG codes for average Medicare payments
#'
#' @param stat name of the statistics you want, can be 'Mean', 'Median', or 'Sd'
#'
#' @return A dataframe that includes DRG codes and the statistics you want for average Medicare payments
#' @export
#'
#' @examples cal_stats('Mean')
cal_stats <- function(stat){
  drg %>%
    group_by(`DRG Definition`) %>%
    summarize(Mean = mean(`Average Medicare Payments`),
              Median = median(`Average Medicare Payments`),
              Sd = sd(`Average Medicare Payments`)) %>%
    select(c(`DRG Definition`, paste0(stat)))

}


