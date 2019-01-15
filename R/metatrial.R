#' one trial
#'
#' @inheritParams metastats
#'
#' @export

metatrial <- function(between_study_variation = 0.6,
                      within_study_variation = 0.3,
                      median_ratio = 1.2,
                      rdist = "norm",
                      parameters = list(mean = 50, sd = 0.2),
                      n_df = meta_n(k = 3),
                      true_effect = 50) {

  # create this trial's sampling function
 rsample <- get(paste0("r", rdist))

 metadata <- n_df %>%
    metastats(sampling_fn = rsample, fn_parameters = parameters)  %>%
   dplyr::mutate(
      median = purrr::map_dbl(summary_stats, "median"),
      iqr = purrr::map_dbl(summary_stats, "third_quartile") -
        purrr::map_dbl(summary_stats, "first_quartile"),
      se = purrr::pmap_dbl(
        list(
          centre = median,
          spread = iqr,
          n = n
        ),
        metasim::effect_se,
        centre_type = "median",
        spread_type = "iqr"
      )
    ) %>%
    dplyr::select(study, group, n, se, median, iqr) %>%
  dplyr::mutate(effect = median, effect_se = se)

 purrr::map2_df(list(trial_m, trial_md),
                list(true_effect, abs(true_effect - true_effect / median_ratio)),
                .f = function(x, y) {
                  x(metadata, y)
                }
                ) %>%
   mutate(effect_type = c("m", "md"))

}
