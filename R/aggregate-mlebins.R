aggregate_mlebins <- function(res_list,    # Now doing just the lists in
                                        # analysis.Rmd to save objects, then
                                        # using some of this function in the
                                        # plotting and tables maybe.
                              res_table,
                              strata){
                              #group_names = c("crust",
                              #                "actin",
                              #                "chondr",
                              #               "cephsmall",
                              #                "cephlarge")){

  # TODO might not need group_names as naming them in the call to this function
  res_all <- list()

  group_names <- names(res_list)

  aggregated_data_temp <- tibble::tibble()

  for(i in 1:length(res_list)){
    res <- res_list[[i]]

    stopifnot("determine_xmin_and_fit_mlebins" %in% class(res))

    res_all[[i]] <- res[["mlebins_fit"]]

    # Just want the mlebins_fit object, keeping it intact it is class
    # size_spectrum_mlebins and the plotting works automatically.

    aggregated_data_temp <- rbind(aggregated_data_temp,
                                  res[["mlebins_fit"]]$data)
  }

  names(res_all) <- group_names

  # Next aggregate matching bin_min and bin_max (might not be any, as would
  # require two species in different groups to have same bin_min and bin_max),
  # but need to recalculate anyway since count_gte_bin_min etc. will be different
  # for aggregated data set compared to values in each group.

  aggregated_data <- dplyr::summarise(group_by(aggregated_data_temp,
                                               bin_min,
                                               bin_max),
                                      bin_count = sum(bin_count)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(bin_min)


  # Can't do in dplyr, same approach as in fit_size_spectrum_mlebins()
  count_gte_bin_min <- rep(NA, length = nrow(aggregated_data))
  low_count <- count_gte_bin_min
  high_count <- count_gte_bin_min

  for(iii in 1:length(count_gte_bin_min)){
    count_gte_bin_min[iii] <- sum( (aggregated_data$bin_min >= aggregated_data$bin_min[iii]) * aggregated_data$bin_count)
    low_count[iii] <- sum( (aggregated_data$bin_min >= aggregated_data$bin_max[iii]) * aggregated_data$bin_count)
    high_count[iii] <- sum( (aggregated_data$bin_max > aggregated_data$bin_min[iii]) * aggregated_data$bin_count)
  }

  aggregated_data$count_gte_bin_min <- count_gte_bin_min
  aggregated_data$low_count <- low_count
  aggregated_data$high_count <- high_count

  res_all[[length(res_list) + 1]] <- aggregated_data
  names(res_all)[length(res_list) + 1] <- "aggregated_data"

  res_table_here <- dplyr::filter(res_table,
                                  Strata == strata)

  res_all[[length(res_list) + 2]] <- res_table_here
  names(res_all)[length(res_list) + 2] <- "res_table"

  class(res_all) <- c("mlebins_list",
                      class(res_all))
  # Save as a mlebins_list object, then create plotting function for that,
  # including aggregation, but can easily use the size_spectrum_mlebins plotting functions.

  return(res_all)
}
