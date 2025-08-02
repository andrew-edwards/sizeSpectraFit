##' Convert data of counts of species in length bins to counts in body-mass bins
##'  using species-specific length-weight coefficients
##'
##' For each species $s$, the length-weight relationship is
##' $w = \alpha_s l^{\beta_s}$
##' where $w$ is the estimated body mass (g) of an individual, $l$ is the known
##' body length (cm), and $alpha_s$ and $\beta_s$ are species-specific
##' parameters; this is equation (1) in our [MEPS paper](https://www.int-res.com/abstracts/meps/v636/p19-33/).
##'
##' Each count corresponds to numbers of a given species in a given length bin,
##' and we use the species-specific coefficients to convert the length bins to
##' body mass bins (see the MEPS paper for more details). Body-mass bins that
##' are repeated (likely from the same species being measured in the same length
##' bin, though could also occur coincidentally for different species) are
##' aggregated to give total counts for such bins, so that there are no
##' duplicated bins in the output. The resulting tibble is also sorted in
##' order of `bin_min`, starting with the lowest. Any species that do not have
##' length-weight coefficients get omitted from the resulting tibble.
##'
##' The resulting species-specific body-mass bins can then be used for the
##' MLEbins method.
##'
##' @param dat data.frame where each row is a count of a particular species in a
##'   particular length bin. It must include the columns (can have more):
##'  * `species` - name of the species
##'  * `length_bin_min` - minimum of the length bin
##'  * `length_bin_max` - maximum of the length bin
##'  * `bin_count` - count in the bin, can be non-integer (as can happen when
##'   raw counts are normalised to account for sampling effort).
##' @param coefficients data.frame of species-specific length-weight
##'   coefficients, with columns:
##'  * `species` - name of the species; must match the conventions used in `dat`
##'  * `alpha`, `beta` - length-weight coefficients for the given species
##' @param length_relationship_unit character giving the units used for $l$ in the the length-weight
##'   relationship; must be mm or cm (the default)
##' @param body_mass_relationship_unit character giving the units used for $w$ in the
##'   length-weight relationship; must be g (the default) or kg
##' @param length_data_unit character giving the units of the length data, must
##'   be mm or cm (the default).
##' @return data.frame with each row representing a body-mass bins, and columns:
##'  * `species` - unique species corresponding to `bin_min` and `bin_max` in
##'   the corresponding row. If any resulting bin definitions are the same for
##'   two or more distinct species, then this column is omitted. This could
##'   occur if the same length-weight relationship is used for different species
##'   (e.g. same relationship used for multiple rockfish species), or just by
##'   chance. The `species` is not needed for the subsequent MLEbins method, but
##'   it can be helpul for users to still keep track of species if they body-mass
##'   bins each correspond to just one species.
##'  * `bin_min` - minimum of the body-mass bin (g)
##'  * `bin_max` - maximum of the body-mass bin (g)
##'  * `bin_count` - total count of individuals within that body-mass bin; can
##'   be non-integer.
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##'  # Example using the first 1000 lines of the Mediterranean data in the
##'   package, copy from vignette:
##' med_subset <- mediterranean_data[1:1000, ]
##' # TODO need to functionalise the conversion of lengths to length bins
##'  mediterranean_example <-
##'   length_bins_to_body_mass_bins(med_subset,
##'                                 mediterranean_length_weight_coefficients,
##' TODO, ...)
##' }
length_bins_to_body_mass_bins <- function(dat,
                                          coefficients,
                                          length_relationship_unit = "cm",
                                          body_mass_relationship_unit = "g",
                                          length_data_unit = "cm"){

  stopifnot("dat needs to include columns species, length_bin_min, length_bin_max, and bin_count" = c("species", "length_bin_min", "length_bin_max", "bin_count") %in% names(dat))

  stopifnot("coefficients needs to include columns species, alpha, and beta" = c("species", "alpha", "beta") %in% names(coefficients))

  stopifnot("length_relationship_unit needs to be mm or cm" =
              length_relationship_unit %in% c("mm", "cm"))
  stopifnot("body_mass_relationship_unit needs to be g or kg" =
              body_mass_relationship_unit %in% c("g", "kg"))
  stopifnot("length_data_unit needs to be mm or cm" =
              length_relationship_unit %in% c("mm", "cm"))

  # Equation 1 of MEPS paper is for alpha and beta with lengths in cm and body
  #  mass in g, so using that and converting data and parameters here if needed.

  if(length_relationship_unit == "mm"){   # need it in cm, so equivalently just
                                          # scale the data
    dat <- dplyr::mutate(dat,
                         length_bin_min = length_bin_min / 10,
                         length_bin_max = length_bin_max / 10)
  }

  if(body_mass_relationship_unit == "kg"){   # need it in g, so just scale alpha
    coefficients <- dplyr::mutate(coeffieicents,
                                  alpha = 1000 * alpha)
  }

  if(length_data_unit == "mm"){   # need it in cm    TODO need to test if
                                  # correct with this and length_relatinships
                                  # both mm
    dat <- dplyr::mutate(dat,
                         length_bin_min = length_bin_min / 10,
                         length_bin_max = length_bin_max / 10)
  }

  dat_joined <- dplyr::left_join(dat,
                                 coefficients,
                                 by = "species") %>%
    dplyr::mutate(weight_bin_min = alpha * (length_bin_min)^beta,
                  weight_bin_max = alpha * (length_bin_max)^beta) %>%
    # Remove species with no length-weight coefficients:
    dplyr::filter(!is.na(alpha))

  # NOT DOING THIS NOW as need to return the original tibble (to not lose strata
  # etc.), and this only summarises by species. TODO decide on this, may be best to just remove it and tell people to
  # aggregate repeated measurements (by strata) at the start, like I'm doing for
  # med analysis. Too fiddly to keep this more general and allow for all strata
  # to be retained.

  if(FALSE){
    # Now aggregate duplicate bins (not keeping species in case duplicate bins are
    #  for different species, which can happen if they share length-weight
    #  coefficients), and then arrange by weight_bin_min, but first check if we
    #  can keep species in there also.  bin_min etc. here need to be
    #  weight_bin_min if keeping
    check_unique_species <- dplyr::summarise(dplyr::group_by(dat_joined,
                                                             bin_min,
                                                             bin_max,),
                                             number_species =
                                               length(unique(species))) %>%
      dplyr::ungroup()

    if(max(check_unique_species$number_species) > 1){
      # Cannot include species name
      dat_joined_2 <- dplyr::summarise(dplyr::group_by(dat_joined,
                                                       bin_min,
                                                       bin_max),
                                       bin_count = sum(bin_count)) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(bin_min)
    } else {
      # Can include species name
      dat_joined_2 <- dplyr::summarise(dplyr::group_by(dat_joined,
                                                       bin_min,
                                                       bin_max),
                                       bin_count = sum(bin_count),
                                       species = unique(species)) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(bin_min) %>%
        dplyr::relocate(species)
    }

  }

  dat_joined
}
