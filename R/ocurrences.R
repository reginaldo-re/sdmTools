#' @export
occurrences_to_shapefile <- function(o_file = NULL, sp_names = NULL, shp = NULL, a_crs = "EPSG:4326"){
  sp_names <- unlist(sp_names)

  if (o_file %>% is_null() || shp %>% is_null()){
    if (sp_occurrences %>% names() %>% length() != 3){
      "Both o_file and sp_names must have diffent values then NULL." %>%
        abort()
    }
  }

  sp_occurrences <- o_file %>%
    vroom::vroom(show_col_types = F) %>%
    janitor::clean_names() %>%
    janitor::remove_empty(c("rows", "cols")) %>%
    mutate(across(is.character, str_squish))

  if (sp_occurrences %>% names() %>% length() != 3){
    "Occurrences file must be three columns named: species, long, lat." %>%
      abort()
  }

  sp_column_name <- sp_occurrences %>%
    select_if(is.character) %>%
    names() %>%
    pluck(1)

  sp_occurrences <- sp_occurrences %>%
    rename(species = all_of(sp_column_name))

  if (!is.null(sp_names)){
    sp_names <- sp_names %>%
      str_squish()
    sp_occurrences <-  sp_occurrences %>%
      filter(species %>% is_in(sp_names))
  }
  sp_occurrences %>%
    mutate(across(!species, ~ .x %>%  as.character() %>% as.numeric()))

  sp::coordinates(sp_occurrences) <- ~long+lat
  if (is.character(a_crs)){
    crs(sp_occurrences) <- crs(a_crs)
  } else {
    crs(sp_occurrences) <- a_crs
  }

  return(
    sp_occurrences %>%
      crs(shp) %>%
      spTransform()
  )
}

#' @export
occurrences_to_pa_shapefile <- function(shp_occ, shp_area, sp_names){
  sp_names <- sp_names %>%
    unlist() %>%
    str_squish()

  pa_matrix <- shp_area@data[, F]

  for (spp in sp_names){
    pa_matrix <- pa_matrix %>%
      bind_cols(
        shp_area %>%
          sp::over(shp_occ[shp_occ@data$species == spp, ])
      )
  }
  names(pa_matrix) <- sp_names

  pa_matrix[!is.na(pa_matrix)] <- 1
  pa_matrix[is.na(pa_matrix)] <- 0

  pa_matrix <- pa_matrix %>%
    mutate_all(~ as.integer(.))

  grid_pa_matrix <- shp_area

  grid_pa_matrix@data <-  pa_matrix %>%
    as.data.frame() %>%
    set_rownames(shp_area@data %>% rownames())

  return(grid_pa_matrix)
}

#' @export
map_of_occurrences <- function(shp_occ, shp_area, title="", crs_subtitle=T){
  map_tmp <- ggplot(
    data =  fortify(shp_area),
    aes(
      x = long,
      y = lat,
      group=group
    )
  )

  map_tmp <- map_tmp +
    scale_x_continuous(labels = comma) +
    scale_y_continuous(labels = comma) +
    coord_equal()

  if (title!=""){
    map_tmp <- map_tmp +
      labs(title=title)
  }

  if (crs_subtitle){
    map_tmp <- map_tmp +
      labs(subtitle = paste0(crs(shp_area)))
  }

  map_tmp <- map_tmp +
    geom_polygon(colour = "black", fill=NA)

  map_tmp <- map_tmp +
    geom_point(data = fortify(shp_occ), aes(x = long, y = lat, group=species, color=species))

  return(map_tmp)
}

#' @export
map_of_pa <- function(shp_pa, shp_area, sp_names){
  sp_names <- sp_names %>%
    unlist() %>%
    to_snake_case()

  df_temp <- shp_pa@data %>%
    rownames_to_column("id") %>%
    tidyr::pivot_longer(-id, names_to = "species", values_to="presence")

  legend_title  <- ifelse(length(sp_names)==1, sp_names[[1]], "Presence/Absence")

  ggplot(data = fortify(shp_area) %>% left_join(df_temp)) +
    aes(x = long, y = lat, group = group) +
    geom_polygon(aes(fill = as.factor(presence))) +
    scale_fill_manual(legend_title, values=c("darkseagreen2", "tomato")) +
    scale_x_continuous(labels = comma) +
    scale_y_continuous(labels = comma) +
    coord_equal()
}
