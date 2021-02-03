#' Functions to import data from a \strong{pmcharacterr} database. 
#' 
#' @param con Database connection to a \strong{pmcharacterr} database. 
#' 
#' @param organic_carbon_multiplier Multiplier for \code{organic_carbon} 
#' measurements to be transformed to organic matter. Usually, a value of 
#' \code{1.6} or \code{1.8} is used. The variable name will also be switched to 
#' \code{organic_matter}. 
#' 
#' @param convert_units Should units be normalised to \code{ug.m-3}. This 
#' requires other units to be set correctly in the \code{measurements} table. 
#' 
#' @param value_type A vector to filter the `value_type` variable to. 
#' 
#' @param use_complete_dates If a `complete_observation_dates` table is present, 
#' should the observations be filtered to these dates?
#' 
#' @param tz Time zone to format dates to.
#' 
#' @return Tibble.
#' 
#' @author Stuart K. Grange
#' 
#' @export
import_measurements <- function(con, organic_carbon_multiplier = 1, 
                                convert_units = FALSE, value_type = NA, 
                                use_complete_dates = FALSE, tz = "UTC") {
  
  # Check for table existence
  if (!databaser::db_table_exists(con, "measurements")) {
    stop("`measurements` does not exist.", call. = FALSE)
  }
  
  df <- databaser::db_get(
    con, 
    "SELECT m.field_campaign, 
    field_campaigns.field_campaign_name,
    m.data_source, 
    m.laboratory_identifier, 
    m.filter,
    m.method_type, 
    m.calculated,
    m.particulate_fraction, 
    m.laboratory_notes, 
    m.date, 
    m.date_end,
    m.site, 
    sites.site_name,  
    m.variable, 
    m.value_type, 
    m.analytical_note_id,
    m.unit, 
    m.value
    FROM measurements AS m
    LEFT JOIN sites 
    ON m.site = sites.site
    LEFT JOIN field_campaigns 
    ON m.field_campaign = field_campaigns.field_campaign
    ORDER BY m.site,
    date,
    variable,
    m.data_source,
    particulate_fraction"
  ) %>% 
    mutate(date = threadr::parse_unix_time(date, tz = tz),
           date_end = threadr::parse_unix_time(date_end, tz = tz))
  
  # Use a multiplier
  if (organic_carbon_multiplier != 1) {
    df <- df %>% 
      mutate(
        organic_carbon = if_else(
          variable == "organic_carbon" & !is.na(variable), TRUE, FALSE
        ),
        variable = if_else(organic_carbon, "organic_matter", variable),
        value = if_else(organic_carbon, value * !!organic_carbon_multiplier, value)
      ) %>% 
      select(-organic_carbon)
  }
  
  # Convert units
  if (convert_units) {
    df <- df %>% 
      mutate(unit_converted = if_else(unit == "ng.m-3" & !is.na(unit), TRUE, FALSE),
             value = if_else(unit_converted, value / 1000, value),
             unit = if_else(unit_converted, "ug.m-3", unit))
  }
  
  # Filter observations to `complete_observation_dates` table
  if (use_complete_dates) {
    
    # Get filtering table
    df_complete <- import_complete_dates(con, tz = tz) %>% 
      select(-site_name)
    
    # Filter observations
    df <- inner_join(df, df_complete, by = c("particulate_fraction", "date", "site"))
    
  }
  
  # Filter to particular value types
  if (!is.na(value_type[1])) {
    df <- filter(df, value_type %in% !!value_type)
  }
  
  return(df)
  
}


#' @rdname import_measurements
#' @export
import_field_campaigns <- function(con, tz = "UTC") {
  
  # Check for table existence
  if (!databaser::db_table_exists(con, "field_campaigns")) {
    stop("`field_campaigns` does not exist.", call. = FALSE)
  }
  
  databaser::db_get(
    con, 
    "SELECT * 
    FROM field_campaigns
    ORDER BY field_campaign"
  ) %>% 
    mutate(across(c("date_start", "date_end"), lubridate::ymd, tz = tz))
  
}


#' @rdname import_measurements
#' @export
import_sites <- function(con) {
  
  # Check for table existence
  if (!databaser::db_table_exists(con, "sites")) {
    stop("`sites` does not exist.", call. = FALSE)
  }
  
  databaser::db_get(
    con, 
    "SELECT * 
    FROM sites 
    ORDER BY site"
  )
  
}


#' @rdname import_measurements
#' @export
import_mass_contributions <- function(con) {
  
  # Check for table existence
  if (!databaser::db_table_exists(con, "mass_contributions")) {
    stop("`mass_contributions` does not exist.", call. = FALSE)
  }
  
  databaser::db_get(
    con, 
    "SELECT * 
    FROM mass_contributions
    ORDER BY field_campaign, 
    site"
  )
  
}


#' @rdname import_measurements
#' @export
import_data_sources <- function(con) {
  
  # Check for table existence
  if (!databaser::db_table_exists(con, "data_sources")) {
    stop("`data_sources` does not exist.", call. = FALSE)
  }
  
  databaser::db_get(
    con, 
    "SELECT * 
    FROM data_sources
    ORDER BY data_source"
  )
  
}


#' @rdname import_measurements
#' @export
import_laboratories <- function(con) {
  
  # Check for table existence
  if (!databaser::db_table_exists(con, "laboratories")) {
    stop("`laboratories` does not exist.", call. = FALSE)
  }
  
  databaser::db_get(
    con, 
    "SELECT * 
    FROM laboratories
    ORDER BY laboratory"
  )
  
}


#' @rdname import_measurements
#' @export
import_complete_dates <- function(con, tz = "UTC") {
  
  # Check for table existence
  if (!databaser::db_table_exists(con, "complete_observation_dates")) {
    stop("`complete_observation_dates` does not exist.", call. = FALSE)
  }
  
  databaser::db_get(
    con, 
    "SELECT * 
    FROM complete_observation_dates
    ORDER BY site,
    particulate_fraction,
    date"
  ) %>% 
    mutate(date = threadr::parse_unix_time(date, tz = tz))
  
}
