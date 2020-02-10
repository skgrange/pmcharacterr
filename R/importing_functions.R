#' Functions to import data from a \strong{pmcharacterr} database. 
#' 
#' @param con Database connection to a \strong{pmcharacterr} database. 
#' 
#' @param organic_carbon_multiplier Multiplier for organic carbon measurements. 
#' Often, a value of \code{1.6} or \code{1.8} is used.
#' 
#' @param tz Time zone to format dates to. 
#' 
#' @return Tibble.
#' 
#' @author Stuart K. Grange
#' 
#' @export
import_measurements <- function(con, organic_carbon_multiplier = 1, 
                                tz = "UTC") {
  
  df <- databaser::db_get(
    con, 
    "SELECT m.field_campaign, 
    field_campaigns.field_campaign_name,
    m.data_source, 
    data_sources.data_source_name,
    m.laboratory_identifier, 
    m.filter,
    m.method_type, 
    m.calculated,
    m.particulate_fraction, 
    m.date_analysis, 
    m.laboratory_notes, 
    m.date, 
    m.date_end,
    m.site, 
    sites.site_name,  
    m.variable, 
    m.value_type, 
    m.unit, 
    m.value
    FROM measurements AS m
    LEFT JOIN sites 
    ON m.site = sites.site
    LEFT JOIN data_sources 
    ON m.data_source = data_sources.data_source
    LEFT JOIN field_campaigns 
    ON m.field_campaign = field_campaigns.field_campaign
    ORDER BY m.site,
    date,
    variable,
    m.data_source,
    particulate_fraction"
  ) %>% 
    mutate(date = threadr::parse_unix_time(date, tz = tz),
           date_end = threadr::parse_unix_time(date_end, tz = tz),
           date_analysis = threadr::parse_unix_time(date_analysis, tz = tz))
  
  # Use a multiplier
  if (organic_carbon_multiplier != 1) {
    df <- df %>% 
      mutate(
        value = if_else(
          variable == "organic_carbon", value * !!organic_carbon_multiplier, value
        )
      )
  }
  
  return(df)
  
}


#' @rdname import_measurements
#' @export
import_field_campaigns <- function(con) {
  
  databaser::db_get(
    con, 
    "SELECT * 
    FROM field_campaigns
    ORDER BY field_campaign"
  )
  
}


#' @rdname import_measurements
#' @export
import_sites <- function(con) {
  
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
  
  databaser::db_get(
    con, 
    "SELECT * 
    FROM laboratories
    ORDER BY laboratory"
  )
  
}