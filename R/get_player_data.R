


#' @export get_player_year_data
#'
get_player_year_data <- function(player_id, year = NULL) {

  player_url <- get_player_url(player_id)

  player_html <-  xml2::read_html(player_url)

  player_name <- .get_player_name(player_html)

  if(is.null(year)) {

    stop("Must provide a year")

  } else {

    available_years <- get_available_years(player_html)

    if(!year %in% available_years) {
      stop("Player has no data for ", year, ". the following are available: \n",
           capture.output(available_years))
    }
  }

  year_url <- glue::glue('{player_url}&year={year}')

  player_data <- xml2::read_html(year_url) %>%
    rvest::html_node(".table_container") %>%
    rvest::html_table() %>%
    janitor::clean_names()%>%
    mutate(player_id = player_id)

  player_data <- player_data %>%
    dplyr::mutate(player_id = player_id,
                  player_name = player_name ) %>%
    dplyr::select(player_name, player_id, everything())

  return(player_data)

}

#' Get the valid years of data for a given player_id
#'
#' @param player_id a valid player id value. Use \code{get_current_owgr} to
#' get a list of valid player_id values
#'
#' @example
#' get_available_years(11676) # Tony Finau
#'
#' @export get_available_years
#'
get_available_years <-  function(player_id) {

  player_url <- get_player_url(player_id)

  player_html <-  xml2::read_html(player_url)

  available_years <- rvest::html_nodes(player_html, "option") %>%
    rvest::html_text()
  available_years <- available_years[available_years!="Counting Events"]

  return(available_years)
}



#' @export get_player_url
#'
get_player_url <- function(player_id) {

  url <- glue::glue('http://www.owgr.com/en/Ranking/PlayerProfile.aspx?playerID={player_id}')
  return(url)
}

.get_player_name <- function(html) {

  player_name <- rvest::html_nodes(html, 'h2') %>%
    rvest::html_text() %>%
    .[1]

  return(player_name)
}
