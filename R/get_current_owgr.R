
#' Function to extract the current official world golf rankings
#'
#' @details This function extracts the current Official World
#' Golf Rankings from www.owgr.com. A tibble is returned which
#' contains the current rankings along with links to player
#' specific pages. These links can be used as input to
#' player data extraction functions.
#'
#' @examples
#' get_current_owgr()
#'
#'@importFrom magrittr %>%
#'@export get_current_owgr
get_current_owgr <- function() {

  owgr_html <- .get_current_owgr_html()

  ranking_table_node <- rvest::html_node(owgr_html,
                                    "#ranking_table")

  current_w <- rvest::html_nodes(ranking_table_node, "h2") %>%
    rvest::html_text()

  current_d <-  rvest::html_node(ranking_table_node, "time") %>%
    rvest::html_text()

  player_frame <- .create_player_frame(ranking_table_node)

  owgr_table <- rvest::html_node(ranking_table_node, ".table_container") %>%
    rvest::html_table() %>%
    dplyr::as_tibble() %>%
    janitor::clean_names() %>%
    dplyr::select(-dplyr::contains("name"))

  owgr_data <- player_frame %>%
    dplyr::bind_cols(owgr_table) %>%
    dplyr::mutate(current_week = current_w,
                  current_date = lubridate::dmy(current_d))


  return(owgr_data)
}



# Helpers -----------------------------------------------------------------

.get_current_owgr_html <- function() {
  html_obj <- xml2::read_html(
    'http://www.owgr.com/ranking?pageNo=1&pageSize=All&country=All'
  )
  return(html_obj)
}

.get_player_links <- function(html_obj) {

  player_links <- rvest::html_nodes(html_obj, "a") %>%
    rvest::html_attr('href') %>%
    grep("playerID=", ., value = T)

  return(player_links)
}

.get_player_ids <- function(player_links){
  match_reg <- gregexpr('[0-9]+', player_links)
  player_ids <- unlist(regmatches( player_links, match_reg))
  return(player_ids)
}

.get_player_names <- function(html_obj) {

  player_names <-rvest::html_nodes(html_obj, ".name") %>%
    rvest::html_text() %>%
    .[. != ""] %>%
    .[. != "Name"]

  return(player_names)
}

.create_player_frame <- function(html_obj) {

  pl  <- .get_player_links(html_obj)
  pid <- .get_player_ids(pl)
  pn  <- .get_player_names(html_obj)

  player_data <- dplyr::tibble(
    player_id = pid,
    player_name = pn,
    player_link = pl
  ) %>%
    dplyr::filter(grepl('/en/', player_link))

  return(player_data)
}




