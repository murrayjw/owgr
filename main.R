library(owgr)
library(dplyr)
library(rlog)

docker_flag <- check_docker_running()

if(!docker_flag) {
  rlog::log_error("Docker container is not running")
} else {

}

rlog::log_info('Connecting to database:')
con <- connect_to_db()

rlog::log_info('Extracting available tables:')
available_tables <- DBI::dbListTables(con)

rlog::log_info('Main if/else statement')
if(length(available_tables) == 0) {

        time <- Sys.time()

        current_owgr <- tryCatch(
          expr = get_current_owgr(),
          error = function(e) {
            rlog::log_error("Could not extract current OWGR")
            rlog::log_debug(e)
          })

        rlog::log_info('Adding last update time')
        current_owgr <- current_owgr %>%
                mutate(last_update_time = time)

        tryCatch(expr = copy_to(con,
                                current_owgr,
                                name = "CURRENT_OWGR",
                                temporary = FALSE),
                 error = function(e){
                   rlog::log_error("Could not write to the database")
                   rlog::log_debug(e)
                 })
} else {

  rlog::log_info('Connecting to database:')
  last_tbl <- tbl(con, "CURRENT_OWGR")

  current_owgr <- tryCatch(
    expr = get_current_owgr(),
    error = function(e) {
      rlog::log_error("Could not extract current OWGR")
      rlog::log_debug(e)
    })

  last_update_time <- last_tbl %>%
    summarize(last_update_date = max(current_date)) %>%
    collect() %>%
    pull(last_update_date)

  current_update_time <- current_owgr %>%
    summarize(last_update_date = max(current_date)) %>%
    collect()%>%
    pull(last_update_date)

  if(current_update_time > last_update_date) {

    copy_to(con,
            current_owgr,
            name = "CURRENT_OWGR",
            temporary = FALSE)
  }
}

rlog::log_info('Success!')
