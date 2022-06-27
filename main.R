library(owgr)
library(dplyr)

con <- connect_to_db()

available_tables <- DBI::dbListTables(con)

if(length(available_tables) == 0) {

        time <- Sys.time()

        current_owgr <- get_current_owgr()

        current_owgr <- current_owgr %>%
                mutate(last_update_time = time)

        copy_to(con,
                current_owgr,
                name = "CURRENT_OWGR",
                temporary = FALSE)
} else {

  last_tbl <- tbl(con, "CURRENT_OWGR")

  current_owgr <- get_current_owgr()

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
