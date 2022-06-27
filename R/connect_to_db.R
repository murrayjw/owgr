#' connect_to_db
#' @description Connect to the postgres database that lives in docker.
#' @param POSTGRES_HOST the host
#' @param POSTGRES_PORT the port number
#' @param POSTGRES_DB the database name
#' @param POSTGRES_USER the user
#' @param POSTGRES_PASSWORD the password
#' @param n_connection_tries number of attempts to connect to the database. default is 5
#' @details The default value for each parameter is taken from the .Environ file.
#
#' @export connect_to_db
connect_to_db <- function(POSTGRES_HOST = NULL,
                                POSTGRES_PORT = NULL,
                                POSTGRES_USER = NULL,
                                POSTGRES_PASSWORD = NULL,
                                POSTGRES_DB = NULL,
                                n_connection_tries = 5) {

  parent_formals <- as.list(environment())

  db_credentials <- lapply(names(parent_formals), function(x) {
    if (is.null(parent_formals[[x]])) {
      parent_formals[[x]] <- Sys.getenv(x)
    } else {
      parent_formals[[x]]
    }
  })
  names(db_credentials) <- names(parent_formals)

  n <- 1

  while( n <= n_connection_tries) {

    message(glue::glue("Trying to connect: try {n}"))

    connection <- try({
      DBI::dbConnect(RPostgres::Postgres(),
                host = db_credentials$POSTGRES_HOST,
                port = db_credentials$POSTGRES_PORT,
                user = db_credentials$POSTGRES_USER,
                password = db_credentials$POSTGRES_PASSWORD,
                dbname = db_credentials$POSTGRES_DB
      )
    })

    if (inherits(connection, "PqConnection")) {

      return(connection)

    } else {
      n <- n + 1
    }

  }

  stop("Database connection failed")

}


