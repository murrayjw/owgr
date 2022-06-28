#' check_docker_running
#'
#' Function checks if docker is running and returns a message
#' with information on any currently active containers
#'
#' @param POSTGRES_HOST the host
#' @export check_docker_running
check_docker_running <- function() {

  cmd_line <- system2("docker",  "ps ",
                      stdout = TRUE,
                      stderr = TRUE)

  if (length(cmd_line) == 1) {
    msg <- ifelse(stringr::str_detect(cmd_line[[1]],"CONTAINER ID"),
                      "Docker is up but running no containers",
                      cmd_line[[1]])
    print(msg)

    return(F)
  } else {
   containers <- .format_docker_text(cmd_line)
    msg <- c("Docker is up, running these containers:", containers)
    print(msg)
    return(T)

  }

}


.format_docker_text <- function(cmd_line) {

  n <- length(cmd_line)
  lines <- sapply(1:n, function(x) {
    if(x == 1) {
      l <-gsub("CONTAINER ID","CONTAINER_ID",cmd_line[x])
      l <- gsub("\\s+"," ",l)
      strsplit(l, " ")
    } else {
      strsplit(cmd_line[x], "  ")
    }
  })

  nm <- lines[[1]]
  rw <- do.call(rbind, lines[-1])
  tb <- as.data.frame(rw) %>%
    setNames(nm)
  return(tb)
}
