
html_obj <- try(owgr:::.get_current_owgr_html())
ranking_table_node <- try(rvest::html_node(html_obj,
                                       "#ranking_table"))

test_that("'http://www.owgr.com returns xml ojbect", {
  expect_s3_class(html_obj, "xml_document")
})

test_that("'http://www.owgr.com contains correct HTML", {


    # the ranking table node exists
    expect_s3_class(ranking_table_node, "xml_node")
    # the ranking table current week name exists
    expect_s3_class(rvest::html_node(ranking_table_node, "h2"),
                    "xml_node")
    # the ranking table current week date exists
    expect_s3_class(rvest::html_node(ranking_table_node, "time"),
                 "xml_node")
    # the data table node exists
    expect_s3_class(rvest::html_node(ranking_table_node, ".table_container"),
                    "xml_node")

})

test_that("Current OWGR table contains data", {
  current_owgr_table <- rvest::html_table(ranking_table_node)

  player_links <- owgr:::.get_player_links(ranking_table_node)
  player_ids <- owgr:::.get_player_ids(player_links)
  player_names <- owgr:::.get_player_names(ranking_table_node)

  # the html object contains one table
  expect_s3_class(current_owgr_table, "tbl_df")
  # there are 11 fields
  expect_equal(ncol(current_owgr_table), 11)
  # there are observations
  expect_gt(nrow(current_owgr_table), 0)
  # same number of player page links  as rows in the data
  expect_equal(length(player_links), nrow(current_owgr_table))
  # same number of player ids  as rows in the data
  expect_equal(length(player_ids), nrow(current_owgr_table))
  # same number of player names  as rows in the data
  expect_equal(length(player_names), nrow(current_owgr_table))

})
