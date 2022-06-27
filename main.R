library(owgr)



DBI::dbConnect(
 drv      = RPostgres::Postgres(),
 host     = 'localhost',
 dbname   = Sys.getenv('POSTGRES_DB'),
 port     = 5432,
 user     = Sys.getenv('POSTGRES_USER'),
 password = Sys.getenv('POSTGRES_PASSWORD')

)
