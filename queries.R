##################################################
# A Shiny app for creating custom IPEDS peer     #
# reports.                                       #
##################################################

flexible_query <- function(metric, tablename, instnm_table)
{
  my_q <- paste0("select a.UNITID, a.INSTNM as Institution, b.", metric," as y ",
               "from ", instnm_table, " a ",
               "inner join ", tablename, " b on a.UNITID = b.UNITID ",
               "where a.UNITID in ", R_vector_to_SQL_vector(id_vec), " "
  )
  my_r <- sqlQuery(channel = ipeds, my_q)
  return(my_r)
}

## Load Peer Institutions -----
my_peers <- read.csv("report_peers.csv", header = TRUE, stringsAsFactors = FALSE)
id_vec <- as.numeric(my_peers[ ,1])

id_self <- id_vec[1]

## Load Report Metrics from csv file -----
my_report_metrics <- read.csv("report_metrics.csv", header = TRUE, stringsAsFactors = FALSE)

report_list <- list()

## Query metrics from Database -----
for (i in 2:nrow(my_report_metrics)) {
  i_r <- flexible_query(metric = my_report_metrics$Metric[i],
                      tablename = my_report_metrics$Table[i],
                      instnm_table = my_report_metrics$Table[1])
  
  i_r <- i_r[match(id_vec, as.numeric(as.character(i_r$UNITID))), ]
  report_list[[i]] <- i_r
}
