#' Set up initial parameters for Cosmos DB querying
#' 
#' @param sql.what String for specifying what fields to retrieve. Typically called select condition
#' @param sql.where String for specifying what filter to use on data. Typically called search condition
#' @param sql.orderby String for specifying what field to order query by.
#' @param asc Logical for Is the order ascending.
#' @return String containing a full SELECT statement for the database
#' @keywords query select where
#' @export
#' @examples
#' constructQuery(sql.what = "c.contact.eloquaId", sql.where = "c.contact.eloquaId != null")

constructQuery <- function(sql.what, sql.where, sql.orderby, asc) {
    if(!asc){
        sql.rank <- 'DESC'
    } else{
        sql.rank <- 'ASC'
    }
    # Create the query using predicate if it exists
    if (sql.where == "" & sql.orderby == "") {
        full.query <- paste("SELECT", sql.what, "FROM c", sep = " ")
    } else if(sql.orderby == ""){
        full.query <- paste("SELECT ", sql.what, " FROM c WHERE (", sql.where, ")", sep = "")
    } else{
        full.query <- paste("SELECT ", sql.what, " FROM c WHERE (", sql.where, ")"," ORDER BY (",sql.orderby , ")", sql.rank,sep = "")
    }

    # Return the query
    full.query
}
