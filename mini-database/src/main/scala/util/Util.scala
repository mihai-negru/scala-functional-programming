package util

object Util {

  /* Row typedef contains the column name and the value, defining a cell */
  type Row = Map[String, String]

  /* Line typedef is just a row without column names */
  type Line = List[String]
}
