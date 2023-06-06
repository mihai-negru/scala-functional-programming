import util.Util.{Line, Row}

/**
 * <p>Typeclass defining a filter condition on a tabular.</p>
 */
trait FilterCond {

  /**
   * <p>Logical `and` operator applied on two filter conditions.</p>
   *
   * @param other another filter condition to chain with the current one.
   * @return this `and` other filter condition.
   */
  def &&(other: FilterCond): FilterCond = And(this, other)

  /**
   * <p>Logical `or` operator applied on two filter conditions.</p>
   *
   * @param other another filter condition to chain with the current one.
   * @return this `or` other filter condition.
   */
  def ||(other: FilterCond): FilterCond = Or(this, other)

  /**
   * <p>Evaluates the condition to a boolean value if the
   * condition can be applied on the tabular.</p>
   *
   * @param r row typedef to apply with condition on the tabular.
   * @return Some(booleanValue) if the condition could be applied,
   *         or None otherwise.
   */
  def eval(r: Row): Option[Boolean]
}

/**
 * <p>Atom case class for the FilterCond typeclass.</p>
 *
 * @param colName name of the column to apply the condition.
 * @param predicate function to filter the value of the column name.
 */
case class Field(colName: String, predicate: String => Boolean) extends FilterCond {
  override def eval(r: Row): Option[Boolean] = {
    r.get(colName) match {
      case Some(value) => Some(predicate(value))
      case None => None
    }
  }
}

/**
 * <p>And case class for the FilterCond typeclass in order
 * to compute the logical `&&` operator on two conditions.</p>
 *
 * @param f1 first filter condition.
 * @param f2 second filter condition.
 */
case class And(f1: FilterCond, f2: FilterCond) extends FilterCond {
  override def eval(r: Row): Option[Boolean] = {
    (f1.eval(r), f2.eval(r)) match {
      case (Some(b1), Some(b2)) => Some(b1 && b2)
      case _ => None
    }
  }
}

/**
 * <p>Or case class for the FilterCond typeclass in order
 * to compute the logical `||` operator on two conditions.</p>
 *
 * @param f1 first filter condition.
 * @param f2 second filter condition.
 */
case class Or(f1: FilterCond, f2: FilterCond) extends FilterCond {
  override def eval(r: Row): Option[Boolean] = {
    (f1.eval(r), f2.eval(r)) match {
      case (Some(b1), Some(b2)) => Some(b1 || b2)
      case _ => None
    }
  }
}

/**
 * <p>Typeclass defining a query operation on a tabular.</p>
 */
trait Query {

  /**
   * <p>Evaluate the query to a table, if all queries evaluated
   * successfully. </p>
   *
   * @return Some(table) if all queries evaluated, or `None` otherwise.
   */
  def eval: Option[Table]
}

/**
 * <p>Atom query which evaluates to the input table.
 * Always succeeds and gets evaluated.</p>
 *
 * @param tab table class instance.
 */
case class Value(tab: Table) extends Query {
  override def eval: Option[Table] = Some(tab)
}

/**
 * <p>Selects certain columns from the result of a target query.
 * Fails with `None` if some rows are not present in the resulting table.</p>
 *
 * @param columns the list of column names to get selected.
 * @param target eagerly execute the nested query and then the current one.
 */
case class Select(columns: Line, target: Query) extends Query {
  override def eval: Option[Table] = {
    target.eval match {
      case Some(tab) => tab.select(columns)
      case None => None
    }
  }
}

/**
 * <p>Filters rows from the result of the target query.
 * Success depends only on the success of the target.</p>
 *
 * @param condition FilterCond typeclass defining a query condition.
 * @param target eagerly execute the nested query and then the current one.
 */
case class Filter(condition: FilterCond, target: Query) extends Query {
  override def eval: Option[Table] = {
    target.eval match {
      case Some(tab) => tab.filter(condition)
      case None => None
    }
  }
}

/**
 * <p>Creates a new column with default values.
 * Success depends only on the success of the target.</p>
 *
 * @param name String defining the new column name.
 * @param defaultVal The default value assigned to each cell.
 * @param target eagerly execute the nested query and then the current one.
 */
case class NewCol(name: String, defaultVal: String, target: Query) extends Query {
  override def eval: Option[Table] = {
    target.eval match {
      case Some(tab) => Some(tab.newCol(name, defaultVal))
      case None => None
    }
  }
}

/**
 * <p>Combines two tables based on a common key.
 * Success depends on whether the key exists in both
 * tables or not AND on the success of the target.</p>
 *
 * @param key String value to merge the tables.
 * @param t1 first instance of the Table class.
 * @param t2 second instance of the Table class.
 */
case class Merge(key: String, t1: Query, t2: Query) extends Query {
  override def eval: Option[Table] = {
    (t1.eval, t2.eval) match {
      case (Some(tab1), Some(tab2)) => tab1.merge(key, tab2)
      case _ => None
    }
  }
}

/**
 * <p>Table Class defining a table in a database format.</p>
 *
 * <p>The cells are represented as `columnNameXvalue` matrix.</p>
 *
 * @param columnNames name list of all fields.
 * @param tabular the matrix of values (fields cells).
 */
class Table (columnNames: Line, tabular: List[List[String]]) {

  /**
   * <p>Gets the fields name from the table.</p>
   *
   * @return List of Strings representing the fields name.
   */
  def getColumnNames : Line = columnNames

  /**
   * <p>Gets the value cells without fields name.</p>
   *
   * @return List of String Lists as cells.
   */
  def getTabular : List[List[String]] = tabular

  override def toString: String = columnNames.mkString(",") + "\n" + tabular.map(_.mkString(",")).mkString("\n")

  /**
   * <p>Selects a number of columns from the values cells.
   * Columns are selected via the input field names.</p>
   *
   * @param columns input field names to select from table.
   * @return Some(table) if any column was selected or None otherwise.
   */
  def select(columns: Line): Option[Table] = {
    val zippedTabular = tabular.transpose
        .zip(columnNames)
        .filter(tabPair => columns.contains(tabPair._2))

    if (zippedTabular.length <= 0)
      None
    else
      Option(Table(columns.filter(columnNames.contains), zippedTabular.map(_._1).transpose))
  }

  /**
   * <p>Zips every value cell to the corresponding field name.</p>
   */
  private val zipTabWithNames = (tabular: List[List[String]], columnNames: Line) =>
    tabular.map(line => line.zip(columnNames).map { case (k, v) => (v, k) }.toMap)

  /**
   * <p>Filters the rows of the table via a specified condition.</p>
   *
   * @param cond the condition to filter the table rows.
   * @return Some(table) with filtered rows or None if no rows
   *         were filtered.
   */
  def filter(cond: FilterCond): Option[Table] = {
    val filteredMapTabular = zipTabWithNames(tabular, columnNames)
        .filter(row =>
          cond.eval(row) match {
            case Some(b) => b
            case None => false
          }
        )

    if (filteredMapTabular.length <= 0)
      None
    else {
      Option(
        Table(
          columnNames,
          filteredMapTabular.map(
            line => columnNames.foldRight(Nil: List[String])((name, acc) => line(name) :: acc)
          )
        )
      )
    }
  }

  /**
   * <p>Inserts a new column in a table with a new name
   * sets the column to a specified default value.</p>
   *
   * @param name the name of the new field.
   * @param defaultVal the default value for the column.
   * @return A table with a new column or the same table
   *         if the field already existed in the table.
   */
  def newCol(name: String, defaultVal: String): Table = {
    if (columnNames.contains(name))
      this
    else
      Table(columnNames :+ name, tabular.map(_ :+ defaultVal))
  }

  /**
   * <p>Combines two maps structures, where collisions
   * are combined into one value separated by semicolons.</p>
   *
   * @param m1 first instance of a Map structure.
   * @param m2 second instance of a Map structure.
   * @return m1 ++ m2 where collisions are concatenated
   *         into a single String separated by semicolons.
   */
  private def combineMaps(m1: Map[String, String], m2: Map[String, String]): Map[String, String] = {
    m1 ++ m2.map { case (k, v) =>
      val newV = if (m1.contains(k) && !m1(k).contains(v)) m1(k) + ';' + v else v
      k -> newV
    }
  }

  /**
   * <p>Merges two tables after a key field and concatenates the collisions
   * as a single value cell. Fails if the key cannot be found in both
   * tables.</p>
   *
   * @param key field name in order to merge after.
   * @param other the other instance of the table class.
   * @return this ++ other operation if the key exists in both
   *         tables or None otherwise.
   */
  def merge(key: String, other: Table): Option[Table] = {
    if (!columnNames.contains(key) || !other.getColumnNames.contains(key))
      None
    else {
      val mergedNames = (columnNames ++ other.getColumnNames).distinct

      val mergedTabular = (zipTabWithNames(tabular, columnNames) ++ zipTabWithNames(other.getTabular, other.getColumnNames))
          .groupBy(pair => pair(key))
          .map { case (_, v) => v.foldLeft(Map.empty[String, String])((acc, el) => combineMaps(acc, el)) }
          .map(hashMap => mergedNames.foldRight(Nil: List[String])((name, acc) => {
            val value = if (hashMap.contains(name)) hashMap(name) else ""
            value :: acc
          }))
          .toList

      Option(Table(mergedNames, mergedTabular))
    }
  }
}

/**
 * <p>Companion Object for the Table class in order to define special constructors.</p>
 */
object Table {

  /**
   * <p>Default Empty constructor initializes the Table to
   * an empty table with no values or fields.</p>
   *
   * @return An empty table class instance.
   */
  def apply(): Table = new Table(List(), List(List()))

  /**
   * <p>Default constructor initializes the Table with
   * specified fields and value cells, no special checks
   * are applied.</p>
   *
   * <p>Should be called from another constructors scope.</p>
   *
   * @param columnNames list of field names.
   * @param tabular value cells.
   * @return a new Filled tables class instance.
   */
  private def apply(columnNames: Line, tabular: List[List[String]]): Table = {
    new Table(columnNames, tabular)
  }

  /**
   * <p>String constructor initializes a Table instance from a
   * valid String, no checks are run on the String in order to
   * check for validity. The String represents a full operable
   * table that follows the "toString" function from the Table
   * class.</p>
   *
   * @param s valid Table mapped to a String value.
   * @return a new instance of a filled Table from the source String.
   */
  def apply(s: String): Table = {
    val parsedString = s.split("\n")
    new Table(
      parsedString.head.split(",").toList,
      parsedString.tail.map(str => str.split(",|,$", -1).toList).toList
    )
  }
}
