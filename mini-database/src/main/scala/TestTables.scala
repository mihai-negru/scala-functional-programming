import scala.io.Source

object TestTables {
  val table1 : Table = new Table(
    List("col1", "col2"), List(
      List("a", "2"),
      List("b", "3"),
      List("c", "4"),
      List("d", "5")
    ))

  val table1String: String = {
    val src = Source.fromFile("tables/table1.csv")
    val str = src.mkString
    src.close()
    str.replace("\r", "")
  }

  val table2 : Table = {
    val src = Source.fromFile("tables/table2.csv")
    val str = src.mkString
    src.close()
    Table(str.replace("\r", ""))
  }

  val table3 : Table = {
    val src = Source.fromFile("tables/table3.csv")
    val str = src.mkString
    src.close()
    Table(str.replace("\r", ""))
  }

  val table4: Table = {
    val src = Source.fromFile("tables/table4.csv")
    val str = src.mkString
    src.close()
    Table(str.replace("\r", ""))
  }

  val table3_4_merged : Table = {
    val src = Source.fromFile("tables/table3_4_merged.csv")
    val str = src.mkString
    src.close()
    Table(str.replace("\r", ""))
  }

  val test3_newCol_Value : Table = {
    val src = Source.fromFile("tables/test_3_newCol_Value.csv")
    val str = src.mkString
    src.close()
    Table(str.replace("\r", ""))
  }

  val test3_Select_Value: Table = {
    val src = Source.fromFile("tables/test_3_Select_Value.csv")
    val str = src.mkString
    src.close()
    Table(str.replace("\r", ""))
  }

  val test3_Filter_Value: Table = {
    val src = Source.fromFile("tables/test_3_Filter_Value.csv")
    val str = src.mkString
    src.close()
    Table(str.replace("\r", ""))
  }

  val test3_Merge_Value: Table = {
    val src = Source.fromFile("tables/test_3_Merge_Value.csv")
    val str = src.mkString
    src.close()
    Table(str.replace("\r", ""))
  }

  val tableFunctional : Table = {
    val src = Source.fromFile("tables/Functional.csv")
    val str = src.mkString
    src.close()
    Table(str.replace("\r", ""))
  }

  val tableObjectOriented : Table = {
    val src = Source.fromFile("tables/Object-Oriented.csv")
    val str = src.mkString
    src.close()
    Table(str.replace("\r", ""))
  }

  private val tableImperative : Table = {
    val src = Source.fromFile("tables/Imperative.csv")
    val str = src.mkString
    src.close()
    Table(str.replace("\r", ""))
  }

  val ref_programmingLanguages1: Table = {
    val src = Source.fromFile("tables/test_3_1.csv")
    val str = src.mkString
    src.close()
    Table(str.replace("\r", ""))
  }

  val ref_programmingLanguages2: Table = {
    val src = Source.fromFile("tables/test_3_2.csv")
    val str = src.mkString
    src.close()
    Table(str.replace("\r", ""))
  }

  val ref_programmingLanguages3: Table = {
    val src = Source.fromFile("tables/test_3_3.csv")
    val str = src.mkString
    src.close()
    Table(str.replace("\r", ""))
  }

  val programmingLanguages1: Table = {
    val tableFunctionalUpdated = tableFunctional.newCol("Functional", "Yes")
    val tableObjectOrientedUpdated = tableObjectOriented.newCol("Object-Oriented", "Yes")
    val tableImperativeUpdated = tableImperative.newCol("Imperative", "Yes")

    tableFunctionalUpdated.merge("Language", tableObjectOrientedUpdated) match {
      case Some(tab) => tab.merge("Language", tableImperativeUpdated) match {
        case Some(tab) => tab
        case None => Table()
      }
      case None => Table()
    }
  }

  val programmingLanguages2: Table = {
    val condition =
      Field("Original purpose", _.contains("Application")) && Field("Other paradigms", _.contains("concurrent"))

    programmingLanguages1.filter(condition) match {
      case Some(tab) => tab
      case None => Table()
    }
  }

  val programmingLanguages3: Table = {
    programmingLanguages2.select(List("Language", "Object-Oriented", "Functional")) match {
      case Some(tab) => tab
      case None => Table()
    }
  }

}
