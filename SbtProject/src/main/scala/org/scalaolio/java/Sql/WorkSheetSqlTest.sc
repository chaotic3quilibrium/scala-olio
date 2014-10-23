import org.scalaolio.java.Sql
import org.scalaolio.java.Sql.{ResultSetReadOnlyRow, DatabaseAccessUrl}
import scala.util.{Try, Success}
val q = 1
//
val databaseAccessUrl =
  new DatabaseAccessUrl(
      "org.h2.Driver"
    , "jdbc:h2:tcp://localhost/~/test"
    , "sa"
    , "sa"
  )
case class AliMain(
    val id: Int
  , val aliValue: String
)
val converter: ResultSetReadOnlyRow => Try[AliMain] =
  (resultSetRow) => {
    Success(
      AliMain(
          resultSetRow.getInt(1)
        , resultSetRow.getString(2)
      )
    )
  }
val sql = "SELECT id, ali_value FROM ali_main "
val list = Sql.select(() => databaseAccessUrl.getConnection, sql, converter)
