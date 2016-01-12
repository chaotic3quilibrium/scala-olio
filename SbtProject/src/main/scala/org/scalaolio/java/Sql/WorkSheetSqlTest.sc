import org.scalaolio.java.Sql.{ResultSetRowReadOnly, DatabaseAccessUrl, Select}
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
    id: Int
  , aliValue: String
)
val converter: ResultSetRowReadOnly => Try[Option[AliMain]] =
  (resultSetRow) => {
    Success(
      Some(
        AliMain(
            resultSetRow.getInt(1)
          , resultSetRow.getString(2)
        )
      )
    )
  }
val sql = "SELECT id, ali_value FROM ali_main"
val select =
  new Select[AliMain](
      databaseAccessUrl
    , sql
    , converter
  ).content
