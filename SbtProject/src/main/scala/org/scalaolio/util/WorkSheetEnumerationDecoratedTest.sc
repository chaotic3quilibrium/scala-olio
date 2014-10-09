val q = 1
//
import scala.reflect.runtime.universe.{TypeTag,typeTag}

import org.scalaolio.util.EnumerationDecorated

object ChessPiecesEnhancedDecorated extends EnumerationDecorated {
  //A. Enumerated values
  case object KING extends Member
  case object QUEEN extends Member
  case object BISHOP extends Member
  case object KNIGHT extends Member
  case object ROOK extends Member
  case object PAWN extends Member

  //B. Defining the associated data
  val decorationOrderedSet: List[Decoration] =
    List(
        Decoration(KING,   'K', 0)
      , Decoration(QUEEN,  'Q', 9)
      , Decoration(BISHOP, 'B', 3)
      , Decoration(KNIGHT, 'N', 3)
      , Decoration(ROOK,   'R', 5)
      , Decoration(PAWN,   'P', 1)
    )

  //C. Safe place to extend any/all needed data for a specific Member; i.e use all the vals you like
  final case class Decoration private[ChessPiecesEnhancedDecorated] (
      member: Member
    , char: Char
    , pointValue: Int
  ) extends DecorationBase {
    val description: String = member.name.toLowerCase.capitalize
  }

  //D. from Enumeration [item L]
  override def typeTagMember: TypeTag[_] = typeTag[Member]

  //E. from EnumerationDecoration [item C], required type extension
  sealed trait Member extends MemberDecorated
}
//
//F. Simple test for exhaustive pattern matching
//   - just comment out any of the case entries
object ChessPiecesEnhancedDecoratedTestExhaustivePatternMatch {
  def stringMe(member: ChessPiecesEnhancedDecorated.Member): String =
    member match {
      case ChessPiecesEnhancedDecorated.KING => "Of the World"
      case ChessPiecesEnhancedDecorated.QUEEN => "Of the World"
      case ChessPiecesEnhancedDecorated.BISHOP => "Of the Church"
      case ChessPiecesEnhancedDecorated.KNIGHT => "Of the Church"
      case ChessPiecesEnhancedDecorated.ROOK => "Of the Castle"
      case ChessPiecesEnhancedDecorated.PAWN => "Of the Field"
    }
}

