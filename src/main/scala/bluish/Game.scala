package bluish

sealed trait GameStatus
object Starting extends GameStatus
object Playing extends GameStatus
object Won extends GameStatus
object Lost extends GameStatus

case class State(level: Level, actors: Seq[Actor], status: GameStatus) {
  def player: Actor = {
    actors.filter({
      case p: Player => true
      case _ => false
    }).head
  }
}
