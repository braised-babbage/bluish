package bluish
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import org.scalajs.dom
import dom.document
import org.scalajs.dom.html
import scala.util.Random



@JSExportTopLevel("ScalaJSExample")
object ScalaJSExample {


  def update(s: State, dt: Double, keys: KeyState): State = {
    val actors = s.actors.map(a => a.update(dt, s, keys))
    var newState = State(s.level, actors, s.status)

    if (newState.status != Playing)
      return newState

    val player = newState.player

    if (s.level.touches(player.pos, player.size, Lava))
      return State(s.level, actors, Lost)

    for (actor <- actors) {
      if (actor != player && actor.overlaps(player)) {
        newState = actor.collide(newState)
      }
    }
    newState
  }





  @JSExport
  def main(): Unit = {

    val dt = 20.0 // milliseconds
    val ui = new DomUI()


    var currentLevel = 0
    var state = State(null, null, Starting)
    var endTime = 1.0
    var intervalId = 0

    def run(): Unit = state.status match {
      case Starting => {
          endTime = 1.0
          ui.clear()
          val (level, actors) = Level.parse(Level.levels(currentLevel))
          state = State(level, actors, Playing)
          ui.draw(state)
      }
      case Playing => {
        state = update(state, dt/1000, ui.keys)
        ui.update(state)
        ui.scrollPlayerIntoView(state)
      }
      case Won => {
        currentLevel += 1
        if (currentLevel < Level.levels.length)
          state = State(null, null, Starting)
        else {
          println("You win!")
          dom.window.clearInterval(intervalId)
        }
      }
      case Lost => {
        if (endTime < 0)
          state = State(null, null, Starting)
        else
          endTime -= dt/1000
      }
    }
    intervalId = dom.window.setInterval(() => run, dt)
  }
}
