import scalafx.application.JFXApp3.PrimaryStage
import scalafx.application.{JFXApp3, Platform}
import scalafx.beans.property.ObjectProperty
import scalafx.scene.Scene
import javafx.scene.input.KeyCode
import scalafx.scene.paint.Color._
import scalafx.scene.shape.Rectangle
import scalafx.scene.text.{Text, Font}
import scalafx.scene.SceneIncludes.jfxScene2sfx

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Random

object Main extends JFXApp3 {
  val windowSize = 600
  val cellSize = 10
  val numberOfPredators = 5

  type Coordinates = (Int, Int)
  case class Predator(location: Coordinates)
  case class Prey(location: Coordinates)
  case class State(predators: List[Predator], prey: Prey)

  override def start(): Unit = {
    val initialState = ObjectProperty(
      State(
        (1 to numberOfPredators).map(_ => Predator((Random.nextInt(windowSize - cellSize), Random.nextInt(windowSize - cellSize)))).toList,
        Prey((300, 300))
      )
    )
    stage = new PrimaryStage {
      title = "Predator Prey Game"
      width = windowSize
      height = windowSize
      scene = new Scene {
        fill = LightBlue
        content = drawState(initialState.value)

        onKeyPressed = { key =>
          val currentState = initialState.value
          key.getCode match {
            case KeyCode.UP => initialState.value = currentState.copy(prey = Prey((currentState.prey.location._1, (currentState.prey.location._2 - cellSize) max 0)))
            case KeyCode.DOWN => initialState.value = currentState.copy(prey = Prey((currentState.prey.location._1, (currentState.prey.location._2 + cellSize) min (windowSize - cellSize))))
            case KeyCode.RIGHT => initialState.value = currentState.copy(prey = Prey(((currentState.prey.location._1 + cellSize) min (windowSize - cellSize), currentState.prey.location._2)))
            case KeyCode.LEFT => initialState.value = currentState.copy(prey = Prey(((currentState.prey.location._1 - cellSize) max 0, currentState.prey.location._2)))
            case _ => ()
          }
          content = drawState(initialState.value)
        }
      }
    }

    gameLoop { () =>
      val currentState = initialState.value
      initialState.value = updateGame(currentState)
      Platform.runLater {
        stage.getScene.content = drawState(initialState.value)
      }
    }
  }

  def gameLoop(update: () => Unit): Unit = Future {
    update()
    Thread.sleep(100)
    gameLoop(update)
  }

  def updateGame(state: State): State = {
    val updatedPredators = movePredators(state.predators, state.prey)
    val updatedPrey = state.prey

    val isGameOver = state.predators.exists { predator =>
      predator.location == state.prey.location
    }

    if (isGameOver) {
      stage.getScene.setFill(Black)
      val gameOverText = new Text("Game Over") {
        fill = Red
        font = Font("Arial", 48)
      }
      val textBounds = gameOverText.getBoundsInLocal
      gameOverText.layoutX = (windowSize - textBounds.getWidth) / 2
      gameOverText.layoutY = (windowSize - textBounds.getHeight) / 2

      val gameOverGroup = new javafx.scene.Group(gameOverText)
      stage.getScene.setRoot(gameOverGroup)
    }

    state.copy(predators = updatedPredators, prey = updatedPrey)
  }

  def movePredators(predators: List[Predator], prey: Prey): List[Predator] = {
    predators.map { predator =>
      val predatorLocation = predator.location
      val preyLocation = prey.location

      val newX = if (preyLocation._1 > predatorLocation._1) predatorLocation._1 + 1 else if (preyLocation._1 < predatorLocation._1) predatorLocation._1 - 1 else predatorLocation._1
      val newY = if (preyLocation._2 > predatorLocation._2) predatorLocation._2 + 1 else if (preyLocation._2 < predatorLocation._2) predatorLocation._2 - 1 else predatorLocation._2

      val boundedX = newX.max(0).min(windowSize - cellSize)
      val boundedY = newY.max(0).min(windowSize - cellSize)

      predator.copy(location = (boundedX, boundedY))
    }
  }

  def drawState(state: State): List[Rectangle] = {
    val prey = new Rectangle {
      x = state.prey.location._1
      y = state.prey.location._2
      width = cellSize
      height = cellSize
      fill = Blue
    }

    val predators = state.predators.map { predator =>
      new Rectangle {
        x = predator.location._1
        y = predator.location._2
        width = cellSize
        height = cellSize
        fill = Red
      }
    }

    prey :: predators
  }
}
