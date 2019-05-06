package towers.model.ai

import towers.model.GridLocation
import towers.model.physics.PhysicsVector

class AIPlayer(id: String) {

  def computeMovement(jsonGameState: String): PhysicsVector = {
    // Do not edit this method. It will not be called during grading
    var path = getPath(jsonGameState)
    path = smoothPath(jsonGameState, path)
    pathToDirection(jsonGameState, path)
  }

  def getPath(jsonGameState: String): List[GridLocation] = {
    // TODO
    List()
  }

  def pathToDirection(jsonGameState: String, path: List[GridLocation]): PhysicsVector = {
    // TODO
    new PhysicsVector(Math.random() - 0.5, Math.random() - 0.5)
  }


  def smoothPath(jsonGameState: String, path: List[GridLocation]): List[GridLocation] = {
    // TODO
    path
  }

}
