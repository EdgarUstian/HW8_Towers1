package towers.model.game_objects

import play.api.libs.json.{JsValue, Json}
import towers.model.genetics.genes.Gene
import towers.model.physics.PhysicsVector

class DodgeBallTower(val x: Int, val y: Int) extends GameObject {

  // The height at which projectiles are fired
  val height = 3.0

  // Towers can only fire at players closer than this distance from the tower
  val sightRange = 5.0

  // The magnitude of the velocity at which projectiles are fired
  val projectileVelocity = 5.0


  def fire(jsonGameState: String): List[Projectile] = {
    var closestLocation: PhysicsVector = null
    // TODO: Objective 2
    val parsed:JsValue = Json.parse(jsonGameState)  // Parse jsonGameState
    val players: List[Map[String, JsValue]] = (parsed \ "players").as[List[Map[String, JsValue]]] // Retrieve only the list of players (maps)
    val towerLocation = new PhysicsVector(this.x + 0.5, this.y + 0.5, this.height) //Location of Tower
    val sorted = players.sortWith((a: Map[String, JsValue], b: Map[String, JsValue]) =>
      towerLocation.distance2d(new PhysicsVector(a("x").as[Double], a("y").as[Double])) < towerLocation.distance2d(new PhysicsVector(b("x").as[Double], b("y").as[Double])))
    val closeBoi = sorted.head
    closestLocation = new PhysicsVector(closeBoi("x").as[Double], closeBoi("y").as[Double])
    var projectileList: List[Projectile] = List()
    if (towerLocation.distance2d(closestLocation) < sightRange){
      val deltaX = towerLocation.x - closestLocation.x
      val deltaY = towerLocation.y - closestLocation.y
      var angle = Math.atan(deltaY/deltaX)
      if (deltaX > 0){
        angle += Math.PI
      }
//      println(List(new Projectile(towerLocation, new PhysicsVector(Math.cos(angle) * projectileVelocity, Math.sin(angle) * projectileVelocity))))
      projectileList = projectileList :+ new Projectile(towerLocation, new PhysicsVector(Math.cos(angle) * projectileVelocity, Math.sin(angle) * projectileVelocity))
    }
    projectileList
  }


  def aimFire(jsonGameState: String): List[Projectile] = {
    // TODO: Bonus Objective
    var closestLocation: PhysicsVector = null
    var closestVelocity: PhysicsVector = null
    val parsed:JsValue = Json.parse(jsonGameState)  // Parse jsonGameState
    val players: List[Map[String, JsValue]] = (parsed \ "players").as[List[Map[String, JsValue]]] // Retrieve only the list of players (maps)
    val towerLocation = new PhysicsVector(this.x + 0.5, this.y + 0.5, this.height) //Location of Tower
    val sorted = players.sortWith((a: Map[String, JsValue], b: Map[String, JsValue]) =>
      towerLocation.distance2d(new PhysicsVector(a("x").as[Double], a("y").as[Double])) <
        towerLocation.distance2d(new PhysicsVector(b("x").as[Double], b("y").as[Double])))
    val closeBoi = sorted.head
    closestLocation = new PhysicsVector(closeBoi("x").as[Double], closeBoi("y").as[Double]) //Position of the closest player
    closestVelocity = new PhysicsVector(closeBoi("v_x").as[Double], closeBoi("v_y").as[Double]) //Velocity of the closest player

    val a = Math.pow(closestVelocity.x, 2.0) + Math.pow(closestVelocity.y, 2.0) - Math.pow(projectileVelocity, 2.0)
    val b = 2 * ((closestVelocity.x * (closestLocation.x - towerLocation.x)) + (closestVelocity.y * (closestLocation.y - towerLocation.y)))
    val c = Math.pow(closestLocation.x - towerLocation.x, 2.0) + Math.pow(closestLocation.y - towerLocation.y, 2.0)

    val p = -b / (2 * a)
    val q = Math.sqrt(Math.pow(b, 2.0) - 4 * a * c) / (2 * a)

    var t = 0.0
    val t1 = p - q
    val t2 = p + q

    if (t1 > t2 && t2 > 0){
      t = t2
    } else {
      t = t1
    }

    val aimLocation = new PhysicsVector(t * closestVelocity.x + closestLocation.x, t * closestVelocity.y + closestLocation.y)

//    println(closestLocation, aimLocation)

    var projectileList: List[Projectile] = List()
    if (towerLocation.distance2d(closestLocation) < sightRange){
      val deltaX = towerLocation.x - aimLocation.x
      val deltaY = towerLocation.y - aimLocation.y
      var angle = Math.atan(deltaY/deltaX)
      if (deltaX > 0){
        angle += Math.PI
      }
      //      println(List(new Projectile(towerLocation, new PhysicsVector(Math.cos(angle) * projectileVelocity, Math.sin(angle) * projectileVelocity))))
      projectileList = projectileList :+ new Projectile(towerLocation, new PhysicsVector(Math.cos(angle) * projectileVelocity, Math.sin(angle) * projectileVelocity))
    }
    projectileList
  }


  // Suggested Genetic Algorithm setup
  def getFitnessFunction(targetPlayer: Player): PhysicsVector => Double = {
    null
  }

  def vectorIncubator(genes: List[Gene]): PhysicsVector = {
    null
  }

}
