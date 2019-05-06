package towers.model.game_objects

import towers.model.physics.PhysicsVector

class Projectile(location: PhysicsVector,
                 velocity: PhysicsVector)
  extends PhysicalObject(location, velocity) {


  override def onGround():Unit={
    this.destroy()
  }

  override def collide(): Unit = {
    this.destroy()
  }

}
