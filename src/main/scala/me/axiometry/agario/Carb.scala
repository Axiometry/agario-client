package me.axiometry.agario

case class Carb(override val world: World) extends Entity {
  override var x: Double = _
  override var y: Double = _
}