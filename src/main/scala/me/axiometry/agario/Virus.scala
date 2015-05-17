package me.axiometry.agario

case class Virus(override val world: World) extends Entity {
  override var x: Double = _
  override var y: Double = _
}