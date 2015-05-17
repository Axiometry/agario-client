package me.axiometry.agario

case class Cell(override val world: World) extends Entity {
  override var x: Double = _
  override var y: Double = _
}