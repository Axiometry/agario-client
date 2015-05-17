package me.axiometry.agario

trait Entity {
  def world: World

  def x: Double
  def x_=(x: Double): Unit

  def y: Double
  def y_=(y: Double): Unit
}