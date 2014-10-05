package simulations

import simulations.gui.EpidemyDisplay.Room

import math.random
import scala.util.Random

class EpidemySimulator extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8
    val cells = roomRows * roomColumns
    val possibleDirections = Seq(
      Direction(1, 0),
      Direction(0, 1),
      Direction(-1, 0),
      Direction(0, -1)
    )
    val prevalenceRate = 0.01
    val transmissionRate = 1
  }

  import SimConfig._

  val persons : List[Person] = (0 until population).toList.map(new Person(_))

  Random.shuffle(persons).take((population * prevalenceRate).toInt).foreach(_.infect())

  persons.foreach(p => p.scheduleMove())

  case class Direction(rows: Int, cols: Int)

  case class Room (r: Int, c: Int) {
    def neighbors = possibleDirections.map (direction => this + direction)

    def +(d : Direction) = Room(
      (r + d.rows + roomRows) % roomRows,
      (c + d.cols + roomColumns) % roomColumns)

    def people() = {
      persons.filter(_.room == this)
    }

    def hasInfected = people.filter { _.infected }.size > 0
    def visiblyInfectious = {
      people().filter(p => p.sick || p.dead).size > 0
    }
  }

  class Person (val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    var room = Room(col, row)

    def whereCanIMove() : Seq[Room] =
      room.neighbors.filter(r => !r.visiblyInfectious)

    def infect() = {
      if (!infected && !immune) {
        infected = true

        afterDelay(6) {
          // visibly sick
          sick = true
        }

        afterDelay(14) {
          // chance to die
          dead = randomBelow(4) == 0
        }

        afterDelay(16) {
          if (!dead) {
            // become immune, and not visibly infected
            immune = true
            sick = false
          }
        }

        afterDelay(20) {
          if (!dead) {
            // reset
            sick = false

            infected = false
          }
        }
      }
    }

    def move() = {
      if (!dead) {
        room = whereCanIMove match {
          case List() => room
          case x => x(randomBelow(x.size))
        }

        if (room.hasInfected && randomBelow(100) < transmissionRate * 100) {
          infect()
        }

        scheduleMove()
      }
    }

    def scheduleMove() : Unit = afterDelay(randomBelow(5) + 1) {
      move()
    }
  }
}
