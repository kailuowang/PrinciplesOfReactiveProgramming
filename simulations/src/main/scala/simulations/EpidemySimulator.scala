package simulations

import math.random

class EpidemySimulator extends Simulator {
  type RoomLocation = (Int, Int)
  def randomBelow(i: Int) = (random * i).toInt

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8
    val prevalence: Int = 1
    val transmissibility: Int = 40
    val maxWait: Int = 5
    val sickAt: Int = 6
    val mortalityAt: Int = 14
    val immuneAt: Int = 16
    val recoverAt: Int = 18
    val mortalityRate: Int = 25
  }

  import SimConfig._

  val persons: List[Person] = Range(0, 300).map( i => new Person(i)).toList
  val initNumOfInfected = population * prevalence / 100
  persons.take(initNumOfInfected).foreach(_.getInfected())

  def adjacentRooms(room: RoomLocation): List[RoomLocation] = {
    def circularDimension(d: Int, size: Int) = if (d >= size) 0 else if (d < 0) size - 1 else d

    for( r <- Range(-1, 2).toList;  c <- Range(-1, 2).toList if math.abs(r) != math.abs(c) ) yield
      (circularDimension(room._1 + r, roomRows), circularDimension(room._2 + c, roomColumns))
  }

  def exists(room: RoomLocation)(selector: Person => Boolean) = personsInRoom(room).exists(selector)

  def personsInRoom(room: RoomLocation): List[Person] = persons.filter(_.room == room)


  class Person (val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false


    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    nextMove

    def nextMove() {
      afterDelay(randomBelow(maxWait) + 1) { move() }
    }

    def move() {
      if(!dead) {
        val safeAdjacentRooms = adjacentRooms(room).filterNot(exists(_)(_.sick))
        if(!safeAdjacentRooms.isEmpty){
          val dest = safeAdjacentRooms(randomBelow(safeAdjacentRooms.size))
          row = dest._1
          col = dest._2
        }
        onArrive()
        nextMove()
      }
    }

    def onArrive() {
      if(exists(room)(_.infected) && randomChance(transmissibility) && !infected && !immune ) getInfected()
    }

    def getInfected() {
      infected = true
      afterDelay(sickAt) { sick = true }
      afterDelay(mortalityAt) {
        if(randomChance(mortalityRate)) dead = true
      }
      afterDelay(immuneAt) {
        if(!dead) {
          immune = true
          sick = false

        }
      }
      afterDelay(recoverAt) {
        if(!dead){
          immune = false
          infected = false
        }
      }
    }

    def room: RoomLocation = (row, col)

    def randomChance(percentagePossibility: Int) : Boolean = randomBelow(101) <= percentagePossibility

  }
}
