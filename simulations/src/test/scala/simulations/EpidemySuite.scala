package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class EpidemySuite extends FunSuite {

  test("population"){
    val es = new EpidemySimulator
    assert(es.persons.size == es.SimConfig.population)
  }

  test("prevalence rate"){
    val prevalenceRate = 0.01

    val es = new EpidemySimulator
    val numInfected = es.persons.count(_.infected)

    assert(numInfected == es.SimConfig.population * prevalenceRate,
      "prevalence rate should be 0.01"
      )
  }

  test("dead person stays dead"){
    val es = new EpidemySimulator

    val chosenOne = es.persons.head
    chosenOne.infected = true
    chosenOne.sick = true
    chosenOne.dead = true
    chosenOne.immune = false

    val(row, col) = (chosenOne.row, chosenOne.col)

    val testDays = 100

    while(!es.agenda.isEmpty && es.agenda.head.time < testDays){
      es.next

      assert(chosenOne.dead == true, "Dead person should keep dead state")
      assert(chosenOne.infected == true, "Dead person keeps infected")
      assert(chosenOne.immune == false, "Dead person cannot become immune")
      assert(chosenOne.sick == true, "Dead person keeps sick")
      assert(chosenOne.col == col && chosenOne.row == row, "Dead person cannot move")
    }
  }

  test("nobody moves into a room with dead person"){
    val es = new EpidemySimulator

    val chosenOne = es.persons.head
    chosenOne.infected = true
    chosenOne.sick = true
    chosenOne.dead = true
    chosenOne.immune = false

    val originalPersons = es.personsInRoom(chosenOne.room)

    val testDays = 100

    while(!es.agenda.isEmpty && es.agenda.head.time < testDays){
      es.next

      assert(es.personsInRoom(chosenOne.room).forall(originalPersons.contains(_)), "all persons in the room was there at the beginning")
    }
  }

  test("adjacentRooms") {
    val es = new EpidemySimulator
    assert(es.adjacentRooms(1,2).size == 4, es.adjacentRooms(1,2).toString)
    assert(es.adjacentRooms(1,2).contains(1,3))
    assert(es.adjacentRooms(1,2).contains(1,1))
    assert(es.adjacentRooms(1,2).contains(0,2))
    assert(es.adjacentRooms(1,2).contains(2,2))

    assert(es.adjacentRooms(0,2).contains(7,2))
    assert(es.adjacentRooms(7,5).contains(0,5))
    assert(es.adjacentRooms(3,7).contains(3,0))
    assert(es.adjacentRooms(6,0).contains(6,7))

    assert(es.adjacentRooms(0,0).contains(7,0))
    assert(es.adjacentRooms(7,7).contains(0,7))
  }



  test("life cycle"){
    val es = new EpidemySimulator

    val incubationTime = 6
    val dieTime = 14
    val immuneTime = 16
    val healTime = 18

    val prevalenceRate = 0.01
    val transRate = 0.4
    val dieRate = 0.25

    val infectedPerson = (es.persons.find{_.infected}).get

    //before incubation time
    while(es.agenda.head.time < incubationTime){
      assert(infectedPerson.infected == true, "Infected person keeps infected in 6 days")
    	assert(infectedPerson.sick == false, "Infected person does not get sick in 6 days")
    	assert(infectedPerson.immune == false, "Infected person cannot become immune in 6 days")
    	assert(infectedPerson.dead == false, "Infected person does not die in 6 days")
    	es.next
    }

    //incubation time has passed, there should be an event for getting sick
    assert(es.agenda.head.time == incubationTime, "You should set a 'sick' event after incubation time")
    while(es.agenda.head.time == incubationTime) es.next
    assert(infectedPerson.sick == true, "Infected person should become sick after 6 days")

    //wait for dieTime
    while(es.agenda.head.time < dieTime){
    	assert(infectedPerson.infected == true, "Sick person keeps infected")
    	assert(infectedPerson.sick == true, "Sick person keeps sick before turning immune")
    	assert(infectedPerson.immune == false, "Sick person is not immune")
    	assert(infectedPerson.dead == false, "Sick person does not die before 14 infected days")
    	es.next
    }

    assert(es.agenda.head.time == dieTime, "You should set a 'die' event (decides with a probability 25% whether the person dies) after 14 days")
    while(es.agenda.head.time == dieTime) es.next
  }


  test("transmissibility rate"){
	  var infectedTimes = 0
	  for(i <- 0 to 100){
		  val es = new EpidemySimulator
		  val healthyPerson = (es.persons find {p => !p.infected}).get
		  es.persons.filter(p => p != healthyPerson) foreach {_.infected = true}

      while(es.agenda.head.time < 6) es.next

      infectedTimes = infectedTimes + (if(healthyPerson.infected) 1 else 0)
	  }
	  assert(infectedTimes > 0, "A person should get infected according to the transmissibility rate when he moves into a room with an infectious person")
  }
}
