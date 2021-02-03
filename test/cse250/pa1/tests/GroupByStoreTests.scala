/**
 * cse250.pa1.tests.GroupByStoreTests.scala
 *
 * Copyright 2020 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 * Submission author
 * UBIT: chihotam
 * Person#: 50301678
 *
 * Collaborators (include UBIT name of each, comma separated):
 * UBIT:
 */
package cse250.pa1.tests

import cse250.objects.{AssessmentUtilities, DNode, TaxParcel}
import cse250.pa1.objects.GroupByStore
import org.scalatest.{BeforeAndAfter, FlatSpec}

import scala.collection.mutable

class GroupByStoreTests extends FlatSpec with BeforeAndAfter {
  var dataStore: GroupByStore = _

  // This code is run prior to every test.
  before {
    dataStore = new GroupByStore
  }

  // Your tests for problem 1 should be contained under this header.
  behavior of "GroupByStore.invariants 1(a)"
  it should "insert to the head of a non-defined linked list" in {
    dataStore._groupingAttribute = "STREET"
    val tax1 = new TaxParcel
    tax1.parcelInfo += ("STREET" -> "HERTEL")
    val dnode1 = new DNode[TaxParcel](tax1,null,null)
    dataStore.insert(tax1)
    assert(dataStore._groupings(0)._value.equals(dnode1._value))
  }

  it should "insert to the head of a defined linked list" in {
    dataStore._groupingAttribute = "STREET"
    val tax1 = new TaxParcel
    tax1.parcelInfo += ("STREET" -> "MAIN")
    tax1.parcelInfo += ("STATE" -> "NJ")
    val dnode1 = new DNode[TaxParcel](tax1,null,null)
    dataStore.insert(tax1)
    val tax2 = new TaxParcel
    tax2.parcelInfo += ("STREET" -> "MAIN")
    tax2.parcelInfo += ("STATE" -> "NY")
    val dnode2 = new DNode[TaxParcel](tax2,null,null)
    dataStore.insert(tax2)
    val tax3 = new TaxParcel
    tax3.parcelInfo += ("STREET" -> "MAIN")
    tax3.parcelInfo += ("STATE" -> "NY")
    val dnode3 = new DNode[TaxParcel](tax3,null,null)
    dataStore.insert(tax3)
    assert(dataStore._groupings(0)._value.equals(dnode2._value) && dataStore._groupings(0)._next._value.equals(dnode1._value) && dataStore._groupings(0)._next._next == null)
  }

  it should "check if sorted" in {
    dataStore._groupingAttribute = "STREET"
    val tax1 = new TaxParcel
    tax1.parcelInfo += ("KEY" -> "HI")
    tax1.parcelInfo += ("STREET" -> "D")
    tax1.parcelInfo += ("HM" -> "YES")
    val dnode1 = new DNode[TaxParcel](tax1,null,null)
    dataStore.insert(tax1)
    val tax2 = new TaxParcel
    tax2.parcelInfo += ("KEY" -> "HI")
    tax2.parcelInfo += ("STREET" -> "s")
    tax2.parcelInfo += ("HM" -> "YES")
    val dnode2 = new DNode[TaxParcel](tax2,null,null)
    dataStore.insert(tax2)
    val tax3 = new TaxParcel
    tax3.parcelInfo += ("KEY" -> "HI")
    tax3.parcelInfo += ("STREET" -> "W")
    tax3.parcelInfo += ("HM" -> "NO")
    val dnode3 = new DNode[TaxParcel](tax3,null,null)
    dataStore.insert(tax3)
    val tax4 = new TaxParcel
    tax4.parcelInfo += ("KEY" -> "BYE")
    tax4.parcelInfo += ("STREET" -> "a")
    tax4.parcelInfo += ("HM" -> "NO")
    val dnode4 = new DNode[TaxParcel](tax4,null,null)
    dataStore.insert(tax4)
    val tax5 = new TaxParcel
    tax5.parcelInfo += ("KEY" -> "BYE")
    tax5.parcelInfo += ("STREET" -> "g")
    tax5.parcelInfo += ("HM" -> "YES")
    val dnode5 = new DNode[TaxParcel](tax5,null,null)
    dataStore.insert(tax5)
    val tax6 = new TaxParcel
    tax6.parcelInfo += ("KEY" -> "HI")
    tax6.parcelInfo += ("STREET" -> "q")
    tax6.parcelInfo += ("HM" -> "YES")
    val dnode6 = new DNode[TaxParcel](tax6,null,null)
    dataStore.insert(tax6)
    val tax9 = new TaxParcel
    tax9.parcelInfo += ("KEY" -> "HI")
    tax9.parcelInfo += ("STREET" -> "q")
    tax9.parcelInfo += ("HM" -> "YES")
    val dnode9 = new DNode[TaxParcel](tax9,null,null)
    dataStore.insert(tax9)
    val tax7 = new TaxParcel
    tax7.parcelInfo += ("KEY" -> "HI")
    tax7.parcelInfo += ("STREET" -> "o")
    tax7.parcelInfo += ("HM" -> "YES")
    val dnode7 = new DNode[TaxParcel](tax7,null,null)
    dataStore.insert(tax7)
    val tax8 = new TaxParcel
    tax8.parcelInfo += ("KEY" -> "HI")
    tax8.parcelInfo += ("STREET" -> "D")
    tax8.parcelInfo += ("HM" -> "NO")
    val dnode8 = new DNode[TaxParcel](tax8,null,null)
    dataStore.insert(tax8)
    println("STREET")
    for(x <- dataStore._groupings){
      println(x._value.parcelInfo)
    }
    /*assert(dataStore._groupings(0)._value.equals(dnode1._value))
    assert(dataStore._groupings(0)._next == null)
    assert(dataStore._groupings(1)._value.equals(dnode3._value))
    assert(dataStore._groupings(2)._value.equals(dnode4._value))
    assert(dataStore._groupings(3)._value.equals(dnode5._value))
    assert(dataStore._groupings(4)._value.equals(dnode7._value))
    assert(dataStore._groupings(5)._value.equals(dnode6._value))
    assert(dataStore._groupings(5)._next == null)
    assert(dataStore._groupings(6)._value.equals(dnode2._value))*/
    dataStore.regroup("KEY")
    println()
    println("KEY")
    var x = dataStore._groupings(0)
    var y = dataStore._groupings(1)
    while(x != null){
      println(x._value.parcelInfo)
      x = x._next
    }
    while(y != null){
      println(y._value.parcelInfo)
      y = y._next
    }
    val tax10 = new TaxParcel
    tax10.parcelInfo += ("KEY" -> "BYE")
    tax10.parcelInfo += ("STREET" -> "K")
    tax10.parcelInfo += ("HM" -> "NO")
    val dnode10 = new DNode[TaxParcel](tax9,null,null)
    dataStore.insert(tax10)
    val tax11 = new TaxParcel
    tax11.parcelInfo += ("KEY" -> "BYE")
    tax11.parcelInfo += ("STREET" -> "a")
    tax11.parcelInfo += ("HM" -> "NO")
    val dnode11 = new DNode[TaxParcel](tax11,null,null)
    dataStore.insert(tax11)
    println()
    println("KEY")
    var x1 = dataStore._groupings(0)
    var y2 = dataStore._groupings(1)
    while(x1 != null){
      println(x1._value.parcelInfo)
      if(x1._value.equals(tax4)){
        println(x1._next)
      }
      x1 = x1._next
    }
    while(y2 != null){
      println(y2._value.parcelInfo)
      y2 = y2._next
    }
    dataStore.regroup("HM")
    println()
    println("HM")
    var x3 = dataStore._groupings(0)
    var y4 = dataStore._groupings(1)
    while(x3 != null){
      println(x3._value.parcelInfo)
      x3 = x3._next
    }
    while(y4 != null){
      println(y4._value.parcelInfo)
      y4 = y4._next
    }
  }

  behavior of "GroupByStore.invariants 1(b)"
  it should "after inserting check if arraybuffer is sorted" in {
    dataStore._groupingAttribute = "STREET"
    val tax1 = new TaxParcel
    tax1.parcelInfo += ("STREET" -> "UNIVERSITY")
    val dnode1 = new DNode[TaxParcel](tax1,null,null)
    dataStore.insert(tax1)
    val tax2 = new TaxParcel
    tax2.parcelInfo += ("STREET" -> "MAIN")
    val dnode2 = new DNode[TaxParcel](tax2,null,null)
    dataStore.insert(tax2)
    val tax3 = new TaxParcel
    tax3.parcelInfo += ("STREET" -> "HERTEL")
    val dnode3 = new DNode[TaxParcel](tax3,null,null)
    dataStore.insert(tax3)
    assert(dataStore._groupings(0)._value.equals(dnode3._value) && dataStore._groupings(1)._value.equals(dnode2._value) && dataStore._groupings(2)._value.equals(dnode1._value))
  }
  
  behavior of "GroupByStore.invariants 1(c)"
  it should "after regroup check if arraybuffer is sorted" in {
    dataStore._groupingAttribute = "STREET"
    val tax1 = new TaxParcel
    tax1.parcelInfo += ("STREET" -> "UNIVERSITY")
    tax1.parcelInfo += ("STATE" -> "CALIF")
    val dnode1 = new DNode[TaxParcel](tax1,null,null)
    dataStore.insert(tax1)
    val tax2 = new TaxParcel
    tax2.parcelInfo += ("STREET" -> "MAIN")
    tax2.parcelInfo += ("STATE" -> "NY")
    val dnode2 = new DNode[TaxParcel](tax2,null,null)
    dataStore.insert(tax2)
    val tax3 = new TaxParcel
    tax3.parcelInfo += ("STREET" -> "HERTEL")
    tax3.parcelInfo += ("STATE" -> "TX")
    val dnode3 = new DNode[TaxParcel](tax3,null,null)
    dataStore.insert(tax3)
    assert(dataStore._groupings(0)._value.equals(dnode3._value) && dataStore._groupings(1)._value.equals(dnode2._value) && dataStore._groupings(2)._value.equals(dnode1._value))
    dataStore.regroup("STATE")
    assert(dataStore._groupings(0)._value.equals(dnode1._value) && dataStore._groupings(1)._value.equals(dnode2._value) && dataStore._groupings(2)._value.equals(dnode3._value))
  }

  // ^^^
  behavior of "GroupByStore.length"
  it should "be 0 when initialized" in {
    assert(dataStore.length == 0)
  }

  it should "be updated after each insertion" in {
    val entries = AssessmentUtilities.loadAssessmentEntries(AssessmentUtilities.DATA_FILENAME, AssessmentUtilities.DATA_ROWS)
    for (i <- 0 until entries.length) {
      dataStore.insert(entries(i))
      assert(dataStore.length == i + 1)
    }
  }

  behavior of "GroupByStore.insert"
  it should "..." in {

  }

  behavior of "GroupByStore.regroup"
  it should "..." in {

  }

  behavior of "GroupByStore.iterator"
  it should "retrieve all stored entries" in {
    val entries = AssessmentUtilities.loadAssessmentEntries(AssessmentUtilities.DATA_FILENAME, AssessmentUtilities.DATA_ROWS)
    val testEntriesSet = new mutable.HashSet[TaxParcel]
    
    // Add all loaded values into your dataStore.
    for (i <- 0 until entries.length) {
      dataStore.insert(entries(i))
      testEntriesSet.add(entries(i))
    }

    // Check that all loaded values are iterated through in your dataStore.
    val dataIterator = dataStore.iterator
    val storedEntriesSet = new mutable.HashSet[TaxParcel]
    for (_ <- 0 until entries.length) {
      // dataIterator should still be valid.
      assert(dataIterator.hasNext)
      assert(dataIterator.hasNext)
      // Retrieve next element from sequence.
      val taxParcel = dataIterator.next
      // Check that entry was in the set of inserted entries.
      assert(testEntriesSet.contains(taxParcel))
      // Check that all entries are unique.
      assert(!storedEntriesSet.contains(taxParcel))
      storedEntriesSet.add(taxParcel)
    }
    assert(!dataIterator.hasNext)
  }

  it should "retrieve all stored entries after regrouping" in {
    val entries = AssessmentUtilities.loadAssessmentEntries(AssessmentUtilities.DATA_FILENAME, AssessmentUtilities.DATA_ROWS)
    val testEntriesSet = new mutable.HashSet[TaxParcel]

    // Add all loaded values into your dataStore.
    for (i <- 0 until entries.length) {
      dataStore.insert(entries(i))
      testEntriesSet.add(entries(i))
    }

    // Check that all loaded values are iterated through in your dataStore.
    var dataIterator = dataStore.iterator
    var storedEntriesSet = new mutable.HashSet[TaxParcel]
    for (_ <- 0 until entries.length) {
      // dataIterator should still be valid.
      assert(dataIterator.hasNext)
      assert(dataIterator.hasNext)
      // Retrieve next element from sequence.
      val taxParcel = dataIterator.next
      // Check that entry was in the set of inserted entries.
      assert(testEntriesSet.contains(taxParcel))
      // Check that all entries are unique.
      assert(!storedEntriesSet.contains(taxParcel))
      storedEntriesSet.add(taxParcel)
    }
    assert(!dataIterator.hasNext)

    // Make a call to regroup.
    dataStore.regroup("SBL")

    // Check that all loaded values are iterated through in your dataStore.
    dataIterator = dataStore.iterator
    storedEntriesSet = new mutable.HashSet[TaxParcel]
    for (_ <- 0 until entries.length) {
      // dataIterator should still be valid.
      assert(dataIterator.hasNext)
      assert(dataIterator.hasNext)
      // Retrieve next element from sequence.
      val taxParcel = dataIterator.next
      // Check that entry was in the set of inserted entries.
      assert(testEntriesSet.contains(taxParcel))
      // Check that all entries are unique.
      assert(!storedEntriesSet.contains(taxParcel))
      storedEntriesSet.add(taxParcel)
    }
    assert(!dataIterator.hasNext)
  }

  behavior of "GroupByStore.iterator(String)"
  it should "..." in {

  }
}
