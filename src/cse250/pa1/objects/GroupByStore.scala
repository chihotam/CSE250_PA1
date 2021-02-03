/**
 * cse250.pa1.GroupByStore.scala
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
package cse250.pa1.objects

import cse250.objects.{DNode, TaxParcel}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class GroupByStore extends Seq[TaxParcel] {
  // Member/instance variables are defined public for ease of access for testing.
  var _groupings: ArrayBuffer[DNode[TaxParcel]] = new ArrayBuffer[DNode[TaxParcel]]
  var _groupingAttribute: String = "STREET"
  var _numStored = 0
  var _groups: List[String] = List()

  var temp_groupings: ArrayBuffer[DNode[TaxParcel]] = new ArrayBuffer[DNode[TaxParcel]]
  var temp_groupingAttribute: String = ""
  var temp_numStored = 0
  var temp_groups: List[String] = List()

  def apply(i: Int): TaxParcel = {
    val iter = this.iterator
    for (_ <- 0 until i) iter.next()
    iter.next()
  }

  /** Inserts element to head of corresponding grouping list. */
  def insert(taxParcel: TaxParcel): Unit = {
    val group = taxParcel.parcelInfo(_groupingAttribute)
    val dnode = new DNode[TaxParcel](taxParcel,null,null)
    var copy = false
    var counter = 0
    if(_groups.contains(group)){
      val iter = iterator(group)
      while(iter.hasNext){
        if(iter.next.equals(taxParcel)){
          copy = true
        }
      }
      if(!copy){
        dnode._next = _groupings(_groups.indexOf(group))
        _groupings(_groups.indexOf(group))._prev = dnode
        _groupings(_groups.indexOf(group)) = dnode
        _numStored = _numStored + 1
      }
    }
    else{
      _groups = _groups :+ group
      _groupings = _groupings :+ dnode
      _numStored = _numStored + 1
      if(_groups.length > 1){
        var front: ArrayBuffer[DNode[TaxParcel]] = new ArrayBuffer[DNode[TaxParcel]]
        var back: ArrayBuffer[DNode[TaxParcel]] = new ArrayBuffer[DNode[TaxParcel]]
        var diffCounter = 0
        _groups = _groups.sorted
        while(diffCounter != _groups.length){
          if(_groups(counter) == _groupings(diffCounter)._value.parcelInfo(_groupingAttribute)){
            front = front :+ _groupings(diffCounter)
            counter = counter + 1
          }
          else{
            back = back :+ _groupings(diffCounter)
          }
          diffCounter = diffCounter + 1
        }
        _groupings = front
        for(x <- back){
          _groupings = _groupings :+ x
        }
      }
    }
  }

  def reinsert(taxParcel: TaxParcel): Unit = {
    val group = taxParcel.parcelInfo(temp_groupingAttribute)
    val dnode = new DNode[TaxParcel](taxParcel,null,null)
    var counter = 0
    if(temp_groups.contains(group)){
      dnode._next = temp_groupings(temp_groups.indexOf(group))
      temp_groupings(temp_groups.indexOf(group))._prev = dnode
      temp_groupings(temp_groups.indexOf(group)) = dnode
      temp_numStored = temp_numStored + 1
    }
    else{
      temp_groups = temp_groups :+ group
      temp_groupings = temp_groupings :+ dnode
      temp_numStored = temp_numStored + 1
      if(temp_groups.length > 1){
        var front: ArrayBuffer[DNode[TaxParcel]] = new ArrayBuffer[DNode[TaxParcel]]
        var back: ArrayBuffer[DNode[TaxParcel]] = new ArrayBuffer[DNode[TaxParcel]]
        var diffCounter = 0
        temp_groups = temp_groups.sorted
        while(diffCounter != temp_groups.length){
          if(temp_groups(counter) == temp_groupings(diffCounter)._value.parcelInfo(temp_groupingAttribute)){
            front = front :+ temp_groupings(diffCounter)
            counter = counter + 1
          }
          else{
            back = back :+ temp_groupings(diffCounter)
          }
          diffCounter = diffCounter + 1
        }
        temp_groupings = front
        for(x <- back){
          temp_groupings = temp_groupings :+ x
        }
      }
    }
  }

  /** Regroup . */
  def regroup(attribute: String): Unit = {
    if(attribute != _groupingAttribute){
      temp_groupingAttribute = attribute
      val iter = iterator
      while(iter.hasNext){
        reinsert(iter.next)
      }
    }
    _groupings = temp_groupings
    _groupingAttribute = temp_groupingAttribute
    _numStored = temp_numStored
    _groups = temp_groups
    temp_groupings = new ArrayBuffer[DNode[TaxParcel]]
    temp_groupingAttribute = ""
    temp_numStored = 0
    temp_groups = List()
  }

  /** "Both iterators were based off: https://github.com/ub-cse-250/Lecture-Examples/blob/master/src/cse250/examples/list/LectureSinglyLinkedList.scala" */

  /** Returns an Iterator to all entries that can be used only once. */
  def iterator: Iterator[TaxParcel] = new Iterator[TaxParcel] {
    var idx = 0
    var round = 0
    var current: DNode[TaxParcel] = _
    if(_groupings.nonEmpty){
      current = _groupings.head
    }
    override def hasNext: Boolean = {
      if(_groupings.isEmpty){
        false
      }
      else if(current._next == null && idx == _numStored && round == _groupings.length) {
        false
      }
      else{
        true
      }
    }

    override def next(): TaxParcel = {
      val temp = current._value
      if(current._next != null){
        idx = idx + 1
        current = current._next
      }
      else{
        idx = idx + 1
        round = round + 1
        if(round != _groupings.length && idx != _numStored){
          current = _groupings(round)
        }
      }
      temp
    }
  }

  /** Returns an Iterator to only the entries with matching values on the grouping attribute that can be used only once. */
  def iterator(value: String): Iterator[TaxParcel] = new Iterator[TaxParcel] {
    var current: DNode[TaxParcel] = _
    if(_groupings.nonEmpty && _groups.contains(value)){
      current = _groupings(_groups.indexOf(value))
    }
    override def hasNext: Boolean = {
      if(_groupings.isEmpty){
        false
      }
      else if(current != null){
        true
      }
      else{
        false
      }
    }

    override def next(): TaxParcel = {
      val temp = current._value
      current = current._next
      temp
    }
  }

  def length: Int = _numStored

  override def toString: String = this.iterator.mkString("GroupByStore(", "\n", ")")
}