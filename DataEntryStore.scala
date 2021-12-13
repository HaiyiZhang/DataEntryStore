/**
 * cse250.pa1.DataEntryStore.scala
 *
 * Copyright 2020 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 * Submission author
 * UBIT:haiyizha
 * Person#:50287269
 *
 * Collaborators (include UBIT name of each, comma separated):
 * UBIT:
 */
package cse250.pa1

import cse250.objects.{EmbeddedEmpty, EmbeddedListNode, EmbeddedNode}

import scala.collection.mutable
import scala.collection.mutable.Seq
import scala.util.control._

class DataEntryStore[A](private val _capacity: Int = 100)
  extends collection.mutable.Seq[A] {
  // These private members should not be modified.
  private val _emptyNode = new EmbeddedEmpty[A]
  private val _dataArray = Array.fill[EmbeddedListNode[A]](_capacity)(_emptyNode)
  private var _headIndex = -1
  private var _tailIndex = -1
  private var _numStored = 0

  // Public getters for private members.
  def dataArray = _dataArray
  def headIndex = _headIndex
  def tailIndex = _tailIndex
  def emptyNode = _emptyNode

  /** Inserts element to tail of list. */
  def insert(elem: A): Unit ={
    if(_numStored < _capacity){
    var num = 0;
    val loop = new Breaks
    loop.breakable {
      for(i <- 0 until _capacity){
        if(_dataArray(i) == _emptyNode) {
          num = i
          if(_dataArray(num) == _emptyNode) {
            loop.break;
          }
        }
      }

    }

    val node = new EmbeddedNode[A](elem: A,-1,-1)
    _dataArray(num) = node
    if(_numStored == 0){
      _dataArray(num).prev = -1
    }
    else{
      for(i <- 0 until _capacity){
        if(_dataArray(i).next == -1 && _dataArray(i) != _emptyNode && i != num){
          _dataArray(i).next = num
          _dataArray(num).prev = i
        }
      }
    }
    _dataArray(num).next = -1
    _numStored = _numStored + 1

    for(i <- 0 until _capacity){
      if(_dataArray(i).prev == -1 && _dataArray(i) != _emptyNode){
        _headIndex = i
      }

    }

    for(i <- 0 until _capacity){
      if(_dataArray(i).next == -1 && _dataArray(i) != _emptyNode){
        _tailIndex = i
      }

    }
    }
      //more than capacity
    else{
      _dataArray(_dataArray(_headIndex).next).prev = -1
      val node = new EmbeddedNode[A](elem: A,-1,-1)
      _dataArray(_headIndex) = node
      for(i <- 0 until _capacity){
        if(_dataArray(i).next == -1 && _dataArray(i) != _emptyNode && i != _headIndex){
          _dataArray(i).next = _headIndex
          _dataArray(_headIndex).prev = i
        }
      }
      _dataArray(_headIndex).next = -1
      _numStored = _capacity
      for(i <- 0 until _capacity){
        if(_dataArray(i).prev == -1 && _dataArray(i) != _emptyNode){
          _headIndex = i
        }

      }

      for(i <- 0 until _capacity){
        if(_dataArray(i).next == -1 && _dataArray(i) != _emptyNode){
          _tailIndex = i
        }

      }
    }

  }

  /** Removes all copies of the given element. */
  def remove(elem: A): Boolean = {
    var check = 0
    for (i <- 0 until _capacity) {
      if (_dataArray(i) != _emptyNode) {
        if (_dataArray(i).value.get == elem) {
          check = check + 1
          if(_headIndex == _tailIndex){
            _dataArray(i) = _emptyNode
            _headIndex = -1
            _tailIndex = -1
          }
          else{
            if (i == _headIndex) {
              _headIndex = _dataArray(_headIndex).next // change the headIndex
              _dataArray(_dataArray(i).next).prev = -1 // change the second to first so change the prev to -1
              _dataArray(i) = _emptyNode
            }
            else if (i == _tailIndex) {
              _tailIndex = _dataArray(_tailIndex).prev
              _dataArray(_dataArray(i).prev).next = -1
              _dataArray(i) = _emptyNode
            }
            else {
              _dataArray(_dataArray(i).prev).next = _dataArray(i).next
              _dataArray(_dataArray(i).next).prev = _dataArray(i).prev
              _dataArray(i) = _emptyNode
            }
          }

        }
      }
    }

    _numStored = _numStored - check
    if(check == 0){
      return false
    }
    else{
      return true
    }
  }

  /** Returns the count of nodes containing given entry. */
  def countEntry(entry: A): Int = {
    var num = 0
    for (i <- 0 until _capacity){
      if (_dataArray(i) != _emptyNode) {
        if (_dataArray(i).value.get == entry){
          num = num + 1
        }
      }
    }
    num
  }

  /** Gets the element at the specified tindex. */
  override def apply(idx: Int): A = {
    require(idx < _numStored)
    var seq: collection.mutable.Seq[A] = collection.mutable.Seq()
    var fi = _headIndex
    if(_numStored == 1){
      seq = seq :+ _dataArray(_headIndex).value.get
    }
    else if(_numStored > 1){
      while(fi != _tailIndex){
        seq = seq :+ _dataArray(fi).value.get
        fi = _dataArray(fi).next
      }
      seq = seq :+ _dataArray(_tailIndex).value.get
    }
    seq(idx)
  }



  /** Replaces element at given index with a new value. */
  override def update(idx: Int, elem: A): Unit = {
    require(idx < _numStored)
    var seq: collection.mutable.Seq[A] = collection.mutable.Seq()
    var fi = _headIndex
    var flag = 0
    if(_numStored == 1){
      seq = seq :+ _dataArray(_headIndex).value.get
    }
    else if(_numStored > 1){
      while(fi != -1){
        flag = flag + 1
        if(flag-1 == idx){
          seq = seq :+ elem
          _dataArray(fi).value = elem
        }
        else {
          seq = seq :+ _dataArray(fi).value.get
        }

        fi = _dataArray(fi).next
      }
    }
    seq(idx)
  }

  /** Returns an Iterator that can be used only once. */
  def iterator: Iterator[A] = new Iterator[A] {
    private var currentIndex = _headIndex

    override def hasNext: Boolean = currentIndex != -1

    override def next(): A = {
      val previousIndex = currentIndex
      currentIndex = _dataArray(currentIndex).next
      _dataArray(previousIndex).value.get
    }
  }

  /** Returns the length of the stored list. */
  override def length: Int = _numStored

  override def toString: String = if (_numStored == 0) "" else this.iterator.addString(new StringBuilder, "DataEntryStore: (", ",", ")\n").result()
}
