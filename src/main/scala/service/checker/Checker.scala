package edu.fudan.selab
package service.checker

import edu.fudan.selab.dto.{Record, RecordType}

trait Checker {

  def check(stream: CheckerStream): Array[Record] = {
    beforeProcess(stream)
    val records = process(stream)
    afterProcess(stream)
    records
  }

  def beforeProcess(stream: CheckerStream): Unit = {}

  def process(stream: CheckerStream): Array[Record]

  def afterProcess(stream: CheckerStream): Unit = {}
}
