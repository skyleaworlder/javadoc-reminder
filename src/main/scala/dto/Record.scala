package edu.fudan.selab
package dto

enum RecordType {
  case JAVADOC
  case AGE
  case IMPORTANCE
}


enum BodyType {
  case CLASS
  case METHOD
}


class Record(
              val t: RecordType,
              val bt: BodyType,
              val sig: String,
              val line: Int,
              val message: String
            )


class JavadocRecord(
                     bt: BodyType,
                     sig: String,
                     line: Int,
                     message: String
                   ) extends Record(t = RecordType.JAVADOC, bt, sig, line, message)


class AgeRecord(
                 bt: BodyType,
                 sig: String,
                 line: Int,
                 message: String
               ) extends Record(t = RecordType.AGE, bt, sig, line, message)


class ImportanceRecord(
                        bt: BodyType,
                        sig: String,
                        line: Int,
                        message: String
                      ) extends Record(t = RecordType.IMPORTANCE, bt, sig, line, message)
