package edu.fudan.selab
package dto

enum RecordType {
  case JAVADOC_INCOMPLETENESS
  case JAVADOC_NEW_METHOD
  case JAVADOC_CALL_GRAPH_IMPACT
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


class JavadocMethodRecord(
                     t: RecordType,
                     sig: String,
                     line: Int,
                     message: String
                   ) extends Record(t, bt = BodyType.METHOD, sig, line, message)

class JavadocNewMethodRecord(
                              sig: String,
                              line: Int,
                              message: String
                            ) extends JavadocMethodRecord(t = RecordType.JAVADOC_NEW_METHOD, sig, line, message)


class JavadocIncompletenessRecord(
                                  sig: String,
                                  line: Int,
                                  message: String
                                 ) extends JavadocMethodRecord(t = RecordType.JAVADOC_INCOMPLETENESS, sig, line, message)


class JavadocCallGraphImpactRecord(
                                    sig: String,
                                    line: Int,
                                    message: String
                                  ) extends JavadocMethodRecord(t = RecordType.JAVADOC_CALL_GRAPH_IMPACT, sig, line, message)


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
