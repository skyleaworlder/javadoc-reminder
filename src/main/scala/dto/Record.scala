package edu.fudan.selab
package dto

import dto.RecordType.JAVADOC_INCOMPLETENESS

enum RecordType {
  case JAVADOC_INCOMPLETENESS
  case JAVADOC_NEW_METHOD
  case JAVADOC_CALL_GRAPH_IMPACT
  case JAVADOC_CLASS_HIERARCHY_IMPACT
  case AGE
  case IMPORTANCE
}

object RecordType {
  def parse(t: RecordType): String =
    t match
      case JAVADOC_INCOMPLETENESS => "JAVADOC_INCOMPLETENESS"
      case JAVADOC_NEW_METHOD => "JAVADOC_NEW_METHOD"
      case JAVADOC_CALL_GRAPH_IMPACT => "JAVADOC_CALL_GRAPH_IMPACT"
      case JAVADOC_CLASS_HIERARCHY_IMPACT => "JAVADOC_CLASS_HIERARCHY_IMPACT"
      case AGE => "AGE"
      case IMPORTANCE => "IMPORTANCE"
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


class JavadocClassRecord(
                          t: RecordType,
                          sig: String,
                          line: Int,
                          message: String
                        ) extends Record(t, bt = BodyType.CLASS, sig, line, message) {
  override def toString: String = s"[CLASS-${RecordType.parse(t)}]($line) <- ($sig): $message"
}


class JavadocMethodRecord(
                     t: RecordType,
                     sig: String,
                     line: Int,
                     message: String
                   ) extends Record(t, bt = BodyType.METHOD, sig, line, message) {
  override def toString: String = s"[METHOD-${RecordType.parse(t)}]($line) <- ($sig): $message"
}

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


class ClassHierarchyImpactRecord(
                                  sig: String,
                                  line: Int,
                                  message: String
                                ) extends JavadocClassRecord(t = RecordType.JAVADOC_CLASS_HIERARCHY_IMPACT, sig, line, message)


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
