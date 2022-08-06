package server.models

object CommonClasses {

  final case class Token(id: String) extends AnyVal
  final case class ErrorMessage(error: String) extends AnyVal

}
