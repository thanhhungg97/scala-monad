package content

class BasicExample {
  case class SafeValue[+T](private val internalValue: T) { // constructor = pure or unit
    def get: T = synchronized {
      internalValue
    }
    def flatMap[S](transformer: T => SafeValue[S]): SafeValue[S] = synchronized { // bind // flatMap
      transformer(internalValue)
    }
  }

  // external API
  def giveMeSaveValue[T](value: T): SafeValue[T] = SafeValue(value)

  def safeString: SafeValue[String] = giveMeSaveValue("Scala is awesome")

  //extract
  val string = safeString.get

  //transform
  val upperString = string.toUpperCase()

  // wrap
  val upperSafeString = SafeValue(upperString)
  // ETW

  // compressed:
  val upperSafeString2 = safeString.flatMap(s => SafeValue(s.toUpperCase()))


  // Examples
  case class Person(firstName: String, lastName: String){
    assert(firstName != null && lastName != null)

  }
  // census API
  def getPerson(firstName: String, lastName: String): Option[Person] ={
    for{
      firstName <- Option(firstName)
      lastName <- Option(lastName)
    } yield Person(firstName, lastName)
  }
}
