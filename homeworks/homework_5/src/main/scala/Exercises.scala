object Exercises {

  trait Animal {
    def name: String
  }

  case class Cat(override val name: String) extends Animal

  case class Dog(override val name: String) extends Animal

  case class Shelter[+T <: Animal] (listAnimals: List[T]) {

    def +[A >: T <: Animal](animalAdd: A): Shelter[A] = Shelter[A](listAnimals :+ animalAdd)

    def ++[A >: T <: Animal](otherShelter: Shelter[A]): Shelter[A] =
      Shelter[A](listAnimals ++ otherShelter.listAnimals)

    def getNames: List[String] = listAnimals.map(animal => animal.name)

    def feed[A >: T <: Animal](food: Food[A]): List[String] = {
      if (!food.isInstanceOf[Food[T]]) throw new IllegalArgumentException
      var result = List[String]()
      for (animal <- listAnimals) {
        result :+= s"${animal.name} eats ${food.name}"
      }
      result
    }
  }
  trait Food[T <: Animal] {
    def name: String
  }

  case object Meat extends Food[Animal] {
    override def name: String = "meat"
  }

  case object Milk extends Food[Animal] {
    override def name: String = "milk"
  }

  case object Bread extends Food[Dog] {
    override def name: String = "bread"
  }
}