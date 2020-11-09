object Exercises {
  trait Animal {
    def name: String
  }

  case class Cat(override val name: String) extends Animal

  case class Dog(override val name: String) extends Animal



  case class Shelter[+T <: Animal](animalList: List[T]){
    val AnimalList = animalList

    def ++ [A >: T <: Animal](that: Shelter[A]) :Shelter[A] =
      new Shelter[A](AnimalList ::: that.AnimalList)

    def + [A >: T <: Animal](that: A) :Shelter[A] =
      new Shelter[A](AnimalList :+ that)

    def getNames(): List[String] =
      AnimalList.map(_.name)

    def feed[A >: T <: Animal](food: Food[A]): List[String] =
      animalList.map(food.feed)

  }

  trait Food[T <: Animal]{
    def feed(animal: T): String =
      s"${animal.name} eats ${this.toString.toLowerCase}"
  }

  case object Meat extends Food[Animal]

  case object Milk extends Food[Cat]

  case object Bread extends Food[Dog]
}


