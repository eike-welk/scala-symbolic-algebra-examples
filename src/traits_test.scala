object traits_test {
  def main(args : Array[String]) : Unit = {
    
    var pet: Friendly = new Dog
	println(pet.greet())
	
	pet = new Cat
	println(pet.greet())
	
	pet = new HungryDog
	println(pet.greet())
	
	pet = new Dog with ExclamatoryGreeter
	println(pet.greet())
	
	pet = new Cat with ExclamatoryGreeter
	println(pet.greet())
	
	pet = new HungryDog with ExclamatoryGreeter
	println(pet.greet())
	  }
}

trait Friendly {
  def greet() = "Hi"
}

class Dog extends Friendly {
  override def greet() = "Woof"
}

class Cat extends Friendly {
  override def greet() = "Meow"
}

class HungryDog extends Dog {
  override def greet() = "I'd like to eat my own dog food"
}

trait ExclamatoryGreeter extends Friendly {
  override def greet() = super.greet() + "!"
}
