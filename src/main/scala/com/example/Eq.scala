package com.example

trait Eq[A] {
  // TODO #1: Define an 'eq' method that takes two A values as parameters, and returns a Boolean
  def eq(lhs: A, rhs: A) : Boolean
}

object Eq {
  // TODO #2: Define the method 'apply' so we can summon instances from implicit scope
  def apply[A](implicit ev: Eq[A]) : Eq[A] = return ev

  // TODO #3: Define the method 'instance' so we can build instances of the Eq typeclass more easily.
  //          It should take as the only parameter a function of type (A, A) => Boolean
  def instance[A](f: Function2[A, A, Boolean]) : Eq[A] = return new Eq[A] {
    override def eq(lhs: A, rhs: A) : Boolean = return f.apply(lhs, rhs)
  }

  // TODO #4: Define an Eq instance for String
  implicit val stringEq: _root_.com.example.Eq[_root_.java.lang.String] = Eq.instance((lhs: String, rhs: String) => lhs.==(rhs))

  // TODO #5: Define an Eq instance for Int
  implicit object intEq extends Eq[Int] {
    override def eq(lhs: Int, rhs: Int): Boolean = return lhs.==(rhs)
  }

  // TODO #6: Define an Eq instance for Person. Two persons are equal if both their names and ids are equal.
  //          Extra points: receive implicit instances for String and Int and use them
  implicit object personEq extends Eq[Person] {
    override def eq(lhs: Person, rhs: Person): Boolean = return intEq.eq(lhs.id, rhs.id).&&(stringEq.eq(lhs.name, rhs.name))
  }

  // TODO #7: Provide a way to automatically derive instances for Eq[Option[A]] given that we have an implicit
  //          instance for Eq[A]
  implicit def optionEq[A](implicit aEq: Eq[A]) : Eq[Option[A]] = return new Eq[Option[A]] {
    override def eq(lhsOpt: Option[A], rhsOpt: Option[A]): Boolean = {

      lhsOpt.flatMap(l => rhsOpt.map(r => aEq.eq(l, r))).getOrElse(false)
    }
  }

  object Syntax {
    // TODO #8: Define a class 'EqOps' with a method 'eqTo' that enables the following syntax:
    //          "hello".eqTo("world")
  }
}