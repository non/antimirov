package inclusion

object SetUtil {
  implicit class SetOps[A](val lhs: Set[A]) extends AnyVal {
    def intersects(rhs: Set[A]): Boolean =
      if (lhs.size <= rhs.size) lhs.exists(a => rhs(a))
      else rhs.exists(a => lhs(a))
  }
}

import SetUtil._

object Re {

  case class Letter(c: Char) extends Re
  case class Choice(x: Re, y: Re) extends Re
  case class Cat(x: Re, y: Re) extends Re
  //case class Inter(rs: List[Re]) extends Re
  case class Star(r: Re) extends Re
  case object Epsilon extends Re
  case object Phi extends Re

  val phi: Re = Phi
  val eps: Re = Epsilon

  def apply(c: Char): Re =
    Letter(c)

  def apply(rs: Re*): Re =
    rs.toList match {
      case r :: Nil => r
      case r :: rs => Re.cat(r, apply(rs: _*))
      case Nil => Epsilon
    }

  def choice(x: Re, y: Re): Re =
    Choice(x, y)

  def cat(x: Re, y: Re): Re =
    if (x == Epsilon) y
    else if (y == Epsilon) x
    else Cat(x, y)

  def star(x: Re): Re =
    if (x == Epsilon) x else Star(x)

  val special: Set[Char] =
    Set('ε', '(', ')', '|', '*', '&')

  def escape(s: String): String = {
    val sb = new StringBuilder
    s.foreach { c =>
      if (special(c)) sb += '\\'
      sb += c
    }
    sb.toString
  }

  def runInclusion(r1: Re, r2: Re): Inclusion =
    incFull(Set.empty, r1, r2)._1

  import Included._
  import Inclusion._

  def incFull(m: Set[(Re, Re)], lhs: Re, rhs: Re): (Inclusion, Set[(Re, Re)]) =
    if (lhs.isNullable && rhs.nonNullable) {
      (NotInclusion(rhs, rhs, s"Not an inclusion because $lhs is nullable, while $rhs is not"), m)
    } else {
      val (re1, re2) = (lhs.headerForm, rhs.headerForm)
      val pair = (re1, re2)
      val m1 = m + pair
      if (m(pair)) (Loop(re1, re2), m1) else incFull1(m1, re1, re2)
    }

  def incFull1(m: Set[(Re, Re)], lhs: Re, rhs: Re): (Inclusion, Set[(Re, Re)]) =
    (lhs, rhs) match {
      case (Phi, r) =>
        (Axm(r), m + ((Phi, r)))
      case (Epsilon, r) if r.isNullable =>
        (Axm(r), m + ((Epsilon, r)))
      case (Epsilon, r) =>
        (NotInclusion(Epsilon, r, s"Axm error: $r not nullable"), m)
      case (Cat(Letter(c1), r1), Cat(Letter(c2), r2)) if c1 == c2 =>
        val (i1, m1) = incFull(m, r1, r2)
        (LetterRule(c1, r1, r2, i1), m1)
      case (Cat(Letter(c1), r1), Cat(Letter(c2), r2)) =>
        (NotInclusion(lhs, rhs, s"Cannot show that $lhs is a subset of $rhs"), m)
      case (Cat(Letter(c), r1), Cat(Star(r2), r3)) =>
        val (f2, f3) = (r2.firstSet, r3.firstSet)
        (f2(c), f3(c)) match {
          case (true, true) =>
            (Not1Unambiguous(s"Error while deciding $lhs <= $rhs, since letter $c is in firstset of $r2"), m)
          case (true, false) =>
            val (i1, m1) = incFull(m, lhs, Cat(r2, rhs))
            (LetterStar(c, r1, r2, r3, i1), m1)
          case (false, true) =>
            val (i1, m1) = incFull(m, lhs, r3)
            (ElimCat(lhs, Star(r2), r3, i1), m1)
          case (false, false) =>
            (NotInclusion(lhs, rhs, s"Cannot show that $lhs is a subset of $rhs"), m)
        }
      case (Cat(Letter(c), r1), Cat(ch @ Choice(r2, r3), r4)) =>
        val (f2, f3, f4) = (r2.firstSet, r3.firstSet, r4.firstSet)
        if (f2(c)) {
          if (f3(c)) {
            (Not1Unambiguous(s"Error while deciding $lhs <= $rhs, since $c is in firstset of $r3"), m)
          } else if (ch.isNullable && f4(c)) {
            (Not1Unambiguous(s"Error while deciding $lhs <= $rhs, since $c is in firstset of $r4"), m)
          } else {
            val (i1, m1) = incFull(m, lhs, Cat(r2, r4))
            (LetterChoice2(c, r1, r2, r3, r4, i1), m1)
          }
        } else if (f3(c)) {
          if (ch.isNullable && f4(c)) {
            (Not1Unambiguous(s"$rhs is 1-ambiguous"), m)
          } else {
            val (i1, m1) = incFull(m, lhs, Cat(r3, r4))
            (LetterChoice3(c, r1, r2, r3, r4, i1), m1)
          }
        } else if (ch.isNullable && f4(c)) {
          val (i1, m1) = incFull(m, lhs, r4)
          (ElimCat(lhs, ch, r4, i1), m1)
        } else {
          (NotInclusion(lhs, rhs, s"Cannot show that $lhs is a subset of $rhs"), m)
        }
      case (Cat(Choice(r1, r2), r3), r4) =>
        val (i1, m1) = incFull(m, Cat(r1, r3), r4)
        val (i2, m2) = incFull(m1, Cat(r2, r3), r4)
        (LeftChoice(r1, r2, r3, r4, i1, i2), m2)
      case (Cat(Star(r1), Epsilon), Cat(Star(r2), Epsilon)) =>
        val (i1, m1) = incFull(m, r1, rhs)
        (StarStarE(r1, r2, i1), m1)
      case (Cat(Star(r1), r2), Cat(Choice(r3, r4), r5)) =>
        val (f1, f2, f3, f4) = (r1.firstSet, r2.firstSet, r3.firstSet, r4.firstSet)
        val (f5, f35, f45) = (r5.firstSet, Cat(r3, r5).firstSet, Cat(r4, r5).firstSet)
        if (((f1 intersects f3) || (f2 intersects f3)) &&
          (f1 subsetOf f35) && (f2 subsetOf f35) &&
          r2.nonNullable && r3.isNullable) {
          if ((f1 intersects f4) || (f2 intersects f4)) {
            (Not1Unambiguous(s"Not 1-unambiguous: $rhs"), m)
          } else {
            val (i1, m1) = incFull(m, lhs, Cat(r3, r5))
            (StarChoice1(r1, r2, r3, r4, r5, i1), m1)
          }
        } else if (((f1 intersects f4) || (f2 intersects f4)) &&
          (f1 subsetOf f45) && (f2 subsetOf f45) && (r2.nonNullable || r2.isNullable)) {
          if ((f1 intersects f3) || (f2 intersects f3)) {
            (Not1Unambiguous(s"Not 1-unambiguous: $rhs"), m)
          } else {
            val (i1, m1) = incFull(m, lhs, Cat(r4, r5))
            (StarChoice1(r1, r2, r3, r4, r5, i1), m1)
          }
        } else if ((f1 subsetOf f5) && (f2 subsetOf f5) && (Choice(r3, r4).isNullable)) {
          if ((f1 intersects f3) || (f1 intersects f4) || (f2 intersects f3) || (f2 intersects f4)) {
            (Not1Unambiguous(s"Not 1-unambiguous: $rhs"), m)
          } else {
            val (i1, m1) = incFull(m, lhs, r5)
            (ElimCat(lhs, Choice(r3, r4), r5, i1), m1)
          }
        } else {
          val (i2, m1) = incFull(m, r2, rhs)
          val (i1, m2) = incFull(m1, Cat(r1, lhs), rhs)
          (StarChoice2(r1, r2, r3, r4, r5, i1, i2), m2)
        }
      case (Cat(Star(r1), r2), Cat(r3, r4)) =>
        val (f1, f3, f4) = (r1.firstSet, r3.firstSet, r4.firstSet)
        if (f1 intersects f3) {
          if (r3.isNullable && (f1 subsetOf f4)) {
            (Not1Unambiguous(s"Firstet of $r1 is included in firset of $r4. This came up in the decision: $lhs <= $rhs"), m)
          } else if (r4 == Epsilon && r3.isLetter) {
            (NotInclusion(lhs, r3, s"LeftStart prevents showing $lhs <= $r3. A single letter cannot be the superset of a starred expression."), m)
          } else {
            val (i1, m1) = incFull(m, Cat(r1, lhs), rhs)
            val (i2, m2) = incFull(m1, r2, rhs)
            (LeftStar(r1, r2, r3, r4, i1, i2), m2)
          }
        } else if (f1 subsetOf f4) {
          val (i1, m1) = incFull(m, lhs, r4)
          (ElimCat(lhs, r3, r4, i1), m1)
        } else {
          (NotInclusion(lhs, rhs, s"Cannot show that $lhs is a subset of $rhs"), m)
        }
      case (r1, r2) =>
        (NotInclusion(r1, r2, s"Error in call to IncFull: $r1 <= $r2"), Set.empty)
    }



}

sealed abstract class Re { lhs =>

  import Re.{Letter, Choice, Cat, /*Inter, */Star, Epsilon, Phi}

  def +(rhs: Re): Re =
    Re.choice(lhs, rhs)

  def *(rhs: Re): Re =
    Re.cat(lhs, rhs)

  def star: Re =
    Re.star(this)

  override def toString: String =
    this match {
      case Letter(c) => Re.escape(c.toString)
      case Choice(x, y) => s"($x|$y)"
      case Cat(x, y) => s"$x$y"
      //case Inter(rs) => rs.mkString("(", "&", ")")
      case Star(r) => s"($r)*"
      case Epsilon => "ε"
      case Phi => "ϕ"
    }

  // def fold[A](eps: A)(chr: Char => A)(str: A => A)(cat: (A, A) => A)(int: List[A] => A)(cho: (A, A) => A): A = {
  //   def recur(r: Re): A = r.fold(eps)(chr)(str)(cat)(int)(cho)
  //   this match {
  //     case Epsilon => eps
  //     case Phi => eps
  //     case Letter(c) => chr(c)
  //     case Star(r) => str(recur(r))
  //     case Cat(x, y) => cat(recur(x), recur(y))
  //     //case Inter(rs) => int(rs.map(r => recur(r)))
  //     case Choice(x, y) => cho(recur(x), recur(y))
  //   }
  // }

  // def map(f: Re => Re): Re =
  //   this match {
  //     case Epsilon => Epsilon
  //     case letter @ Letter(_) => letter
  //     case Star(r) => Re.star(f(r.map(f)))
  //     case Cat(x, y) => Re.cat(f(x.map(f)), f(y.map(f)))
  //     case Choice(x, y) => Re.choice(f(x.map(f)), f(y.map(f)))
  //     //case Inter(rs) => Inter(rs.map(r => f(r.map(f))))
  //   }

  def nonNullable: Boolean =
    !isNullable

  def isNullable: Boolean =
    this match {
      case Phi => false
      case Epsilon => true
      case Star(_) => true
      case Letter(_) => false
      case Choice(x, y) => x.isNullable || y.isNullable
      //case Inter(rs) => rs.forall(_.isNullable)
      case Cat(x, y) => x.isNullable && y.isNullable
    }

  def nonEmpty: Boolean =
    !isEmpty

  def isEmpty: Boolean =
    this match {
      case Epsilon => true
      case Letter(_) => false
      case Star(r) => r.isEmpty
      case Choice(x, y) => x.isEmpty && y.isEmpty
      //case Inter(rs) => rs.isEmpty || rs.exists(_.isEmpty)
      case Cat(x, y) => x.isEmpty && y.isEmpty
    }

  def isEpsilonForm: Boolean = {
    def recur(r: Re): Boolean =
      r match {
        case Epsilon => false
        case Letter(_) => true
        case Star(r) => recur(r)
        case Cat(x, y) => recur(x) && recur(y)
        //case Inter(rs) => rs.isEmpty || rs.forall(recur)
        case Choice(x, y) => recur(x) || recur(y)
      }
    this == Epsilon || recur(this)
  }

  def isStarNormalForm: Boolean = {
    def recur(r: Re): Boolean =
      r match {
        case Epsilon => true
        case letter @ Letter(_) => true
        case Star(x) => recur(x) && x.nonNullable
        case Cat(x, y) => recur(x) && recur(y)
        //case Inter(xs) => xs.forall(recur(_))
        case Choice(x, y) => recur(x) && recur(y)
      }
    recur(epsilonForm)
  }

  def firstSet: Set[Char] =
    this match {
      case Epsilon => Set.empty
      case Letter(c) => Set(c)
      case Star(r) => r.firstSet
      case Choice(x, y) => x.firstSet | y.firstSet
      // case Inter(rs) if rs.isEmpty => Set.empty
      // case Inter(rs) => rs.map(_.firstSet).reduceLeft(_ & _)
      case Cat(x, y) if x.isNullable => x.firstSet | y.firstSet
      case Cat(x, y) => x.firstSet
    }

  def epsilonForm: Re =
    this match {
      case Star(x) if x.isEmpty => Epsilon
      case Star(x) => Re.star(x.epsilonForm)
      case Cat(x, y) =>
        if (x.isEmpty) y.epsilonForm
        else if (y.isEmpty) x.epsilonForm
        else Re.cat(x.epsilonForm, y.epsilonForm)
      case Choice(x, y) =>
        if (x.isEmpty && y.isEmpty) Epsilon
        else Re.choice(x.epsilonForm, y.epsilonForm)
      // case Inter(Nil) => Epsilon
      // case Inter(rs) =>
      //   rs.filter(_ != Epsilon).map(_.epsilonForm) match {
      //     case Nil => Epsilon
      //     case rs => Inter(rs)
      //   }
      case r => r
    }

  def headerForm: Re =
    this match {
      case Cat(Cat(r1, r2), r3) => Cat(r1, (Cat(r2, r3))).headerForm
      case Cat(Epsilon, r) => r.headerForm
      case Choice(_, _) | Star(_) | Letter(_) /* | Inter(_) */ => Cat(this, Epsilon)
      case r => r
    }

  def isLetter: Boolean =
    this match {
      case Letter(_) => true
      case _ => false
    }
}

sealed abstract class Included { lhs =>

  import Included._

  def &(rhs: Included): Included =
    (lhs, rhs) match {
      case (IsIncluded, IsIncluded) => IsIncluded
      case (OneAmbiguous, _) => OneAmbiguous
      case (_, OneAmbiguous) => OneAmbiguous
      case (NotIncluded, _) => NotIncluded
      case (_, NotIncluded) => NotIncluded
    }
}

object Included {
  case object IsIncluded extends Included
  case object OneAmbiguous extends Included
  case object NotIncluded extends Included
}

sealed abstract class Inclusion {

  import Included._
  import Inclusion._

  def isIncluded: Included =
    this match {
      case NotInclusion(_, _, _) => NotIncluded
      case Not1Unambiguous(_) => OneAmbiguous
      case Loop(_, _) => IsIncluded
      case Axm(_) => IsIncluded
      case LetterRule(_, _, _, i) => i.isIncluded
      case LetterStar(_, _, _, _, i) => i.isIncluded
      case LetterChoice2(_, _, _, _, _, i) => i.isIncluded
      case LetterChoice3(_, _, _, _, _, i) => i.isIncluded
      case LeftChoice(_, _, _, _, i1, i2) => i1.isIncluded & i2.isIncluded
      case LeftStar(_, _, _, _, i1, i2) => i1.isIncluded & i2.isIncluded
      case StarStarE(_, _, i) => i.isIncluded
      case StarChoice1(_, _, _, _, _, i) => i.isIncluded
      case StarChoice2(_, _, _, _, _, i1, i2) => i1.isIncluded & i2.isIncluded
      case ElimCat(_, _, _, i) => i.isIncluded
    }
}

object Inclusion {
  case class Axm(r: Re) extends Inclusion
  case class LetterRule(c: Char, r1: Re, r2: Re, inc: Inclusion) extends Inclusion
  case class LetterStar(c: Char, r1: Re, r2: Re, r3: Re, inc: Inclusion) extends Inclusion
  case class LetterChoice2(c: Char, r1: Re, r2: Re, r3: Re, r4: Re, inc: Inclusion) extends Inclusion
  case class LetterChoice3(c: Char, r1: Re, r2: Re, r3: Re, r4: Re, inc: Inclusion) extends Inclusion
  case class LeftChoice(r1: Re, r2: Re, r3: Re, r4: Re, inc1: Inclusion, inc2: Inclusion) extends Inclusion
  case class LeftStar(r1: Re, r2: Re, r3: Re, r4: Re, inc1: Inclusion, inc2: Inclusion) extends Inclusion
  case class StarStarE(r1: Re, r2: Re, inc: Inclusion) extends Inclusion
  case class StarChoice1(r1: Re, r2: Re, r3: Re, r4: Re, r5: Re, inc: Inclusion) extends Inclusion
  case class StarChoice2(r1: Re, r2: Re, r3: Re, r4: Re, r5: Re, inc1: Inclusion, inc2: Inclusion) extends Inclusion
  case class ElimCat(r1: Re, r2: Re, r3: Re, inc: Inclusion) extends Inclusion
  case class Loop(r1: Re, r2: Re) extends Inclusion
  case class Not1Unambiguous(s: String) extends Inclusion
  case class NotInclusion(r1: Re, r2: Re, s: String) extends Inclusion
}
