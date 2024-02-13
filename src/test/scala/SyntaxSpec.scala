import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SyntaxSpec extends AnyFlatSpec with Matchers {
  behavior of "type class syntax"

  import me.chuwy.otusbats.Monad._

  it should "return flattened List instance given the \"point-map-flatMap\" pipeline from the List monad" in {
    val x: List[Int] = 1.point[List]
                        .mapFromFunctor { _ + 5 }
                        .flatMapFromMonad { x => (1 until x).toList }

    x shouldBe List(1, 2, 3, 4, 5)
  }

  it should "return flattened Option instance given the \"point-map-flatMap\" pipeline from the Option monad" in {
    val y: Option[Boolean] = "foo".point[Option]
                                  .mapFromFunctor { _ + "bar" }
                                  .flatMapFromMonad { _.toBooleanOption }

    y shouldBe Option.empty[Boolean]
  }

  it should "return flattened RightBiasedEither instance given the \"point-map-flatMap\" pipeline from the respective monad" in {
    val z: RightBiasedEither[Int] = 1.point[RightBiasedEither]
                                     .mapFromFunctor { _ + 1 }
                                     .flatMapFromMonad { _ => Left("boo") }

    z shouldBe Left("boo").asInstanceOf[RightBiasedEither[Int]]
  }
}
