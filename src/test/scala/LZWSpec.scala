import org.scalacheck.Properties
import org.scalacheck.Prop.{forAll, BooleanOperators}
import org.scalacheck.Gen.alphaUpperStr

object LZWSpec extends Properties("LZW"){

  val pairGen = for{
    a <- alphaUpperStr
    b <- alphaUpperStr
  } yield (a,b)

  property("inverse") = forAll(alphaUpperStr){ (a:String) =>
    a == LZW.decode(LZW.encode(a))
  }

  property("injective function") = forAll(pairGen){case (a:String, b:String) =>
    (a != b) ==> (LZW.encode(a) != LZW.encode(b))
  }
}
