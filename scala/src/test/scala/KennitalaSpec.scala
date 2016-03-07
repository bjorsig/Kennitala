import com.koddsson.Kennitala
import org.scalatest._

class KennitalaSpec extends FlatSpec with Matchers {

  it should "be able to tell if it's valid" in {
    Kennitala.isValid("1111111119") should be (true)
    Kennitala.isValid("6503760649") should be (true)
  }

  it should "be able to tell if it's invalid" in {
    Kennitala.isValid("010101") should be (false)
    Kennitala.isValid("abcdefghij") should be (false)
    Kennitala.isValid("1709715049") should be (false)
  }

}
