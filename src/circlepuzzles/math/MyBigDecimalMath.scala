package circlepuzzles.math

import java.math.BigDecimal

import scala.annotation.tailrec

/**
  * Operations on Java [[BigDecimal]]s to a fixed scale. In general, methods are not guaranteed to return the result
  * closest to the exact answer, but most computations will be correct for all but perhaps the last digit.
  *
  * Some methods compute functions that are not continuous or that are defined over a bounded range. In general, these
  * are computed on a best effort basis; a rounding error may lead to returning a value on the wrong side of a
  * discontinuity or just beyond a bound.
  */
object MyBigDecimalMath {
  /**
    * `BigDecimal` with value 2.
    */
  val Two = new BigDecimal(2)

  /**
    * `RoundingMode` for all computations.
    */
  val RoundingMode = java.math.RoundingMode.HALF_EVEN

  /**
    * pi/2 rounded to 1000 digits after the decimal point.
    */
  val HalfPi1000 = new BigDecimal( // Strings below are grouped in blocks of 100 digits
    "1.5707963267948966192313216916397514420985846996875529104874722961539082031431044993140174126710585339" +
      "9107404325664115332354692230477529111586267970406424055872514205135096926055277982231147447746519098" +
      "2214405487832966723064237824116893391582635600954572824283461730174305227163324106696803630124570636" +
      "8622935033031577940874407604604814146270458576821839462951800056652652744102332606920734759707558047" +
      "1652863518287979597654609305869096630589655255927403723118998137478367594287636244561396909150597456" +
      "4916836681220328321543010697473197612368595351089930471851385269608588146588376192337409233834702566" +
      "0002840635726317804138928856713788948045868185893607342204506124767150732747926855253961398446294617" +
      "7100997805606451098043201720907990681488738565498025935360567499999918648902497552986586640804815929" +
      "7512229727673454151321261154126672342517630965594085505001568919376443293766604190710308588834573651" +
      "7991267452143777343655797814319411768937968759788909288902660856134033065009639383055979546082100995"
  )

  /**
    * pi rounded to 1000 digits after the decimal point.
    */
  val Pi1000 = new BigDecimal( // Strings below are grouped in blocks of 100 digits
    "3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679" +
      "8214808651328230664709384460955058223172535940812848111745028410270193852110555964462294895493038196" +
      "4428810975665933446128475648233786783165271201909145648566923460348610454326648213393607260249141273" +
      "7245870066063155881748815209209628292540917153643678925903600113305305488204665213841469519415116094" +
      "3305727036575959195309218611738193261179310511854807446237996274956735188575272489122793818301194912" +
      "9833673362440656643086021394946395224737190702179860943702770539217176293176752384674818467669405132" +
      "0005681271452635608277857713427577896091736371787214684409012249534301465495853710507922796892589235" +
      "4201995611212902196086403441815981362977477130996051870721134999999837297804995105973173281609631859" +
      "5024459455346908302642522308253344685035261931188171010003137838752886587533208381420617177669147303" +
      "5982534904287554687311595628638823537875937519577818577805321712268066130019278766111959092164201989"
  )

  /**
    * 3*pi/2 rounded to 1000 digits after the decimal point.
    */
  val ThreeHalvesPi1000 = new BigDecimal(
    "4.7123889803846898576939650749192543262957540990626587314624168884617246094293134979420522380131756019" +
      "7322212976992345997064076691432587334758803911219272167617542615405290778165833946693442343239557294" +
      "6643216463498900169192713472350680174747906802863718472850385190522915681489972320090410890373711910" +
      "5868805099094733822623222813814442438811375730465518388855400169957958232306997820762204279122674141" +
      "4958590554863938792963827917607289891768965767782211169356994412435102782862908733684190727451792369" +
      "4750510043660984964629032092419592837105786053269791415554155808825764439765128577012227701504107698" +
      "0008521907178953412416786570141366844137604557680822026613518374301452198243780565761884195338883853" +
      "1302993416819353294129605162723972044466215696494077806081702499999755946707492658959759922414447789" +
      "2536689183020362453963783462380017027552892896782256515004706758129329881299812572130925766503720955" +
      "3973802356431332030967393442958235306813906279366727866707982568402099195028918149167938638246302984"
  )

  /**
    * 2*pi rounded to 1000 digits after the decimal point.
    */
  val TwoPi1000 = new BigDecimal(
    "6.2831853071795864769252867665590057683943387987502116419498891846156328125724179972560696506842341359" +
      "6429617302656461329418768921910116446345071881625696223490056820540387704221111928924589790986076392" +
      "8857621951331866892256951296467573566330542403818291297133846920697220908653296426787214520498282547" +
      "4491740132126311763497630418419256585081834307287357851807200226610610976409330427682939038830232188" +
      "6611454073151918390618437223476386522358621023709614892475992549913470377150544978245587636602389825" +
      "9667346724881313286172042789892790449474381404359721887405541078434352586353504769349636935338810264" +
      "0011362542905271216555715426855155792183472743574429368818024499068602930991707421015845593785178470" +
      "8403991222425804392172806883631962725954954261992103741442269999999674595609990211946346563219263719" +
      "0048918910693816605285044616506689370070523862376342020006275677505773175066416762841234355338294607" +
      "1965069808575109374623191257277647075751875039155637155610643424536132260038557532223918184328403979"
  )

  /**
    * Computes pi/2 to the desired scale. Only supported for up to 1000 digits after the decimal point.
    * @param scale Scale of the returned value. Requires `0 <= scale <= 1000`.
    * @return pi/2 rounded to the desired scale.
    * @throws IllegalArgumentException If `scale < 0` or `scale > 1000`.
    */
  def halfPi(scale: Int): BigDecimal = {
    if(0 <= scale && scale <= 1000) HalfPi1000.setScale(scale, RoundingMode)
    else throw new IllegalArgumentException("pi/2 only implemented to 1000 digits")
  }

  /**
    * Computes pi to the desired scale. Only supported for up to 1000 digits after the decimal point.
    * @param scale Scale of the returned value. Requires `0 <= scale <= 1000`.
    * @return pi rounded to the desired scale.
    * @throws IllegalArgumentException If `scale < 0` or `scale > 1000`.
    */
  def pi(scale: Int): BigDecimal = {
    if(0 <= scale && scale <= 1000) Pi1000.setScale(scale, RoundingMode)
    else throw new IllegalArgumentException("pi only implemented to 1000 digits")
  }

  /**
    * Computes 3*pi/2 to the desired scale. Only supported for up to 1000 digits after the decimal point.
    * @param scale Scale of the returned value. Requires `0 <= scale <= 1000`.
    * @return 3*pi/2 rounded to the desired scale.
    * @throws IllegalArgumentException If `scale < 0` or `scale > 1000`.
    */
  def threeHalvesPi(scale: Int): BigDecimal = {
    if(0 <= scale && scale <= 1000) ThreeHalvesPi1000.setScale(scale, RoundingMode)
    else throw new IllegalArgumentException("3*pi/2 only implemented to 1000 digits")
  }

  /**
    * Computes 2*pi to the desired scale. Only supported for up to 1000 digits after the decimal point.
    * @param scale Scale of the returned value. Requires `0 <= scale <= 1000`.
    * @return 2*pi rounded to the desired scale.
    * @throws IllegalArgumentException If `scale < 0` or `scale > 1000`.
    */
  def twoPi(scale: Int): BigDecimal = {
    if(0 <= scale && scale <= 1000) TwoPi1000.setScale(scale, RoundingMode)
    else throw new IllegalArgumentException("2*pi only implemented to 1000 digits")
  }

  /**
    * Computes the nonnegative square root of the argument, or zero if the argument is negative.
    * @param x Value whose square root is to be computed.
    * @param scale Scale of intermediate computations and the returned value.
    * @return Nonnegative square root of the argument (or zero if the argument is negative), rounded to the given scale.
    */
  def sqrt(x: BigDecimal, scale: Int): BigDecimal = {
    if(x.signum() <= 0) return BigDecimal.ZERO.setScale(scale)

    // This uses Newton's method. The initial seed is set using math.sqrt on the double value.
    val init = new BigDecimal(math.sqrt(x.doubleValue()))

    // Performs one iteration of Newton's method, or returns the result if it has converged.
    // This method preserves the invariant that the current best estimate has the desired scale.
    @tailrec
    def recursiveSqrt(curr: BigDecimal): BigDecimal = {
      // next = curr - term, where term = (curr^2 - x) / (2 * curr)
      val term = curr.pow(2).subtract(x).divide(Two.multiply(curr), scale, RoundingMode)
      if(term.signum() == 0) curr
      // Scale is preserved when we subtract if curr.scale == scale.
      else recursiveSqrt(curr.subtract(term))
    }

    // Set the scale of the initial value for loop invariant.
    recursiveSqrt(init.setScale(scale, RoundingMode))
  }

  /**
    * Computes the arc sine of the argument, or -pi/2 if it is less than -1, or pi/2 if it is greater than 1.
    * @param x Value whose arc sine is to be computed.
    * @param scale Scale of intermediate computations and the returned value.
    * @return Arc sine of the argument (or -pi/2 if `x < -1`, or pi/2 if `x > 1`), rounded to the given scale.
    */
  def asin(x: BigDecimal, scale: Int): BigDecimal = {
    val signum = x.signum()
    // If x < 0, use asin(x) = -asin(-x)
    if(signum < 0) asin(x.negate(), scale).negate()
    // If x = 0, return 0
    else if(signum == 0) BigDecimal.ZERO.setScale(scale)
    // If x >= 1, return pi/2
    else if(x.compareTo(BigDecimal.ONE) >= 0) halfPi(scale)
    // If 0 < x <= 0.5, use Taylor series at x = 0
    else if(x.doubleValue <= 0.5) {
      val xSquared = x.pow(2)

      // Adds one term from the Taylor series, or returns the result if it has converged.
      // This method preserves the invariant that the current best estimate and term have the desired scale.
      @tailrec
      def recursiveAsinTaylor(curr: BigDecimal, term: BigDecimal, iter: Int): BigDecimal = {
        val next = curr.add(term)
        val nextIter = iter + 1

        val numerator = (2 * nextIter - 1) * (2 * nextIter - 1)
        val denominator = 2 * nextIter * (2 * nextIter + 1)
        val nextTerm = term.multiply(xSquared)
          .multiply(new BigDecimal(numerator))
          .divide(new BigDecimal(denominator), scale, RoundingMode)

        if(nextTerm.signum() == 0) next
        else recursiveAsinTaylor(next, nextTerm, nextIter)
      }

      // Set the scale of the first term for loop invariant.
      // Don't need to set the scale of 0 because we immediately add the term.
      recursiveAsinTaylor(BigDecimal.ZERO, x.setScale(scale, RoundingMode), 0)
    }
    // If 0.5 < x < 1, use Puiseux series at x = 1
    else {
      val oneMinusX = BigDecimal.ONE.subtract(x)

      // Adds one term from the Puiseux series, or returns the result if it has converged.
      // This method preserves the invariant that the current best estimate and term have the desired scale.
      @tailrec
      def recursiveAsinPuiseux(curr: BigDecimal, term: BigDecimal, iter: Int): BigDecimal = {
        val next = curr.add(term)
        val nextIter = iter + 1

        val numerator = (2 * nextIter - 1) * (2 * nextIter - 1)
        val denominator = 4 * nextIter * (2 * nextIter + 1)
        val nextTerm = term.multiply(oneMinusX)
          .multiply(new BigDecimal(numerator))
          .divide(new BigDecimal(denominator), scale, RoundingMode)

        if(nextTerm.signum() == 0) next
        else recursiveAsinPuiseux(next, nextTerm, nextIter)
      }

      // Set the scale of the first estimate and term for loop invariant.
      recursiveAsinPuiseux(halfPi(scale), sqrt(Two.multiply(oneMinusX), scale).negate(), 0)
    }
  }

  /**
    * Computes the arc cosine of the argument, or pi if it is less than -1, or 0 if it is greater than 1.
    * @param x Value whose arc cosine is to be computed.
    * @param scale Scale of intermediate computations and the returned value.
    * @return Arc cosine of the argument (or pi if `x < -1`, or 0 if `x > 1`), rounded to the given scale.
    */
  def acos(x: BigDecimal, scale: Int): BigDecimal = {
    // acos(x) = asin(-x) + pi/2
    asin(x.negate(), scale).add(halfPi(scale))
  }

  /**
    * Computes the arc tangent of the argument.
    * @param x Value whose arc tangent is to be computed.
    * @param scale Scale of intermediate computations and the returned value.
    * @return Arc tangent of the argument, rounded to the given scale.
    */
  def atan(x: BigDecimal, scale: Int): BigDecimal = {
    // Use this identity instead of implementing a series for atan because atan series generally need something similar
    // for convergence near x = 1.
    // atan(x) = asin(asinArg) ; asinArg = x / sqrt(x^2 + 1)
    val asinArg = x.divide(sqrt(x.pow(2).add(BigDecimal.ONE), scale), scale, RoundingMode)
    asin(asinArg, scale)
  }

  /**
    * Computes the arc tangent of two arguments. This can also be seen as returning an angle
    * @param y Y-coordinate.
    * @param x X-coordinate.
    * @param scale Scale of intermediate computations and the returned value.
    * @return Arc tangent of the arguments, rounded to the given scale. The returned answer is in the range (-pi,pi].
    * @throws ArithmeticException If `y` and `x` are both zero.
    */
  def atan2(y: BigDecimal, x: BigDecimal, scale: Int): BigDecimal = {
    // See Wikipedia: https://en.wikipedia.org/wiki/Atan2#Definition_and_computation
    val signX = x.signum()
    val signY = y.signum()
    if(signX > 0) { // x positive
      atan(y.divide(x, scale, RoundingMode), scale)
    }
    else if(signX < 0) { // x negative
      if(signY >= 0) atan(y.divide(x, scale, RoundingMode), scale).add(pi(scale))
      else atan(y.divide(x, scale, RoundingMode), scale).subtract(pi(scale))

    }
    else { // x zero
      if(signY > 0) halfPi(scale)
      else if(signY < 0) halfPi(scale).negate()
      else throw new ArithmeticException("atan2(0,0)")
    }
  }

  /**
    * Computes the sine of the argument.
    * @param x Value whose sine is to be computed.
    * @param scale Scale of intermediate computations and the returned value.
    * @return Sine of the argument, rounded to the given scale.
    */
  def sin(x: BigDecimal, scale: Int): BigDecimal = {
    // TODO consider making this more efficient by calling cos or taking x mod 2*pi
    val xSquared = x.pow(2)

    // Adds one term from the Taylor series, or returns the result if it has converged.
    // This method preserves the invariant that the current best estimate and term have the desired scale.
    @tailrec
    def recursiveSin(curr: BigDecimal, term: BigDecimal, iter: Int): BigDecimal = {
      val next = curr.add(term)
      val nextIter = iter + 1

      val denominator = (2 * nextIter) * (2 * nextIter + 1)
      val nextTerm = term.negate()
        .multiply(xSquared)
        .divide(new BigDecimal(denominator), scale, RoundingMode)

      if(nextTerm.signum() == 0) {
        println(iter)
        next
      }
      else recursiveSin(next, nextTerm, nextIter)
    }

    // Set the scale of the first term for loop invariant.
    // Don't need to set the scale of 0 because we immediately add the term.
    recursiveSin(BigDecimal.ZERO, x.setScale(scale, RoundingMode), 0)
  }

  /**
    * Computes the cosine of the argument.
    * @param x Value whose cosine is to be computed.
    * @param scale Scale of intermediate computations and the returned value.
    * @return Cosine of the argument, rounded to the given scale.
    */
  def cos(x: BigDecimal, scale: Int): BigDecimal = {
    // TODO consider implementing a series for cos
    // cos(x) = sin(x + pi/2)
    sin(x.add(halfPi(scale)), scale)
  }
}
