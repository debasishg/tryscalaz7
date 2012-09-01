package net.debasishg.domain.trade
package model

/**
 * Created by IntelliJ IDEA.
 * User: debasish
 * Date: 23/12/10
 * Time: 6:06 PM
 * To change this template use File | Settings | File Templates.
 */

import scalaz._
import Lens._
import syntax.validation._
import syntax.applicative._
import syntax.std.option._
import syntax.std.function1._
import syntax.std.function2._
// import Scalaz._

import java.util.{Date, Calendar}

trait TradeModel {this: RefModel =>

  // the main domain class
  case class Trade(account: Account, instrument: Instrument, refNo: String, market: Market,
    unitPrice: BigDecimal, quantity: BigDecimal, tradeDate: Date = Calendar.getInstance.getTime, 
    valueDate: Option[Date] = None, taxFees: Option[List[(TaxFeeId, BigDecimal)]] = None, 
    netAmount: Option[BigDecimal] = None) {
    // override def equals(that: Any) = refNo == that.asInstanceOf[Trade].refNo
    // override def hashCode = refNo.hashCode
  }

  // various tax/fees to be paid when u do a trade
  sealed trait TaxFeeId
  case object TradeTax extends TaxFeeId
  case object Commission extends TaxFeeId
  case object VAT extends TaxFeeId
  case object Surcharge extends TaxFeeId

  // rates of tax/fees expressed as fractions of the principal of the trade
  val rates: Map[TaxFeeId, BigDecimal] = Map(TradeTax -> 0.2, Commission -> 0.15, VAT -> 0.1)

  // tax and fees applicable for each market
  // Other signifies the general rule
  val taxFeeForMarket: Map[Market, List[TaxFeeId]] = Map(Other -> List(TradeTax, Commission), Singapore -> List(TradeTax, Commission, VAT))

  // get the list of tax/fees applicable for this trade
  // depends on the market
  val forTrade: Trade => Option[List[TaxFeeId]] = {trade =>
    taxFeeForMarket.get(trade.market).orElse(taxFeeForMarket.get(Other)) 
  }

  def principal(trade: Trade) = trade.unitPrice * trade.quantity

  // combinator to value a tax/fee for a specific trade
  private[model] val valueAs: Trade => TaxFeeId => BigDecimal = {trade => {tid =>
    ((rates get tid) map (_ * principal(trade))) getOrElse (BigDecimal(0)) }}

  // all tax/fees for a specific trade
  val taxFeeCalculate: Trade => List[TaxFeeId] => List[(TaxFeeId, BigDecimal)] = {t => {tids =>
    tids zip (tids map valueAs(t))
  }}

  // validate quantity
  def validQuantity(qty: BigDecimal): Validation[String, BigDecimal] =
    if (qty <= 0) "qty must be > 0".failure
    else if (qty > 500) "qty must be <= 500".failure
    else qty.success

  // validate unit price
  def validUnitPrice(price: BigDecimal): Validation[String, BigDecimal] =
    if (price <= 0) "price must be > 0".failure
    else if (price > 100) "price must be <= 100".failure
    else price.success

  // using Validation as an applicative
  // can be combined to accumulate exceptions
  def makeTrade(account: Account, instrument: Instrument, refNo: String, market: Market,
    unitPrice: BigDecimal, quantity: BigDecimal) =
    (validUnitPrice(unitPrice).toValidationNEL |@|
      validQuantity(quantity).toValidationNEL) { (u, q) => Trade(account, instrument, refNo, market, u, q) }

  val enrichTradeWith: Trade => List[(TaxFeeId, BigDecimal)] => BigDecimal = {trade => {taxes =>
    taxes.foldLeft(principal(trade))((a, b) => a + b._2)
  }}

  /**
   * define a set of lenses for functional updation
   */

  // change ref no
  val refNoLens: Lens[Trade, String] = lensu(
      (t: Trade, r: String) => t.copy(refNo = r),
      (t: Trade) => t.refNo)

  // add tax/fees
  val taxFeeLens: Lens[Trade, Option[List[(TaxFeeId, BigDecimal)]]] = 
    lensu(
      (t: Trade, tfs: Option[List[(TaxFeeId, BigDecimal)]]) => t.copy(taxFees = tfs),
      (t: Trade) => t.taxFees) 

  // add net amount
  val netAmountLens: Lens[Trade, Option[BigDecimal]] = 
    lensu(
      (t: Trade, n: Option[BigDecimal]) => t.copy(netAmount = n),
      (t: Trade) => t.netAmount) 

  // add value date
  val valueDateLens: Lens[Trade, Option[Date]] = 
    lensu(
      (t: Trade, d: Option[Date]) => t.copy(valueDate = d),
      (t: Trade) => t.valueDate)

  /**
   * a set of closures
   */

  import Scalaz._

  // closure that enriches a trade
  val enrichTrade: Trade => Trade = {trade =>
    val taxes = for {
      taxFeeIds      <- forTrade // get the tax/fee ids for a trade
      taxFeeValues   <- taxFeeCalculate // calculate tax fee values
    }
    yield(taxFeeIds ∘ taxFeeValues)
    val t = taxFeeLens.set(trade, taxes(trade))
    netAmountLens.set(t, t.taxFees.map(_.foldLeft(principal(t))((a, b) => a + b._2)))
  }

  // closure for adding a value date
  val addValueDate: Trade => Trade = {trade =>
    val c = Calendar.getInstance
    c.setTime(trade.tradeDate)
    c.add(Calendar.DAY_OF_MONTH, 3)
    valueDateLens.set(trade, Some(c.getTime))
  }

}

object TradeModel extends TradeModel with ExecutionModel with OrderModel with RefModel with ContractNoteModel
