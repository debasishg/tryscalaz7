package net.debasishg.domain.trade
package dsl

import scalaz._
import Kleisli._
import Scalaz._

object TradeDsl {

  import model.TradeModel._

  // enrichment of trade
  // implementation follows problem domain model
  // Reader monad
  val enrich = for {
    taxFeeIds      <- forTrade // get the tax/fee ids for a trade
    taxFeeValues   <- taxFeeCalculate // calculate tax fee values
    netAmount      <- enrichTradeWith // enrich trade with net amount
  }
  yield((taxFeeIds ∘ taxFeeValues) ∘ netAmount)

  // processes client orders in unstructured form and generates a list of Order objects
  val clientOrders: List[Map[String, String]] => List[Order] = {cos => fromClientOrders(cos)}

  // executes an Order in a market on behalf of a specific broker account
  // generates a sequence of Executions
  val execute: Market => Account => Order => List[Execution] = {market =>
    {brokerAccount =>
      {order =>
        order.items.map {item =>
          Execution(brokerAccount, item.ins, "e-123", market, item.price, item.qty)
        }
      }
    }
  }

  // allocates an execution to a List of client accounts
  // generates a List of trades
  val allocate: List[Account] => Execution => List[Validation[NonEmptyList[String], Trade]] = {accounts =>
    {execution =>
      val q = execution.quantity / accounts.size
      accounts.map {account =>
        makeTrade(account, execution.instrument, "t-123", execution.market, execution.unitPrice, q)
      }
    }
  }

  val generateContractNote: Trade => Validation[String, ContractNote] = {t =>
    makeContractNote(t)
  }

  def tradeGeneration(market: Market, broker: Account, clientAccounts: List[Account]) = {
    // client orders           executed at market by broker        & allocated to client accounts
    kleisli(clientOrders) >=> kleisli(execute(market)(broker)) >=> kleisli(allocate(clientAccounts))
  }
}
