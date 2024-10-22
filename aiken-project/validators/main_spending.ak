use aiken/collection/pairs
use cardano/transaction.{Datum, OutputReference, Redeemer, Transaction}
use env

validator main_spend {
  spend(
    _datum: Option<Datum>,
    _action: Redeemer,
    _own_out_ref: OutputReference,
    tx: Transaction,
  ) {
    when pairs.get_first(tx.withdrawals, env.observer_credential) is {
      Some(_) -> True
      None -> False
    }
  }

  else(_) {
    fail
  }
}
