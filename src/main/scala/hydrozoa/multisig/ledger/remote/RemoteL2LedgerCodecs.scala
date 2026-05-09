package hydrozoa.multisig.ledger.remote

import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.joint.{EvacuationDiff, EvacuationKey}
import hydrozoa.multisig.ledger.joint.obligation.Payout
import hydrozoa.multisig.ledger.l2.{Destination, L2LedgerCommand}
import hydrozoa.multisig.ledger.remote.RemoteL2Ledger.{Request, Response}
import io.circe.{Decoder, Encoder}
import scalus.cardano.ledger.{Coin, KeepRaw, TransactionOutput, Value}

/**
 * JSON codecs for the RemoteL2Ledger WebSocket protocol.
 *
 * Backed by [[RemoteL2LedgerRegistry]] — each `given` is a thin adapter that resolves the
 * corresponding `registry.circe.Encoder[T]` / `Decoder[T]` from the registry and lifts it via
 * [[hydrozoa.lib.json.Boundary]] to the `io.circe` shape expected by call sites and http4s.
 *
 * This is a structural migration: codec bodies live in `RemoteL2LedgerRegistry` and are wire-format
 * identical to the previous hand-written versions.
 */
case class RemoteL2LedgerCodecs(config: CardanoNetwork.Section):

    private val encoderRegistry = RemoteL2LedgerRegistry.encoders(config)
    private val decoderRegistry = RemoteL2LedgerRegistry.buildDecoderRegistry(config)

    private inline def enc[T]: Encoder[T] =
        encoderRegistry.make[registry.circe.Encoder[T]].asCirce

    private inline def dec[T]: Decoder[T] =
        decoderRegistry.make[registry.circe.Decoder[T]].asCirce

    given Encoder[Coin] = enc[Coin]
    given Decoder[Coin] = dec[Coin]

    given Encoder[Value] = enc[Value]
    given Decoder[Value] = dec[Value]

    implicit val quantizedInstantEncoder: Encoder[QuantizedInstant] = enc[QuantizedInstant]
    implicit val quantizedInstantDecoder: Decoder[QuantizedInstant] = dec[QuantizedInstant]

    implicit val blockNumberEncoder: Encoder[BlockNumber] = enc[BlockNumber]
    implicit val blockNumberDecoder: Decoder[BlockNumber] = dec[BlockNumber]

    implicit val proxyBlockConfirmationEncoder: Encoder[L2LedgerCommand.ProxyBlockConfirmation] =
        enc[L2LedgerCommand.ProxyBlockConfirmation]
    implicit val proxyBlockConfirmationDecoder: Decoder[L2LedgerCommand.ProxyBlockConfirmation] =
        dec[L2LedgerCommand.ProxyBlockConfirmation]

    implicit val proxyRequestErrorEncoder: Encoder[L2LedgerCommand.ProxyRequestError] =
        enc[L2LedgerCommand.ProxyRequestError]
    implicit val proxyRequestErrorDecoder: Decoder[L2LedgerCommand.ProxyRequestError] =
        dec[L2LedgerCommand.ProxyRequestError]

    implicit val destinationEncoder: Encoder[Destination] = enc[Destination]
    implicit val destinationDecoder: Decoder[Destination] = dec[Destination]

    implicit val depositRegistrationEncoder: Encoder[L2LedgerCommand.RegisterDeposit] =
        enc[L2LedgerCommand.RegisterDeposit]
    implicit val depositRegistrationDecoder: Decoder[L2LedgerCommand.RegisterDeposit] =
        dec[L2LedgerCommand.RegisterDeposit]

    implicit val depositDecisionsEncoder: Encoder[L2LedgerCommand.ApplyDepositDecisions] =
        enc[L2LedgerCommand.ApplyDepositDecisions]
    implicit val depositDecisionsDecoder: Decoder[L2LedgerCommand.ApplyDepositDecisions] =
        dec[L2LedgerCommand.ApplyDepositDecisions]

    implicit val applyTransactionEncoder: Encoder[L2LedgerCommand.ApplyTransaction] =
        enc[L2LedgerCommand.ApplyTransaction]
    implicit val applyTransactionDecoder: Decoder[L2LedgerCommand.ApplyTransaction] =
        dec[L2LedgerCommand.ApplyTransaction]

    implicit val requestEncoder: Encoder[Request] = enc[Request]
    implicit val requestDecoder: Decoder[Request] = dec[Request]

    implicit val responseSuccessEncoder: Encoder[Response.Success] = enc[Response.Success]
    implicit val responseSuccessDecoder: Decoder[Response.Success] = dec[Response.Success]

    implicit val responseFailureEncoder: Encoder[Response.Failure] = enc[Response.Failure]
    implicit val responseFailureDecoder: Decoder[Response.Failure] = dec[Response.Failure]

    implicit val responseEncoder: Encoder[Response] = enc[Response]
    implicit val responseDecoder: Decoder[Response] = dec[Response]

    given Encoder[EvacuationKey] = enc[EvacuationKey]
    given Decoder[EvacuationKey] = dec[EvacuationKey]

    given Encoder[KeepRaw[TransactionOutput]] = enc[KeepRaw[TransactionOutput]]
    given Decoder[KeepRaw[TransactionOutput]] = dec[KeepRaw[TransactionOutput]]

    given Encoder[EvacuationDiff] = enc[EvacuationDiff]
    given evacuationDiffDecoder: Decoder[EvacuationDiff] = dec[EvacuationDiff]

    given payoutObligationEncoder: Encoder[Payout.Obligation] = enc[Payout.Obligation]
    given payoutObligationDecoder: Decoder[Payout.Obligation] = dec[Payout.Obligation]

    implicit val unitEncoder: Encoder[Unit] = enc[Unit]
    implicit val unitDecoder: Decoder[Unit] = dec[Unit]
