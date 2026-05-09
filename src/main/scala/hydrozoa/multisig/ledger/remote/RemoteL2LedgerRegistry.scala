package hydrozoa.multisig.ledger.remote

import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.l1.tx.Tx.Serialized
import hydrozoa.multisig.ledger.joint.{EvacuationDiff, EvacuationKey}
import hydrozoa.multisig.ledger.joint.obligation.Payout
import hydrozoa.multisig.ledger.l2.given
import hydrozoa.multisig.ledger.l2.{Destination, L2LedgerCommand}
import hydrozoa.multisig.ledger.remote.RemoteL2Ledger.{Request, Response}
import io.bullet.borer.Cbor
import hydrozoa.multisig.ledger.event.RequestId
import registry.*
import registry.circe.*
import scala.util.Try
import scalus.cardano.address.{Address, ShelleyAddress}
import scalus.cardano.ledger.{AssetName, Coin, KeepRaw, MultiAsset, PolicyId, ScriptHash, TransactionOutput, Value}
import scalus.crypto.ed25519.VerificationKey
import scalus.uplc.builtin.{ByteString, Data}
import scodec.bits.ByteVector
import hydrozoa.lib.cardano.cip116.JsonCodecsRegistry.{decoderRegistry, encoderRegistry}

/** Registries of registry-circe codecs for the RemoteL2Ledger WebSocket protocol. Mirrors the
  * `RemoteL2LedgerCodecs` case class but threads `config: CardanoNetwork.Section` through
  * registries rather than via a constructor.
  *
  * Split into two registries — one for encoders, one for decoders — since encoder bodies depend
  * only on other encoders and likewise for decoders. Each codec body is preserved verbatim from the
  * legacy file so wire formats are byte-equivalent.
  */
object RemoteL2LedgerRegistry:

    def encoders(config: CardanoNetwork.Section): Registry[? <: Tuple, ? <: Tuple] =
        makeEncoder[Request] +:
            makeEncoder[Response] +:
            makeEncoder[Response.Success] +:
            makeEncoder[Response.Failure] +:
            makeEncoder[L2LedgerCommand.RegisterDeposit] +:
            makeEncoder[L2LedgerCommand.ApplyDepositDecisions] +:
            makeEncoder[L2LedgerCommand.ApplyTransaction] +:
            makeEncoder[L2LedgerCommand.ProxyBlockConfirmation] +:
            makeEncoder[L2LedgerCommand.ProxyRequestError] +:
            encodeVectorOf[EvacuationDiff] +:
            encodeVectorOf[Payout.Obligation] +:
            encodeListOf[RequestId] +:
            makeEncoder[EvacuationDiff] +:
            makeEncoder[Destination] +:
            encodeVectorOf[(RequestId, Serialized)] +:
            encodePairOf[RequestId, Serialized] +:
            makeEncoder[Value] +:
            encodeIArrayOf[Byte] +:
            makeEncoder(addressToString) +:
            encodeOptionOf[Data] +:
            value(dataEncoder) +:
            makeEncoder[Payout.Obligation] +:
            makeEncoder[EvacuationKey] +:
            makeEncoder((kr: KeepRaw[TransactionOutput]) => ByteString.fromArray(kr.raw).toHex) +:
            makeEncoder((_: Coin).value) +:
            makeEncoder((_: QuantizedInstant).instant.toEpochMilli) +:
            makeEncoder((_: BlockNumber).convert) +:
            makeEncoder((_: RequestId).asI64) +:
            makeEncoder((s: Serialized) => (s: ByteString)) +:
            value(registry.circe.Encoder[BigInt](b => io.circe.Json.fromBigInt(b))) +:
            encoderRegistry +:
            Encoder.primitives +:
            defaultEncoderOptions


    // `Address` renders as its bech32 string (or `toString` fallback for non-Shelley addresses)
    val addressToString: Address => String = {
        case s: ShelleyAddress => s.toBech32.get
        case other => other.toString
    }
    
    val dataEncoder: registry.circe.Encoder[Data] =
        registry.circe.Encoder(_ => io.circe.Json.Null)


    // -------------------------------------------------------------------------------------------
    // Decoder registry
    // -------------------------------------------------------------------------------------------

    /** Build the decoder registry. */
    def buildDecoderRegistry(config: CardanoNetwork.Section): Registry[? <: Tuple, ? <: Tuple] =

        // Reuse circe decoders from CIP-0116 (excluding Coin/Value which we override for sugar-rush format).
        import hydrozoa.lib.cardano.cip116.JsonCodecs.CIP0116.Conway.{coinEncoder as _, coinDecoder as _, valueEncoder as _, valueDecoder as _, given}

        // ---- Bespoke primitives (custom JSON shapes — not records) ----

        val regValueDecoder: registry.circe.Decoder[Value] =
            registry.circe.Decoder.jsonDecoderOf(using
              io.circe.Decoder.instance { c =>
                  c.downField("assets").as[List[io.circe.Json]].flatMap { assets =>
                      var coin = Coin(0)
                      val tokenMap = scala.collection.mutable
                          .Map[PolicyId, scala.collection.mutable.Map[AssetName, Long]]()

                      assets.foreach { assetEntry =>
                          val assetCursor = assetEntry.hcursor
                          val tag = assetCursor
                              .downField("asset")
                              .downField("tag")
                              .as[String]
                              .getOrElse("")
                          val value = assetCursor.downField("value").as[Long].getOrElse(0L)
                          tag match {
                              case "Ada" =>
                                  coin = Coin(value)
                              case "NativeToken" =>
                                  val policyIdHex = assetCursor
                                      .downField("asset")
                                      .downField("policyId")
                                      .as[String]
                                      .getOrElse("")
                                  val assetNameHex = assetCursor
                                      .downField("asset")
                                      .downField("assetName")
                                      .as[String]
                                      .getOrElse("")
                                  val policyId = ScriptHash.fromHex(policyIdHex)
                                  val assetName = AssetName.fromHex(assetNameHex)
                                  val innerMap =
                                      tokenMap
                                          .getOrElseUpdate(policyId, scala.collection.mutable.Map())
                                  innerMap(assetName) = value
                              case unknown =>
                                  Left(
                                    io.circe
                                        .DecodingFailure(s"Unknown asset tag: $unknown", c.history)
                                  )
                          }
                      }
                      val multiAsset = MultiAsset(
                        scala.collection.immutable.SortedMap.from(
                          tokenMap.view.mapValues(m => scala.collection.immutable.SortedMap.from(m))
                        )
                      )
                      Right(Value(coin, multiAsset))
                  }
              }
            )

        val regQuantizedInstantDecoder: registry.circe.Decoder[QuantizedInstant] =
            registry.circe.Decoder.long.map(_ =>
                throw new NotImplementedError(
                  "QuantizedInstant decoding requires SlotConfig context"
                )
            )

        val regDestinationDecoder: registry.circe.Decoder[Destination] =
            registry.circe.Decoder.jsonDecoderOf(using
              io.circe.Decoder.decodeString.emap(hexStr =>
                  for {
                      bytes <- ByteVector
                          .fromHex(hexStr)
                          .map(_.toArray)
                          .toRight(s"Invalid hex string: $hexStr")
                      dest <- Try(Cbor.decode(bytes).to[Destination].value).toEither.left.map(e =>
                          s"Could not cbor-decode the bytes: ${e}"
                      )
                  } yield dest
              )
            )

        val regKeepRawTxOutDecoder: registry.circe.Decoder[KeepRaw[TransactionOutput]] =
            registry.circe.Decoder.jsonDecoderOf(using
              io.circe.Decoder.instance { c =>
                  c.as[String].flatMap { hexStr =>
                      ByteString.fromHex(hexStr) match {
                          case bs =>
                              Try(
                                Cbor.decode(bs.bytes).to[TransactionOutput].value
                              ).toEither match {
                                  case Right(txOut) => Right(KeepRaw(txOut))
                                  case Left(e) =>
                                      Left(
                                        io.circe.DecodingFailure(
                                          s"Failed to decode TransactionOutput from CBOR: ${e.getMessage}",
                                          c.history
                                        )
                                      )
                              }
                      }
                  }
              }
            )
        // Payout.Obligation's body uses `c.as[KeepRaw[TransactionOutput]]` — keep the io.circe given.
        given io.circe.Decoder[KeepRaw[TransactionOutput]] = regKeepRawTxOutDecoder.asCirce

        val regEvacuationKeyDecoder: registry.circe.Decoder[EvacuationKey] =
            registry.circe.Decoder.jsonDecoderOf(using
              io.circe.Decoder.instance { c =>
                  summon[io.circe.Decoder[ByteString]].apply(c).flatMap { bytes =>
                      EvacuationKey(bytes) match {
                          case Some(key) => Right(key)
                          case None =>
                              Left(io.circe.DecodingFailure("Invalid EvacuationKey", c.history))
                      }
                  }
              }
            )

        val regPayoutObligationDecoder: registry.circe.Decoder[Payout.Obligation] =
            registry.circe.Decoder.jsonDecoderOf(using
              io.circe.Decoder.instance { c =>
                  for {
                      unvalidated <- c.as[KeepRaw[TransactionOutput]]
                      value <- Payout
                          .Obligation(unvalidated, config)
                          .left
                          .map(e => io.circe.DecodingFailure(e.toString, c.history))
                  } yield value
              }
            )


        // ---- Assemble the decoder registry ----

        // Case classes and sealed traits — auto-derived by registry-circe from their structure.
        makeDecoder[Request] *:
            makeDecoder[L2LedgerCommand.RegisterDeposit] *:
            makeDecoder[L2LedgerCommand.ApplyDepositDecisions] *:
            makeDecoder[L2LedgerCommand.ApplyTransaction] *:
            makeDecoder[L2LedgerCommand.ProxyBlockConfirmation] *:
            makeDecoder[L2LedgerCommand.ProxyRequestError] *:
            makeDecoder[Response] *:
            makeDecoder[Response.Success] *:
            makeDecoder[Response.Failure] *:
            makeDecoder[EvacuationDiff] *:
            // Bespoke wire-format decoders (fallible cursor/CBOR bodies — not simple maps)
            value(regValueDecoder) *:
            value(regQuantizedInstantDecoder) *:
            value(regDestinationDecoder) *:
            value(regKeepRawTxOutDecoder) *:
            value(regEvacuationKeyDecoder) *:
            value(regPayoutObligationDecoder) *:
            // Single-field newtype decoders derived from a registered primitive decoder
            map(Coin.apply) *:
            map(BlockNumber.apply) *:
            map(RequestId.fromI64) *:
            // Primitive decoders (Unit, String, Int, Long, Boolean, Double) the maps above
            // resolve through.
            registry.circe.Decoder.primitives *:
            decodeVectorOf[EvacuationDiff] *:
            decodeVectorOf[Payout.Obligation] *:
            // CIP-0116 primitive decoders
            decoderRegistry *:
            defaultDecoderOptions
