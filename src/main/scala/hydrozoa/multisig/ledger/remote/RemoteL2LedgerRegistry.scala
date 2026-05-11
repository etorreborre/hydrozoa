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
import cats.syntax.all.catsSyntaxEither

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
            makeEncoder((s: Serialized) => s: ByteString) +:
            encoderRegistry +:
            Encoder.primitives +:
            defaultEncoderOptions

    // `Address` renders as its bech32 string (or `toString` fallback for non-Shelley addresses)
    val addressToString: Address => String = {
        case s: ShelleyAddress => s.toBech32.get
        case other             => other.toString
    }

    val dataEncoder: Encoder[Data] =
        Encoder(_ => io.circe.Json.Null)

    // -------------------------------------------------------------------------------------------
    // Decoders
    // -------------------------------------------------------------------------------------------

    def decoders(config: CardanoNetwork.Section) =
        makeDecoder[Request] +:
            makeDecoder[Response] +:
            makeDecoder[Response.Success] +:
            makeDecoder[Response.Failure] +:
            makeDecoder[L2LedgerCommand.RegisterDeposit] +:
            makeDecoder[L2LedgerCommand.ApplyDepositDecisions] +:
            makeDecoder[L2LedgerCommand.ApplyTransaction] +:
            makeDecoder[L2LedgerCommand.ProxyBlockConfirmation] +:
            makeDecoder[L2LedgerCommand.ProxyRequestError] +:
            decodeVectorOf[EvacuationDiff] +:
            decodeVectorOf[Payout.Obligation] +:
            decodeVectorOf[(RequestId, Serialized)] +:
            decodePairOf[RequestId, Serialized] +:
            decodeListOf[RequestId] +:
            makeDecoder[EvacuationDiff] +:
            fun(destinationDecoder) +:
            fun(byteArrayDecoder) +:
            fun(payoutObligationFromKeepRaw) +:
            fun(evacuationKeyDecoder) +:
            makeDecoder(KeepRaw.apply[TransactionOutput]) +:
            fun(cborDecoder[TransactionOutput]) +:
            map(Coin.apply) +:
            map(BlockNumber.apply) +:
            map(RequestId.fromI64) +:
            fun(valueDecoder) +:
            value(scriptHashDecoder) +:
            value(quantizedInstantDecoder) +:
            value(config) +:
            value(byteStringDecoder) +:
            decoderRegistry +:
            Decoder.primitives +:
            defaultDecoderOptions

    def valueDecoder(scriptHashDecoder: Decoder[ScriptHash], assetNameDecoder: Decoder[AssetName]): Decoder[Value] =
        Decoder { json =>
            val c = json.hcursor
            c.downField("assets")
                .as[List[io.circe.Json]]
                .flatMap { assets =>
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
                                for {
                                    policyId <- assetCursor.downField("asset").downField("policyId").decodeAs(scriptHashDecoder)
                                    assetName <- assetCursor.downField("asset").downField("assetName").decodeAs(assetNameDecoder)
                                    innerMap = tokenMap.getOrElseUpdate(policyId, scala.collection.mutable.Map())
                                }
                                 yield innerMap(assetName) = value
                            case unknown => Left(s"Unknown asset tag: $unknown")
                        }
                    }
                    val multiAsset = MultiAsset(
                      scala.collection.immutable.SortedMap.from(
                        tokenMap.view.mapValues(m => scala.collection.immutable.SortedMap.from(m))
                      )
                    )
                    Right(Value(coin, multiAsset))
                }
                .leftMap(e => e.toString)
        }

    extension (cursor: io.circe.ACursor)
        def decodeAs[A](decoder: Decoder[A]): Option[A] =
            cursor.focus.flatMap(v => decoder.decode(v).toOption)

    val scriptHashDecoder: Decoder[ScriptHash] = Decoder.string.map(ScriptHash.fromHex)
    val assetNameDecoder: Decoder[AssetName] = Decoder.string.map(AssetName.fromHex)

    // `ByteString` from a JSON hex string
    val byteStringDecoder: Decoder[ByteString] =
        Decoder.string.map(ByteString.fromHex)

    def cborDecoder[T](bytes: Decoder[ByteString])(using cborD: io.bullet.borer.Decoder[T]): Decoder[T] =
        bytes.emap(bs =>
            Cbor.decode(bs.bytes)
                .to[T]
                .valueEither
                .leftMap(e => s"Could not cbor-decode: ${e.getMessage}")
        )

    def destinationDecoder(bytes: Decoder[Array[Byte]]): Decoder[Destination] =
        bytes.emap(bs =>
            Try(Cbor.decode(bs).to[Destination].value).toEither.left.map(e =>
                s"Could not cbor-decode the bytes: ${e}"
            )
        )

    def byteArrayDecoder(string: Decoder[String]): Decoder[Array[Byte]] =
        string.emap(hex =>
            ByteVector.fromHex(hex).map(_.toArray).toRight(s"Invalid hex string: $hex")
        )

    def evacuationKeyDecoder(byteString: Decoder[ByteString]): Decoder[EvacuationKey] =
        byteString.emap { bytes =>
            EvacuationKey(bytes) match {
                case Some(key) => Right(key)
                case None      => Left("Invalid EvacuationKey")
            }
        }

    def payoutObligationFromKeepRaw(
        config: CardanoNetwork.Section,
        d: Decoder[KeepRaw[TransactionOutput]]
    ): Decoder[Payout.Obligation] =
        d.emap(kr => Payout.Obligation(kr, config).left.map(_.toString))

    val quantizedInstantDecoder: Decoder[QuantizedInstant] =
        Decoder.long.map(_ =>
            throw new NotImplementedError(
              "QuantizedInstant decoding requires SlotConfig context"
            )
        )
