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
import io.circe.{Decoder, Encoder, KeyDecoder, KeyEncoder}
import registry.*
import registry.circe.*
import scalus.cardano.address.{Address, ShelleyAddress}
import scalus.cardano.ledger.{AssetName, Coin, KeepRaw, MultiAsset, PolicyId, ScriptHash, TransactionOutput, Value}
import scalus.crypto.ed25519.VerificationKey
import scalus.uplc.builtin.{ByteString, Data}
import scodec.bits.ByteVector
import hydrozoa.lib.cardano.cip116

object RemoteL2LedgerCodecs:

    case class Codecs(config: CardanoNetwork.Section) {
        given[A](using izumi.reflect.Tag[A]): Decoder[A] =
            RemoteL2LedgerCodecs.decoders(config).makeDecoder[A]

        given[A](using izumi.reflect.Tag[A]): Encoder[A] =
            RemoteL2LedgerCodecs.encoders(config).makeEncoder[A]
    }

    def encoders(config: CardanoNetwork.Section) =
        encoder[Request] +:
            encoder[Response] +:
            encoder[Response.Success] +:
            encoder[Response.Failure] +:
            encoder[L2LedgerCommand.RegisterDeposit] +:
            encoder[L2LedgerCommand.ApplyDepositDecisions] +:
            encoder[L2LedgerCommand.ApplyTransaction] +:
            encoder[L2LedgerCommand.ProxyBlockConfirmation] +:
            encoder[L2LedgerCommand.ProxyRequestError] +:
            encodeVectorOf[EvacuationDiff] +:
            encodeVectorOf[Payout.Obligation] +:
            encodeListOf[RequestId] +:
            encoder[EvacuationDiff] +:
            encoder[Destination] +:
            encodeVectorOf[(RequestId, Serialized)] +:
            encodePairOf[RequestId, Serialized] +:
            encoder[Value] +:
            encodeIArrayOf[Byte] +:
            encoder(addressToString) +:
            encodeOptionOf[Data] +:
            value(dataEncoder) +:
            encoder((o: Payout.Obligation) => o.utxo) +:
            encoder[EvacuationKey] +:
            encoder((kr: KeepRaw[TransactionOutput]) => ByteString.fromArray(kr.raw).toHex) +:
            encoder((_: Coin).value) +:
            encoder((_: QuantizedInstant).instant.toEpochMilli) +:
            encoder((_: BlockNumber).convert) +:
            encoder((_: RequestId).asI64) +:
            encoder((s: Serialized) => s: ByteString) +:
            value(evacuationKeyKeyEncoder) +:
            cip116.Codecs.encoders +:
            Encoders.primitives +:
            defaultEncoderOptions

    // `Address` renders as its bech32 string (or `toString` fallback for non-Shelley addresses)
    val addressToString: Address => String = {
        case s: ShelleyAddress => s.toBech32.get
        case other             => other.toString
    }

    val dataEncoder: Encoder[Data] =
        Encoder.instance(_ => io.circe.Json.Null)

    // -------------------------------------------------------------------------------------------
    // Decoders
    // -------------------------------------------------------------------------------------------

    def decoders(config: CardanoNetwork.Section) =
        decoder[Request] +:
            decoder[Response] +:
            decoder[Response.Success] +:
            decoder[Response.Failure] +:
            decoder[L2LedgerCommand.RegisterDeposit] +:
            decoder[L2LedgerCommand.ApplyDepositDecisions] +:
            decoder[L2LedgerCommand.ApplyTransaction] +:
            decoder[L2LedgerCommand.ProxyBlockConfirmation] +:
            decoder[L2LedgerCommand.ProxyRequestError] +:
            decodeVectorOf[EvacuationDiff] +:
            decodeVectorOf[Payout.Obligation] +:
            decodeVectorOf[(RequestId, Serialized)] +:
            decodePairOf[RequestId, Serialized] +:
            decodeListOf[RequestId] +:
            decoder[EvacuationDiff] +:
            decoder(cborDecoder[Destination]) +:
            decoder(byteArrayDecoder) +:
            decoder(payoutObligation) +:
            decoder(evacuationKeyDecoder) +:
            decoder(KeepRaw.apply[TransactionOutput]) +:
            decoder(cborDecoder[TransactionOutput]) +:
            decoder[Coin] +:
            decoder[BlockNumber] +:
            decoder(RequestId.fromI64) +:
            decoder(valueDecoder) +:
            decoder(quantizedInstantDecoder) +:
            decoder(scriptHashDecoder) +:
            decoder(byteStringDecoder) +:
            value(evacuationKeyKeyDecoder) +:
            value(config) +:
            cip116.Codecs.decoders +:
            Decoders.primitives +:
            defaultDecoderOptions

    def valueDecoder(scriptHashDecoder: Decoder[ScriptHash], assetNameDecoder: Decoder[AssetName]): Decoder[Value] =
        Decoder.instance { c =>
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
        }

    val scriptHashDecoder: Decoder[ScriptHash] = Decoders.string.map(ScriptHash.fromHex)
    val assetNameDecoder: Decoder[AssetName] = Decoders.string.map(AssetName.fromHex)

    // `ByteString` from a JSON hex string
    val byteStringDecoder: Decoder[ByteString] =
        Decoders.string.map(ByteString.fromHex)

    def cborDecoder[T](bytes: Decoder[ByteString])(using cborD: io.bullet.borer.Decoder[T]): Decoder[T] =
        bytes.emap(bs =>
            Cbor.decode(bs.bytes)
                .to[T]
                .valueEither
                .left.map(e => s"Could not cbor-decode: ${e.getMessage}")
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

    def evacuationKeyKeyEncoder: KeyEncoder[EvacuationKey] =
        KeyEncoder.encodeKeyString.contramap(_.byteString.toHex)

    def evacuationKeyKeyDecoder: KeyDecoder[EvacuationKey] =
        KeyDecoder.instance { hex =>
            for {
                bytes <- scala.util.Try(ByteString.fromHex(hex)).toOption
                ek <- EvacuationKey(bytes)
            } yield ek
        }

    def payoutObligation(
        config: CardanoNetwork.Section,
        d: Decoder[KeepRaw[TransactionOutput]]
    ): Decoder[Payout.Obligation] =
        d.emap(kr => Payout.Obligation(kr, config).left.map(_.toString))

    val quantizedInstantDecoder: Decoder[QuantizedInstant] =
        Decoders.long.map(_ =>
            throw new NotImplementedError(
              "QuantizedInstant decoding requires SlotConfig context"
            )
        )
