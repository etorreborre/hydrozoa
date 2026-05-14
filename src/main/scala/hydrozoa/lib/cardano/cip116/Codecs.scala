package hydrozoa.lib.cardano.cip116

import registry.*
import registry.circe.{decoderOf, encoderOf, keyDecoderOf, keyEncoderOf}
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.{AssetName, Coin, Hash32, MultiAsset, PolicyId, TransactionInput, Value}
import scalus.crypto.ed25519.{Signature, SigningKey, VerificationKey}
import scalus.uplc.builtin.ByteString

/**
 * Registry-backed encoders and decoders for CIP-0116 Conway data
 */
object Codecs:

    import JsonCodecs.CIP0116.Conway.given

    lazy val encoders =
        encoderOf[VerificationKey] *:
            encoderOf[SigningKey] *:
            encoderOf[Signature] *:
            encoderOf[Hash32] *:
            encoderOf[AssetName] *:
            encoderOf[ShelleyAddress] *:
            encoderOf[ByteString] *:
            encoderOf[Coin] *:
            encoderOf[MultiAsset] *:
            encoderOf[Value] *:
            encoderOf[TransactionInput] *:
            keyEncoderOf[VerificationKey] *:
            keyEncoderOf[PolicyId] *:
            keyEncoderOf[AssetName]

    lazy val decoders=
        decoderOf[VerificationKey] *:
            decoderOf[SigningKey] *:
            decoderOf[Signature] *:
            decoderOf[Hash32] *:
            decoderOf[AssetName] *:
            decoderOf[ShelleyAddress] *:
            decoderOf[ByteString] *:
            decoderOf[Coin] *:
            decoderOf[MultiAsset] *:
            decoderOf[Value] *:
            decoderOf[TransactionInput] *:
            keyDecoderOf[VerificationKey] *:
            keyDecoderOf[PolicyId] *:
            keyDecoderOf[AssetName]
