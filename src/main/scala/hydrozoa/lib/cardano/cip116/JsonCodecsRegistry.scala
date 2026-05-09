package hydrozoa.lib.cardano.cip116

import registry.*
import registry.circe.{keyDecoder, keyEncoder, jsonDecoder, jsonEncoder}
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.{AssetName, Coin, Hash32, MultiAsset, PolicyId, TransactionInput, Value}
import scalus.crypto.ed25519.{Signature, SigningKey, VerificationKey}
import scalus.uplc.builtin.ByteString

/**
 * Registry-backed encoders and decoders for CIP-0116 Conway data
 */
object JsonCodecsRegistry:

    import JsonCodecs.CIP0116.Conway.given

    lazy val encoderRegistry =
        jsonEncoder[VerificationKey] *:
            jsonEncoder[SigningKey] *:
            jsonEncoder[Signature] *:
            jsonEncoder[Hash32] *:
            jsonEncoder[AssetName] *:
            jsonEncoder[ShelleyAddress] *:
            jsonEncoder[ByteString] *:
            jsonEncoder[Coin] *:
            jsonEncoder[MultiAsset] *:
            jsonEncoder[Value] *:
            jsonEncoder[TransactionInput] *:
            keyEncoder[VerificationKey] *:
            keyEncoder[PolicyId] *:
            keyEncoder[AssetName]

    lazy val decoderRegistry=
        jsonDecoder[VerificationKey] *:
            jsonDecoder[SigningKey] *:
            jsonDecoder[Signature] *:
            jsonDecoder[Hash32] *:
            jsonDecoder[AssetName] *:
            jsonDecoder[ShelleyAddress] *:
            jsonDecoder[ByteString] *:
            jsonDecoder[Coin] *:
            jsonDecoder[MultiAsset] *:
            jsonDecoder[Value] *:
            jsonDecoder[TransactionInput] *:
            keyDecoder[VerificationKey] *:
            keyDecoder[PolicyId] *:
            keyDecoder[AssetName]
