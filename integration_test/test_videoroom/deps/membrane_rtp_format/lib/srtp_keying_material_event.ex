defmodule Membrane.SRTP.KeyingMaterialEvent do
  @moduledoc """
  Event containing keying material for SRTP encryptor and decryptor.
  """
  @derive Membrane.EventProtocol

  @typedoc """
  Type describing `#{inspect(__MODULE__)}` struct.

  Keying material consists of master key and master salt and is used
  by SRTP internally to generate actual keys.
  `local_keying_material` is used for deriving key for encryption while
  `remote_keying_material` is used for deriving key for decryption.
  For more information refer to RFC 3711 section 4.3 or RFC 5764 section 4.2.

  `protection_profile` is `pos_integer()` refering to cryptographic algorithm
  used for negotiating local and remote keying material.
  For more information refer to https://www.iana.org/assignments/srtp-protection/srtp-protection.xhtml
  """
  @type t :: %__MODULE__{
          local_keying_material: binary(),
          remote_keying_material: binary(),
          protection_profile: pos_integer()
        }

  @enforce_keys [:local_keying_material, :remote_keying_material, :protection_profile]
  defstruct @enforce_keys
end
