defmodule MIME do
  @moduledoc """
  Maps MIME types to its file extensions and vice versa.

  MIME types can be extended in your application configuration
  as follows:

      config :mime, :types, %{
        "application/vnd.api+json" => ["json-api"]
      }

  After adding the configuration, MIME needs to be recompiled.
  If you are using mix, it can be done with:

      $ mix deps.clean mime --build

  """

  types = %{
    "application/atom+xml" => ["atom"],
    "application/epub+zip" => ["epub"],
    "application/gzip" => ["gz"],
    "application/java-archive" => ["jar"],
    "application/javascript" => ["js"],
    "application/json" => ["json"],
    "application/json-patch+json" => ["json-patch"],
    "application/ld+json" => ["jsonld"],
    "application/manifest+json" => ["webmanifest"],
    "application/msword" => ["doc"],
    "application/octet-stream" => ["bin"],
    "application/ogg" => ["ogx"],
    "application/pdf" => ["pdf"],
    "application/postscript" => ["ps", "eps", "ai"],
    "application/rss+xml" => ["rss"],
    "application/rtf" => ["rtf"],
    "application/vnd.amazon.ebook" => ["azw"],
    "application/vnd.api+json" => ["json-api"],
    "application/vnd.apple.installer+xml" => ["mpkg"],
    "application/vnd.etsi.asic-e+zip" => ["asice", "sce"],
    "application/vnd.etsi.asic-s+zip" => ["asics", "scs"],
    "application/vnd.mozilla.xul+xml" => ["xul"],
    "application/vnd.ms-excel" => ["xls"],
    "application/vnd.ms-fontobject" => ["eot"],
    "application/vnd.ms-powerpoint" => ["ppt"],
    "application/vnd.oasis.opendocument.presentation" => ["odp"],
    "application/vnd.oasis.opendocument.spreadsheet" => ["ods"],
    "application/vnd.oasis.opendocument.text" => ["odt"],
    "application/vnd.openxmlformats-officedocument.presentationml.presentation" => ["pptx"],
    "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" => ["xlsx"],
    "application/vnd.openxmlformats-officedocument.wordprocessingml.document" => ["docx"],
    "application/vnd.rar" => ["rar"],
    "application/vnd.visio" => ["vsd"],
    "application/wasm" => ["wasm"],
    "application/x-7z-compressed" => ["7z"],
    "application/x-abiword" => ["abw"],
    "application/x-bzip" => ["bz"],
    "application/x-bzip2" => ["bz2"],
    "application/x-cdf" => ["cda"],
    "application/x-csh" => ["csh"],
    "application/x-freearc" => ["arc"],
    "application/x-httpd-php" => ["php"],
    "application/x-msaccess" => ["mdb"],
    "application/x-sh" => ["sh"],
    "application/x-shockwave-flash" => ["swf"],
    "application/x-tar" => ["tar"],
    "application/xhtml+xml" => ["xhtml"],
    "application/xml" => ["xml"],
    "application/zip" => ["zip"],
    "audio/3gpp" => ["3gp"],
    "audio/3gpp2" => ["3g2"],
    "audio/aac" => ["aac"],
    "audio/midi" => ["mid", "midi"],
    "audio/mpeg" => ["mp3"],
    "audio/ogg" => ["oga"],
    "audio/opus" => ["opus"],
    "audio/wav" => ["wav"],
    "audio/webm" => ["weba"],
    "font/otf" => ["otf"],
    "font/ttf" => ["ttf"],
    "font/woff" => ["woff"],
    "font/woff2" => ["woff2"],
    "image/avif" => ["avif"],
    "image/bmp" => ["bmp"],
    "image/gif" => ["gif"],
    "image/heic" => ["heic"],
    "image/heif" => ["heif"],
    "image/jpeg" => ["jpg", "jpeg"],
    "image/jxl" => ["jxl"],
    "image/png" => ["png"],
    "image/svg+xml" => ["svg", "svgz"],
    "image/tiff" => ["tiff", "tif"],
    "image/vnd.adobe.photoshop" => ["psd"],
    "image/vnd.microsoft.icon" => ["ico"],
    "image/webp" => ["webp"],
    "text/calendar" => ["ics"],
    "text/css" => ["css"],
    "text/csv" => ["csv"],
    "text/html" => ["html", "htm"],
    "text/javascript" => ["js", "mjs"],
    "text/markdown" => ["md", "markdown"],
    "text/plain" => ["txt", "text"],
    "text/xml" => ["xml"],
    "video/3gpp" => ["3gp"],
    "video/3gpp2" => ["3g2"],
    "video/mp2t" => ["ts"],
    "video/mp4" => ["mp4"],
    "video/mpeg" => ["mpeg", "mpg"],
    "video/ogg" => ["ogv"],
    "video/quicktime" => ["mov"],
    "video/webm" => ["webm"],
    "video/x-ms-wmv" => ["wmv"],
    "video/x-msvideo" => ["avi"]
  }

  require Application
  custom_types = Application.compile_env(:mime, :types, %{})

  to_exts = fn map ->
    for {media, exts} <- map, ext <- exts, reduce: %{} do
      acc -> Map.update(acc, ext, [media], &[media | &1])
    end
  end

  exts =
    Map.merge(to_exts.(types), %{
      "3g2" => ["video/3gpp2"],
      "3gp" => ["video/3gpp"],
      "js" => ["text/javascript"],
      "xml" => ["text/xml"]
    })

  for {ext, [_, _ | _] = mimes} <- exts do
    raise "conflicting MIMEs for extension .#{ext}, please override: #{inspect(mimes)}"
  end

  all_exts = Map.merge(exts, to_exts.(custom_types))
  all_types = Map.merge(types, custom_types)

  @doc """
  Returns the custom types compiled into the MIME module.
  """
  def compiled_custom_types do
    unquote(Macro.escape(custom_types))
  end

  @doc """
  Returns the extensions associated with a given MIME type.

  ## Examples

      iex> MIME.extensions("text/html")
      ["html", "htm"]

      iex> MIME.extensions("application/json")
      ["json"]

      iex> MIME.extensions("application/vnd.custom+xml")
      ["xml"]

      iex> MIME.extensions("foo/bar")
      []

  """
  @spec extensions(String.t()) :: [String.t()]
  def extensions(type) do
    mime =
      type
      |> strip_params()
      |> downcase("")

    mime_to_ext(mime) || suffix(mime) || []
  end

  defp suffix(type) do
    case String.split(type, "+") do
      [_type_subtype_without_suffix, suffix] -> [suffix]
      _ -> nil
    end
  end

  @default_type "application/octet-stream"

  @doc """
  Returns the MIME type associated with a file extension.

  If no MIME type is known for `file_extension`,
  `#{inspect(@default_type)}` is returned.

  ## Examples

      iex> MIME.type("txt")
      "text/plain"

      iex> MIME.type("foobarbaz")
      #{inspect(@default_type)}

  """
  @spec type(String.t()) :: String.t()
  def type(file_extension) do
    ext_to_mime(file_extension) || @default_type
  end

  @doc """
  Returns whether an extension has a MIME type registered.

  ## Examples

      iex> MIME.has_type?("txt")
      true

      iex> MIME.has_type?("foobarbaz")
      false

  """
  @spec has_type?(String.t()) :: boolean
  def has_type?(file_extension) do
    is_binary(ext_to_mime(file_extension))
  end

  @doc """
  Guesses the MIME type based on the path's extension. See `type/1`.

  ## Examples

      iex> MIME.from_path("index.html")
      "text/html"

  """
  @spec from_path(Path.t()) :: String.t()
  def from_path(path) do
    case Path.extname(path) do
      "." <> ext -> type(downcase(ext, ""))
      _ -> @default_type
    end
  end

  defp strip_params(string) do
    string |> :binary.split(";") |> hd()
  end

  defp downcase(<<h, t::binary>>, acc) when h in ?A..?Z,
    do: downcase(t, <<acc::binary, h + 32>>)

  defp downcase(<<h, t::binary>>, acc), do: downcase(t, <<acc::binary, h>>)
  defp downcase(<<>>, acc), do: acc

  @spec ext_to_mime(String.t()) :: String.t() | nil
  defp ext_to_mime(type)

  for {ext, [type | _]} <- all_exts do
    defp ext_to_mime(unquote(ext)), do: unquote(type)
  end

  defp ext_to_mime(_ext), do: nil

  @spec mime_to_ext(String.t()) :: list(String.t()) | nil
  defp mime_to_ext(type)

  for {type, exts} <- all_types do
    defp mime_to_ext(unquote(type)), do: unquote(List.wrap(exts))
  end

  defp mime_to_ext(_type), do: nil
end
