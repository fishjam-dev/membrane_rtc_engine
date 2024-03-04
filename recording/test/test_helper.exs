Application.put_env(:ex_aws, :http_client, ExAws.Request.HttpMock)
Mox.defmock(ExAws.Request.HttpMock, for: ExAws.Request.HttpClient)

ExUnit.start(capture_log: true, exclude: [:skip])
