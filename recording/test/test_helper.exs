Mox.defmock(ExAws.Request.HttpMock, for: ExAws.Request.HttpClient)
Application.put_env(:ex_aws, :http_client, ExAws.Request.HttpMock)

ExUnit.start(capture_log: true, exclude: [:skip])
