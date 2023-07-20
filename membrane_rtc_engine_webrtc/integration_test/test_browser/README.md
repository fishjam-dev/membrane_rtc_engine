# TestBrowser

Standalone test application running a headless browser using Playwright. Extracted from the
`test_videoroom` app, used in a container for running integration against `membrane_rtc_engine`.

## Building the container
```bash
docker build -t test_browser .
```

For use in tests, run `docker compose build` in the [parent of this directory](../).
