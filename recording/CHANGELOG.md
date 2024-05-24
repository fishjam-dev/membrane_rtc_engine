# Changelog

## 0.2.0-dev
* Synchronize tracks based on RTCP sender reports [#393](https://github.com/fishjam-dev/membrane_rtc_engine/pull/393)
* Fix setting last buffer timestamp [#394](https://github.com/jellyfish-dev/membrane_rtc_engine/pull/394)
* Fix rtcp synchronization [#397](https://github.com/fishjam-dev/membrane_rtc_engine/pull/397)
* Don't list S3 object if no files added in recording [#396](https://github.com/jellyfish-dev/membrane_rtc_engine/pull/396)

## 0.1.0
* Initial release [#356](https://github.com/jellyfish-dev/membrane_rtc_engine/pull/356)
* Add crash groups, fix adding nonexistent tracks to state, fix READMEs [#366](https://github.com/jellyfish-dev/membrane_rtc_engine/pull/366)
* Add working AWS storage [#369](https://github.com/jellyfish-dev/membrane_rtc_engine/pull/369)
* Refactor storage API [#375](https://github.com/jellyfish-dev/membrane_rtc_engine/pull/375)
* Update deps [#374](https://github.com/jellyfish-dev/membrane_rtc_engine/pull/374)
* Extend report created by RecordingEndpoint [#376](https://github.com/jellyfish-dev/membrane_rtc_engine/pull/376)
* Add cleanup function and metrics to s3 sink [#378](https://github.com/jellyfish-dev/membrane_rtc_engine/pull/378)
* Save report only when it contains any track [#379](https://github.com/jellyfish-dev/membrane_rtc_engine/pull/379)
* Add recovery mechanism to s3 sink [#380](https://github.com/jellyfish-dev/membrane_rtc_engine/pull/380)
* Endpoint sends `:finished` after all input tracks unlink and update the use of Engine.subscribe [#381](https://github.com/jellyfish-dev/membrane_rtc_engine/pull/381)
* Use engine manual and auto subscribe mode. [#383](https://github.com/jellyfish-dev/membrane_rtc_engine/pull/383)
* Fix membrane_core version used in mixfile [#385](https://github.com/jellyfish-dev/membrane_rtc_engine/pull/385)
* Change use of ResourceGuard [#386](https://github.com/jellyfish-dev/membrane_rtc_engine/pull/386)
* Add origin to the report [#387](https://github.com/jellyfish-dev/membrane_rtc_engine/pull/387)
* Change s3 on close mechanism [#388](https://github.com/jellyfish-dev/membrane_rtc_engine/pull/388)
* Fix nils in recording report [#389](https://github.com/jellyfish-dev/membrane_rtc_engine/pull/389)