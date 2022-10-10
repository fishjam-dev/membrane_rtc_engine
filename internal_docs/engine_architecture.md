# Engine Architecture

Engine itself is responsible for forwarding tracks to proper endpoints.
It is protocol agnostic i.e. it doesn't store information specific for 
any of multimedia protocols or standards.

For each track there is a separate tee responsible for
forwarding this track to all subscribed endpoints.

![Alt text](assets/engine_architecture.svg)






