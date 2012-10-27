sockjs-pubsub
=============

Publish/Subscribe for [SockJS](https://github.com/sockjs/sockjs-erlang)

## Features
* No history or persistance, for realtime messaging only
* No manual channel creation, if nobody is subscribed - message just discards


## Public API

		sps:start().
    sps:subscribe(Channel, Conn).
    sps:unsubscribe(Channel, Conn).
    sps:publish(Channel, Message).

* **Channel** - arbitrary binary string
* **Conn** and **Message** - as in SockJS

## Internals
* On each node application starts **24 managers** *(locally registered gen_servers with predefined names)* under **1 supervisor**.
* Binary channels are mapped to respective manager with **hash** function *(zlib:crc2/2)*.
* On subscribe/unsubscribe Conn is passed to local manager to be stored in **ETS table**. Each channel has it's own unnamed table. Table ids are looked up via private ETS master-table.
* On publish Message is **broadcasted** to respective manager on each connected erlang node with *gen_server:abcast/2*. Manager determines table id for channel and spawns short-lived process that will JSON-encode it and broadcast to all connections in channel ETS table. If this process stumbles upon dead connection it removes it.