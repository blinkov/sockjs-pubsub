sockjs-pubsub
=============

Publish/Subscribe for [SockJS](https://github.com/sockjs/sockjs-erlang)

# Planned features
* No history or persistance, for realtime messaging only
* No manual channel creation, if nobody is subscribed - message just discards


# Public API

    sps:subscribe(Channel, Conn).
    sps:publish(Channel, Message).

* **Channel** - arbitrary binary string
* **Conn** and **Message** - as in SockJS

