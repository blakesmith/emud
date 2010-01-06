### emud - a mud server and lib

emud is mud server written in Erlang. It is my first non-trivial Erlang application.

### Goals

Right now emud is in it's infancy, meaning it doesn't do much other than send echos back and forth across a TCP socket. The goals of emud align pretty closely with the powerful features of Erlang. (After all, I want to get better at 'Thinking in Erlang').

1. Fault tolerant - when muds crash, it's no fun for anyone.
2. Distributed - I want there to be theoretically unlimited users, as emud should be able to scale horizontally by simply adding more server nodes.
3. Concurrent - All mud objects will be concurrent processes that send messages to each other.
4. Be strongly tested with Eunit - Testing is important to me, and I want it to be applicable for emud.

More to come!
