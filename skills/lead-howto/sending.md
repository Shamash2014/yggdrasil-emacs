# Sending

Take tasks.md from the top. Items that share no file and do not need
each other's result may run at the same time. A waiting item's brief
carries the result it waited for in CONTEXT. At most six workers out:
send the next ready item as one comes back, so the six stay full while
ready items remain.

A worker past its TIMEBOX with no result: stop it, say so, send the
next ready item; its notification wakes you — no polling.

Never poll a worker or a session — no loop of reads or sleeps. End the
turn and wait for the worker's result or the notification.
