# Sending

Take tasks.md from the top. Items that share no file and do not need
each other's result may run at the same time. A waiting item's brief
carries the result it waited for in CONTEXT. Six is the subagents number
the preset sets: send the next ready item as one comes back, so the six
stay full while ready items remain.

Never poll a worker or a session — no loop of reads or sleeps. End the
turn and wait for the worker's result or the notification.
