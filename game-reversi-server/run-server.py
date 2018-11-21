#!/usr/bin/env python
import os
import time

print("Running stack server in a loop. Press Ctrl-C to reload.")
while True:
    os.system('stack exec -- game-reversi-server-exe --port 1456')
    print("Reloading. Press Crtl-C within 1 second again to exit.")

    try:
        time.sleep(1)
    except KeyboardInterrupt:
        print("Exiting...")
        break
    else:
        print("Restarting...")
