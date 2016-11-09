#!/bin/bash
find /home/papa/exact-real/ -name "*.o"  -print0 | xargs -r -0 /bin/rm
find /home/papa/exact-real/ -name "*.hi" -print0 | xargs -r -0 /bin/rm
