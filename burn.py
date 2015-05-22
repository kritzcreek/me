#!/usr/bin/env python
#
# A little script to calculate my burn rate.
# Run with `python burn.py`
# I use it with Simple.com :)

reservoir = input("Safe-to-Spend: ")
burn_rate = input("Saving per day: ")
paycheck  = input("Days until next paycheck: ")

total_burn = float(burn_rate) * float(paycheck)

excess = float(reservoir) - total_burn

print("You have {} remaining this pay period!".format(excess))
