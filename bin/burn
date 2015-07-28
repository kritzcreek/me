#!/usr/bin/env python
#
# burn.py - Calculate your burn rate
#
# A little script to calculate my burn rate. I use it with Simple.com :)
#
# Author: Ben Sima <bensima@gmail.com>
# License: MIT
#

import click # pip install click
from decimal import *

@click.command()
@click.option('--safe', prompt="Current Safe-to-Spend Resevoir", help="Safe to Spend")
@click.option('--burn', prompt="Your savings per day", help="Burn rate")
@click.option('--days', prompt="Days until your next paycheck", help="Days until next paycheck")
def burn_rate(safe: Decimal, burn: Decimal, days: Decimal) -> Decimal:
    """
    Calculates my burn rate based on current paycheck, current saving
    amount, and days until next paycheck.
    """
    money = Decimal(safe) - (Decimal(burn) * Decimal(days))
    color = "green" if money > 0 else "red"
    msg = "You have ${} ramaining this pay period!".format(click.style(str(money), fg = color))
    click.echo(msg)


if __name__ == '__main__':
    burn_rate()
