#!/usr/bin/env python
#
# A little script to calculate my burn rate.
# Run with `python burn.py`
# I use it with Simple.com :)

import click # pip install click
from decimal import *

@click.command()
@click.option('--safe', prompt="Current Safe-to-Spend Resevoir", help="Safe to Spend")
@click.option('--burn', prompt="Your savings per day", help="Burn rate")
@click.option('--days', prompt="Days until your next paycheck", help="Days until next paycheck")
def burn_rate(safe, burn, days):
    """
    Calculates my burn rate based on current paycheck, current saving
    amount, and days until next paycheck.
    """
    num  = Decimal(safe) - (Decimal(burn) * Decimal(days))
    color = "green" if num > 0 else "red"

    msg = "You have ${} ramaining this pay period!".format(click.style(str(num), fg = color))
    click.echo(msg)


if __name__ == '__main__':
    burn_rate()
