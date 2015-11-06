import datetime
import calendar
from examgen.examgen import exam, worksheet

today  = datetime.date.today()
todayf = str(str(today.year) + "_" + str(today.month) + "_" + str(today.day))

month   = calendar.month_name[today.month]
weekday = calendar.day_name[today.weekday()]

filename = str(todayf + "_algebra")
title    = str("Algebra practice for " + weekday + ", " + str(today.day) + " "  + month + ", " + str(today.year))


myexam = worksheet(filename, title)

# add some problem sections
# syntax: problem type, # of problems, section title, instructions
myexam.add_section("Linear equations", 10, "Linear equations",
                   "Solve the following equations for the specified variable.")
myexam.add_section("Quadratic equations", 10, "Quadratic equations",
                   "Solve the following quadratic equations.")

# generate the exam and solutions pdf
myexam.write()
