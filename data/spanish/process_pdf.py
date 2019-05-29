#!/usr/bin/env python3

from json import dumps
from pprint import pprint
import re

regex = re.compile('([a-záéíóúüñ ]*?)\sto\s*([a-zA-Záéíóúüñ,;\s’()]*)\s\(')
space = re.compile('\\s+')

defs = []

with open('./Kendris_501.Spanish.Verbs.Barrons.txt') as f:
    for (sp, en) in regex.findall(f.read()):
        if sp == '' or en == '':
            continue

        def f(s):
            return space.sub(' ', s)

        defs += [(f(sp), f(en))]

print(dumps(defs))
