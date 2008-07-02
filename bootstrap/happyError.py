#!/usr/bin/env python
from __future__ import with_statement
import re

def subst_calls(text):
    return re.sub(
        r'happyReduction_(\d+)([_ ]*)= notHappyAtAll',
        r'happyReduction_\1\2= notHappyAtAll \1',
        text
    )

def subst_definition(text):
    return re.sub(
        r'notHappyAtAll = error "Internal Happy error\n"',
        r'notHappyAtAll reduction = error $ "Internal Happy error in " ++ show reduction ++ "\n"',
        text
    )

def main():
    with open('Parser.hs') as infile:
        new_text = subst_calls(subst_definition(infile.read()))
    with open('Parser.hs', 'w') as outfile:
        outfile.write(new_text)

if __name__ == '__main__':
    main()
