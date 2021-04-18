#!/bin/python3

import getopt
import sys
import os

CC		= "gcc"			# C compiler
LLC		= "llc"			# LLVM compiler
CNET	= "cnet.native" # CNET compiler
LIBCNET = "libcnet/*c"

USAGEMSG  = """USAGE: {} -(t|a|s|l|b) source """.format(sys.argv[0])

def invalid_options(options, source):
		if len(options) > 2: return True
		if len(source) != 1: return True
		if len(options) == 2 and "o" not in options: return True

		return False




def handle_normal(options, sourcef):
		if 'o' in options: # user specified a file
				pass

		option = list(options.keys())[0]

		arg1 = "-" + option
		os.execl(CNET, CNET, arg1, sourcef)
		sys.exit(1)

def handle_full(options, sourcef):
		if 'o' in options: # user specified a file
				pass

		#



def main():
		opts_l, source = getopt.getopt(sys.argv[1:], "taslb")
		# print(opts_l, source)

		options = dict()
		for opt,val in opts_l:
				options[opt[1:]] = val

		# print(options)

		if invalid_options(options, source): print(USAGEMSG); sys.exit(1);


		if "b" not in options: # not a full compilation
				handle_normal(options, source[0])


main();
