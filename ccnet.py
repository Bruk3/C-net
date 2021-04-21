#!/bin/python3

import getopt
import sys
import os
import subprocess

CC		= "gcc"			# C compiler
LLC		= "llc"			# LLVM compiler
CNET	= "cnet.native" # CNET compiler
LIBCNET = "libcnet/libcnet.a"

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
		def die(ret):
				print(ret.stderr.decode(), file=sys.stderr, end='')
				sys.exit(ret.returncode)



		# compile cnet to LLVM
		args = ["./" + CNET, "-c", sourcef]
		ret = subprocess.run(args, capture_output=True)
		if (ret.returncode != 0): die(ret)

		# compile LLVM to assembly
		args = [LLC]
		ret = subprocess.run(args, capture_output=True, input=ret.stdout)
		if (ret.returncode != 0): die(ret)

		# compile assembly to C
		tmpfile = "/tmp/" + sourcef + ".s"
		with open(tmpfile, "w") as tmp:
				tmp.write(ret.stdout.decode())

		args = [CC, tmpfile, LIBCNET, "-o", "/dev/stdout"]
		ret = subprocess.run(args, capture_output=True)

		if (ret.returncode != 0): die(ret)


		outputf = ""
		if 'o' in options: # user specified a file
				with open(options['o'], 'wb') as outfile:
						outfile.write(ret.stdout)
				sys.exit(0)
		# else:
		# 		outputf = sourcef.split('.')
		# 		with open(outputf, 'wb') as outfile:
		# 				outfile.write(ret.stdout)
		# 		sys.exit(0)


		sys.stdout.buffer.write(ret.stdout)
		# print(ret.stdout.decode(), end='')
		sys.exit(0)

		# print(ret)



def main():
		opts_l, source = getopt.getopt(sys.argv[1:], "taslbco:")
		# print(opts_l, source)

		options = dict()
		for opt,val in opts_l:
				options[opt[1:]] = val

		# print(options)

		if invalid_options(options, source):
				print(USAGEMSG)
				sys.exit(1)


		if "b" not in options and len(options) > 0: # not a full compilation
				handle_normal(options, source[0])
		else:
				handle_full(options, source[0])


main();