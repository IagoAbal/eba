#!/usr/bin/env python3

import sys
import os
import subprocess

with open('previous_tests.txt', 'r') as fp:
	for line in fp:
		commit_hash = line.split(' ')[0].strip()
		bug_file = line.split(' ')[1].strip()
		bug_file_i = bug_file.replace('.c', '.i').strip()
		working_directory = os.getcwd()
		os.chdir(working_directory)
		to_run = f"./make-and-evaluate.sh {commit_hash} {bug_file_i}"
		print(f"Running '{to_run}'..")
		subprocess.call(to_run, shell=True)
		
