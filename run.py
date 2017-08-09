#!/usr/bin/python

from subprocess import call
import time
import sys

def build_command(compiler, proposal_version, case, test_type_namespace, disjunction_namespace):
	if disjunction_namespace == 'prop':
		disjunction_namespace = 'proposal'

	return [compiler,  
		'-xc++', 
		'-fsyntax-only', 
		'-std=c++1z', 
		'-ftemplate-depth=2048', 
		'test.cpp',  
		'-DPROPOSAL_VERSION=' + proposal_version,
		'-DCASE=' + case,
		'-DNAMESPACE=' + disjunction_namespace, 
		'-DTO_FIND_TYPE_NAMESPACE=' + test_type_namespace]


def run_test(compiler, proposal_version, case, disjunction_namespace, test_type_namespace):
	command = build_command(compiler, proposal_version, case, test_type_namespace, disjunction_namespace)
	start = time.time()
	call(command)
	end = time.time()
	return end - start


def run_tests(times, compiler, proposal_version, case, disjunction_namespace, test_type_namespace):
	sys.stdout.flush()
	sum = 0.0
	for i in range(0, times):
		sys.stdout.write("%s, \t%s, \t%s, \t%s, \t%s, \t[%d%%]   \r" % (compiler, proposal_version, case, disjunction_namespace, test_type_namespace, (100*i/times)))
		sys.stdout.flush()
		sum += run_test(compiler, proposal_version, case, disjunction_namespace, test_type_namespace)
	
	print(compiler + ', \t' + proposal_version + ', \t' + case + ', \t' + test_type_namespace + ', \t' + disjunction_namespace + ', \t' + str(sum / times))


TIMES_TO_RUN=500
CLANG='clang++-4.0'
GXX='g++-6'

run_tests(TIMES_TO_RUN, CLANG, 'fold', 'best_case',    'prop', 'light')
run_tests(TIMES_TO_RUN, CLANG, 'bools', 'best_case',    'prop', 'light')
run_tests(TIMES_TO_RUN, CLANG, 'fold', 'best_case',    'std',  'light')

run_tests(TIMES_TO_RUN, CLANG, 'fold', 'worse_case', 'prop', 'light')
run_tests(TIMES_TO_RUN, CLANG, 'bools', 'worse_case', 'prop', 'light')
run_tests(TIMES_TO_RUN, CLANG, 'fold', 'worse_case', 'std',  'light')

run_tests(TIMES_TO_RUN, CLANG, 'fold', 'best_case',    'prop', 'heavy')
run_tests(TIMES_TO_RUN, CLANG, 'bools', 'best_case',    'prop', 'heavy')
run_tests(TIMES_TO_RUN, CLANG, 'fold', 'best_case',    'std',  'heavy')

run_tests(TIMES_TO_RUN, CLANG, 'fold', 'worse_case', 'prop', 'heavy')
run_tests(TIMES_TO_RUN, CLANG, 'bools', 'worse_case', 'prop', 'heavy')
run_tests(TIMES_TO_RUN, CLANG, 'fold', 'worse_case', 'std',  'heavy')


run_tests(TIMES_TO_RUN, GXX, 'fold', 'best_case',    'prop', 'light')
run_tests(TIMES_TO_RUN, GXX, 'bools', 'best_case',    'prop', 'light')
run_tests(TIMES_TO_RUN, GXX, 'fold', 'best_case',    'std',  'light')

run_tests(TIMES_TO_RUN, GXX, 'fold', 'worse_case', 'prop', 'light')
run_tests(TIMES_TO_RUN, GXX, 'bools', 'worse_case', 'prop', 'light')
run_tests(TIMES_TO_RUN, GXX, 'fold', 'worse_case', 'std',  'light')

run_tests(TIMES_TO_RUN, GXX, 'fold', 'best_case',    'prop', 'heavy')
run_tests(TIMES_TO_RUN, GXX, 'bools', 'best_case',    'prop', 'heavy')
run_tests(TIMES_TO_RUN, GXX, 'fold', 'best_case',    'std',  'heavy')

run_tests(TIMES_TO_RUN, GXX, 'fold', 'worse_case', 'prop', 'heavy')
run_tests(TIMES_TO_RUN, GXX, 'bools', 'worse_case', 'prop', 'heavy')
run_tests(TIMES_TO_RUN, GXX, 'fold', 'worse_case', 'std',  'heavy')
