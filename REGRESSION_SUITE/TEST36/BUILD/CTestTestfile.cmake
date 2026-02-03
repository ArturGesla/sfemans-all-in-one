# CMake generated Testfile for 
# Source directory: /home/gesla/git/sfemans-all-in-one/REGRESSION_SUITE/TEST36
# Build directory: /home/gesla/git/sfemans-all-in-one/REGRESSION_SUITE/TEST36/BUILD
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
add_test(test36 "/bin/bash" "job.sh" "mpirun" "-n " "test36.exe")
set_tests_properties(test36 PROPERTIES  PASS_REGULAR_EXPRESSION "123" WORKING_DIRECTORY "/home/gesla/git/sfemans-all-in-one/REGRESSION_SUITE/TEST36/BUILD/../REGRESSION_TESTS" _BACKTRACE_TRIPLES "/home/gesla/git/sfemans-all-in-one/REGRESSION_SUITE/TEST36/CMakeLists.txt;71;add_test;/home/gesla/git/sfemans-all-in-one/REGRESSION_SUITE/TEST36/CMakeLists.txt;0;")
