# CMake generated Testfile for 
# Source directory: /home/gesla/sfe2/REGRESSION_SUITE/TEST29
# Build directory: /home/gesla/sfe2/REGRESSION_SUITE/TEST29/BUILD
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
add_test(test29 "/bin/bash" "job.sh" "mpirun" "-n " "test29.exe")
set_tests_properties(test29 PROPERTIES  PASS_REGULAR_EXPRESSION "123" WORKING_DIRECTORY "/home/gesla/sfe2/REGRESSION_SUITE/TEST29/BUILD/../REGRESSION_TESTS" _BACKTRACE_TRIPLES "/home/gesla/sfe2/REGRESSION_SUITE/TEST29/CMakeLists.txt;71;add_test;/home/gesla/sfe2/REGRESSION_SUITE/TEST29/CMakeLists.txt;0;")
