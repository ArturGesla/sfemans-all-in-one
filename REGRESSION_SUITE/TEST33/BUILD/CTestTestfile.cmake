# CMake generated Testfile for 
# Source directory: /home/gesla/sfe2/REGRESSION_SUITE/TEST33
# Build directory: /home/gesla/sfe2/REGRESSION_SUITE/TEST33/BUILD
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
add_test(test33 "/bin/bash" "job.sh" "mpirun" "-n " "test33.exe")
set_tests_properties(test33 PROPERTIES  PASS_REGULAR_EXPRESSION "123" WORKING_DIRECTORY "/home/gesla/sfe2/REGRESSION_SUITE/TEST33/BUILD/../REGRESSION_TESTS" _BACKTRACE_TRIPLES "/home/gesla/sfe2/REGRESSION_SUITE/TEST33/CMakeLists.txt;71;add_test;/home/gesla/sfe2/REGRESSION_SUITE/TEST33/CMakeLists.txt;0;")
