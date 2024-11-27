# CMake generated Testfile for 
# Source directory: /home/gesla/sfe2/REGRESSION_SUITE/TEST27
# Build directory: /home/gesla/sfe2/REGRESSION_SUITE/TEST27/BUILD
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
add_test(test27 "/bin/bash" "job.sh" "mpirun" "-n " "test27.exe")
set_tests_properties(test27 PROPERTIES  PASS_REGULAR_EXPRESSION "123" WORKING_DIRECTORY "/home/gesla/sfe2/REGRESSION_SUITE/TEST27/BUILD/../REGRESSION_TESTS" _BACKTRACE_TRIPLES "/home/gesla/sfe2/REGRESSION_SUITE/TEST27/CMakeLists.txt;71;add_test;/home/gesla/sfe2/REGRESSION_SUITE/TEST27/CMakeLists.txt;0;")
