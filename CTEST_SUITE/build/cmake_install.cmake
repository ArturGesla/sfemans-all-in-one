# Install script for directory: /home/gesla/git/sfemans-all-in-one/CTEST_SUITE

# Set the install prefix
if(NOT DEFINED CMAKE_INSTALL_PREFIX)
  set(CMAKE_INSTALL_PREFIX "/usr/local")
endif()
string(REGEX REPLACE "/$" "" CMAKE_INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}")

# Set the install configuration name.
if(NOT DEFINED CMAKE_INSTALL_CONFIG_NAME)
  if(BUILD_TYPE)
    string(REGEX REPLACE "^[^A-Za-z0-9_]+" ""
           CMAKE_INSTALL_CONFIG_NAME "${BUILD_TYPE}")
  else()
    set(CMAKE_INSTALL_CONFIG_NAME "")
  endif()
  message(STATUS "Install configuration: \"${CMAKE_INSTALL_CONFIG_NAME}\"")
endif()

# Set the component getting installed.
if(NOT CMAKE_INSTALL_COMPONENT)
  if(COMPONENT)
    message(STATUS "Install component: \"${COMPONENT}\"")
    set(CMAKE_INSTALL_COMPONENT "${COMPONENT}")
  else()
    set(CMAKE_INSTALL_COMPONENT)
  endif()
endif()

# Install shared libraries without execute permission?
if(NOT DEFINED CMAKE_INSTALL_SO_NO_EXE)
  set(CMAKE_INSTALL_SO_NO_EXE "1")
endif()

# Is this installation the result of a crosscompile?
if(NOT DEFINED CMAKE_CROSSCOMPILING)
  set(CMAKE_CROSSCOMPILING "FALSE")
endif()

# Set default install directory permissions.
if(NOT DEFINED CMAKE_OBJDUMP)
  set(CMAKE_OBJDUMP "/usr/bin/objdump")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for each subdirectory.
  include("/home/gesla/git/sfemans-all-in-one/LIBS/cmake_install.cmake")
  include("/home/gesla/git/sfemans-all-in-one/REGRESSION_SUITE/TEST1/BUILD/cmake_install.cmake")
  include("/home/gesla/git/sfemans-all-in-one/REGRESSION_SUITE/TEST2/BUILD/cmake_install.cmake")
  include("/home/gesla/git/sfemans-all-in-one/REGRESSION_SUITE/TEST3/BUILD/cmake_install.cmake")
  include("/home/gesla/git/sfemans-all-in-one/REGRESSION_SUITE/TEST4/BUILD/cmake_install.cmake")
  include("/home/gesla/git/sfemans-all-in-one/REGRESSION_SUITE/TEST5/BUILD/cmake_install.cmake")
  include("/home/gesla/git/sfemans-all-in-one/REGRESSION_SUITE/TEST6/BUILD/cmake_install.cmake")
  include("/home/gesla/git/sfemans-all-in-one/REGRESSION_SUITE/TEST7/BUILD/cmake_install.cmake")
  include("/home/gesla/git/sfemans-all-in-one/REGRESSION_SUITE/TEST8/BUILD/cmake_install.cmake")
  include("/home/gesla/git/sfemans-all-in-one/REGRESSION_SUITE/TEST9/BUILD/cmake_install.cmake")
  include("/home/gesla/git/sfemans-all-in-one/REGRESSION_SUITE/TEST10/BUILD/cmake_install.cmake")
  include("/home/gesla/git/sfemans-all-in-one/REGRESSION_SUITE/TEST11/BUILD/cmake_install.cmake")
  include("/home/gesla/git/sfemans-all-in-one/REGRESSION_SUITE/TEST12/BUILD/cmake_install.cmake")
  include("/home/gesla/git/sfemans-all-in-one/REGRESSION_SUITE/TEST13/BUILD/cmake_install.cmake")
  include("/home/gesla/git/sfemans-all-in-one/REGRESSION_SUITE/TEST14/BUILD/cmake_install.cmake")
  include("/home/gesla/git/sfemans-all-in-one/REGRESSION_SUITE/TEST15/BUILD/cmake_install.cmake")
  include("/home/gesla/git/sfemans-all-in-one/REGRESSION_SUITE/TEST16/BUILD/cmake_install.cmake")
  include("/home/gesla/git/sfemans-all-in-one/REGRESSION_SUITE/TEST17/BUILD/cmake_install.cmake")
  include("/home/gesla/git/sfemans-all-in-one/REGRESSION_SUITE/TEST18/BUILD/cmake_install.cmake")
  include("/home/gesla/git/sfemans-all-in-one/REGRESSION_SUITE/TEST21/BUILD/cmake_install.cmake")
  include("/home/gesla/git/sfemans-all-in-one/REGRESSION_SUITE/TEST22/BUILD/cmake_install.cmake")
  include("/home/gesla/git/sfemans-all-in-one/REGRESSION_SUITE/TEST23/BUILD/cmake_install.cmake")
  include("/home/gesla/git/sfemans-all-in-one/REGRESSION_SUITE/TEST24/BUILD/cmake_install.cmake")
  include("/home/gesla/git/sfemans-all-in-one/REGRESSION_SUITE/TEST25/BUILD/cmake_install.cmake")
  include("/home/gesla/git/sfemans-all-in-one/REGRESSION_SUITE/TEST26/BUILD/cmake_install.cmake")
  include("/home/gesla/git/sfemans-all-in-one/REGRESSION_SUITE/TEST27/BUILD/cmake_install.cmake")
  include("/home/gesla/git/sfemans-all-in-one/REGRESSION_SUITE/TEST28/BUILD/cmake_install.cmake")
  include("/home/gesla/git/sfemans-all-in-one/REGRESSION_SUITE/TEST29/BUILD/cmake_install.cmake")
  include("/home/gesla/git/sfemans-all-in-one/REGRESSION_SUITE/TEST30/BUILD/cmake_install.cmake")
  include("/home/gesla/git/sfemans-all-in-one/REGRESSION_SUITE/TEST33/BUILD/cmake_install.cmake")
  include("/home/gesla/git/sfemans-all-in-one/REGRESSION_SUITE/TEST34/BUILD/cmake_install.cmake")
  include("/home/gesla/git/sfemans-all-in-one/REGRESSION_SUITE/TEST35/BUILD/cmake_install.cmake")
  include("/home/gesla/git/sfemans-all-in-one/REGRESSION_SUITE/TEST36/BUILD/cmake_install.cmake")
  include("/home/gesla/git/sfemans-all-in-one/REGRESSION_SUITE/TEST37/BUILD/cmake_install.cmake")
  include("/home/gesla/git/sfemans-all-in-one/REGRESSION_SUITE/TEST38/BUILD/cmake_install.cmake")
  include("/home/gesla/git/sfemans-all-in-one/REGRESSION_SUITE/TEST39/BUILD/cmake_install.cmake")
  include("/home/gesla/git/sfemans-all-in-one/REGRESSION_SUITE/TEST40/BUILD/cmake_install.cmake")
  include("/home/gesla/git/sfemans-all-in-one/REGRESSION_SUITE/TEST41/BUILD/cmake_install.cmake")
  include("/home/gesla/git/sfemans-all-in-one/REGRESSION_SUITE/TEST42/BUILD/cmake_install.cmake")

endif()

if(CMAKE_INSTALL_COMPONENT)
  set(CMAKE_INSTALL_MANIFEST "install_manifest_${CMAKE_INSTALL_COMPONENT}.txt")
else()
  set(CMAKE_INSTALL_MANIFEST "install_manifest.txt")
endif()

string(REPLACE ";" "\n" CMAKE_INSTALL_MANIFEST_CONTENT
       "${CMAKE_INSTALL_MANIFEST_FILES}")
file(WRITE "/home/gesla/git/sfemans-all-in-one/CTEST_SUITE/build/${CMAKE_INSTALL_MANIFEST}"
     "${CMAKE_INSTALL_MANIFEST_CONTENT}")
