cmake_minimum_required(VERSION 3.28)
project(CryptoMetricsUbuntu)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_BUILD_TYPE Release)

# paths for external dependencies
set(LIBOQS_DIR "${CMAKE_SOURCE_DIR}/external/liboqs/build")
set(OPENSSL_DIR "${CMAKE_SOURCE_DIR}/external/openssl")

# INCLUDE LIBOQS AND OPENSSL
include_directories(${LIBOQS_DIR}/include ${OPENSSL_DIR}/include)

# LIB LIBOQS AND OPENSSL
link_directories(${LIBOQS_DIR}/lib ${OPENSSL_DIR}/lib)

#FIND LIBOQS, LIBSSL and LIBCRYPTO
find_library(LIBOQS oqs PATHS ${LIBOQS_DIR}/lib REQUIRED)
find_library(LIBSSL ssl PATHS ${OPENSSL_DIR}/lib REQUIRED)
find_library(LIBCRYPTO crypto PATHS ${OPENSSL_DIR}/lib REQUIRED)

if(NOT LIBSSL OR NOT LIBCRYPTO)
    message(FATAL_ERROR "Could not find OpenSSL libraries. Check installation!!!")
endif()

# create  executable for Kyber
add_executable(kyber src/kyber.cpp)
target_link_libraries(kyber ${LIBOQS} ${LIBSSL} ${LIBCRYPTO})

# create executable for ECDH
add_executable(ecdh src/ecdh.cpp)
target_link_libraries(ecdh ${LIBOQS} ${LIBSSL} ${LIBCRYPTO})

# create    executable for Kyber RAM 
add_executable(kyber_ram src/kyber_ram.cpp)
target_link_libraries(kyber_ram ${LIBOQS} ${LIBSSL} ${LIBCRYPTO})

# create executable for ECDH RAM
add_executable(ecdh_ram src/ecdh_ram.cpp)
target_link_libraries(ecdh_ram ${LIBOQS} ${LIBSSL} ${LIBCRYPTO})

