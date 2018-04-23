
cmake_minimum_required(VERSION 2.8.12)
project(bdopt Fortran)

add_executable(bdopt
  modules.f90
  main.f90
  )

target_link_libraries(bdopt ${CMAKE_DL_LIBS})
set_property(TARGET bdopt PROPERTY LINKER_LANGUAGE Fortran)
