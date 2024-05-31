# FGPUTensor
Copyright 2024 Fluid Numerics LLC

[![Build Status](https://github.com/fluidnumerics/fgputensor/actions/workflows/linux-gnu-cmake.yml/badge.svg)](https://github.com/FluidNumerics/fgputensor/actions/workflows/linux-gnu-cmake.yml)
[![Build Status](https://github.com/fluidnumerics/fgputensor/actions/workflows/linux-intel-cmake.yml/badge.svg)](https://github.com/FluidNumerics/fgputensor/actions/workflows/linux-intel-cmake.yml)
[![codecov](https://codecov.io/gh/FluidNumerics/fgputensor/graph/badge.svg?token=ZTCC9PFHOA)](https://codecov.io/gh/FluidNumerics/fgputensor)


`fgputensor` is an equation parser Fortran class that is used to interpret and evaluate functions provided as strings.
This library provides derived types to define "tensors" in Fortran that have CPU and GPU memory pointers.

CPU memory is defined through Fortran multi-dimensional `pointer` arrays
GPU memory is defined through `type(c_ptr)` from `iso_c_binding`

Additional utilities are provided to make it a little easier to manage CPU and GPU data in Fortran.


## Installation
`fgputensor` can be installed using CMake.

### Prerequisites
All you need is a Fortran compiler that is compliant with the Fortran 2008 standard and supports C interoperability. Additionally, the table below lists the [supported compilers](#supported-compilers). You will need to have CMake version 3.0.2 or greater.


### CMake
For a quick installation to `/usr/local/fgputensor`,
```
cd build/
cmake ../
make
sudo make install
```
If you'd like to run the provided tests to verify your installation, simply run `ctest` while in the `build/` subdirectory.


## Supported Compilers

The following combinations are tested on the main branch of fgputensor:

Name | Version | Platform | Build System | Architecture
--- | --- | --- | --- | --- |
GNU Fortran | 9, 10, 11, 12 | Ubuntu 22.04.2 LTS | `cmake` | x86_64
Intel oneAPI (`ifx`)| 2023.2 | Ubuntu 22.04.2 LTS | `cmake` | x86_64
Intel oneAPI classic (`ifort`) | 2021.1 | Ubuntu 22.04.2 LTS | `cmake` | x86_64

## Usage

### Run examples
> [!NOTE]
> Examples are included in the `example/` subdirectory

Included examples
* `simple_2d_array.f90` - Creates a 2d float32 tensor with CPU and GPU data pointers.


### Simple example with Makefile
*Example Makefile*
```
FC = gfortran
FLIBS += -L/usr/local/fgputensor/lib -lfgputensor
FFLAGS += -I/usr/local/fgputensor/include

demo : fgputensor_demo.f90
	${FC} -c fgputensor_demo.f90 ${FFLAGS}
	${FC} fgputensor_demo.o ${FFLAGS} ${FLIBS} -o $@
```

*Example program*
```
program fgputensor_demo

  use FGPUTensor

  implicit none

  type(tensor_f32_r2) :: t

  call t % init((/100,100/),0)

  call t % info()

  call t % free()

end program fgputensor_demo
```

## Contributors

* (Maintainer) Joe Schoonover, Fluid Numerics LLC
