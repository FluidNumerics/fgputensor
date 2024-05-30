# FGPUTensor

This library provides derived types to define "tensors" in Fortran that have CPU and GPU memory pointers.

CPU memory is defined through Fortran multi-dimensional `pointer` arrays
GPU memory is defined through `type(c_ptr)` from `iso_c_binding`

Additional utilities are provided to make it a little easier to manage CPU and GPU data in Fortran.
