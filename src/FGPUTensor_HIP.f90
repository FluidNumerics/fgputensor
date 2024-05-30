module FGPUTensor_HIP

!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! ==============================================================================
! hipfort: FORTRAN INTERFACEs for GPU kernels
! ==============================================================================
! Copyright (c) 2020-2022 Advanced Micro Devices, Inc. All rights reserved.
! [MITx11 License]
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in
! all copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
! THE SOFTWARE.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use iso_c_binding

  implicit none

  interface hipGetDeviceCount
    function hipGetDeviceCount_(count) bind(c,name="hipGetDeviceCount")
      use iso_c_binding
      use FGPUTensor_HIP_enums
      implicit none
      integer(kind(hipSuccess)) :: hipGetDeviceCount_
      integer(c_int) :: count
    end function
  end interface

  interface hipMalloc
    function hipMalloc_(ptr,mySize) bind(c,name="hipMalloc")
      use iso_c_binding
      use FGPUTensor_HIP_enums
      implicit none
      integer(kind(hipSuccess)) :: hipMalloc_
      type(c_ptr) :: ptr
      integer(c_size_t),value :: mySize
    end function
  end interface hipMalloc

  interface hipFree
    function hipFree_(ptr) bind(c,name="hipFree")
      use iso_c_binding
      use FGPUTensor_HIP_enums
      implicit none
      integer(kind(hipSuccess)) :: hipFree_
      type(c_ptr),value :: ptr
    end function
  end interface hipFree

  interface hipMemcpy
    function hipMemcpy_(dest,src,sizeBytes,myKind) bind(c,name="hipMemcpy")
      use iso_c_binding
      use FGPUTensor_HIP_enums
      implicit none
      integer(kind(hipSuccess)) :: hipMemcpy_
      type(c_ptr),value :: dest
      type(c_ptr),value :: src
      integer(c_size_t),value :: sizeBytes
      integer(kind(hipMemcpyHostToHost)),value :: myKind
    end function hipMemcpy_
  end interface hipMemcpy

  interface hipSetDevice
    function hipSetDevice_(deviceId) bind(c,name="hipSetDevice")
      use iso_c_binding
      use FGPUTensor_HIP_enums
      implicit none
      integer(kind(hipSuccess)) :: hipSetDevice_
      integer(c_int),value :: deviceId
    end function hipSetDevice_
  end interface hipSetDevice

contains

  subroutine hipCheck(hipError_t)
    use FGPUTensor_HIP_enums
    implicit none
    integer(kind(hipSuccess)) :: hipError_t

    if (hipError_t /= hipSuccess) then
      write (*,*) "HIP ERROR: Error code = ",hipError_t
      call exit(hipError_t)
    end if
  end subroutine hipCheck

end module FGPUTensor_HIP
