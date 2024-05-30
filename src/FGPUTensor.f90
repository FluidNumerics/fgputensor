!
! Copyright 2024 Fluid Numerics LLC
! Author : Joseph Schoonover (joe@fluidnumerics.com)
! Support : support@fluidnumerics.com
!
! //////////////////////////////////////////////////////////////////////////////////////////////// !
module FGPUTensor

  use iso_fortran_env
  use iso_c_binding

  !#ifdef HIP
  use FGPUTensor_HIP
  use FGPUTensor_HIP_enums
  use FGPUTensor_Utils
  !#endif

  implicit none

  integer,parameter,public :: FGPUTensor_CPU_Device_ID = -1
  integer,parameter,public :: FGPUTensor_MaxDims = 7

  type,abstract :: tensor

    !! tensor object is the base object that all others are type extensions of
    !! At the bottom level, all tensor objects have a device id and c_ptr that
    !! points to device memory. The main distinction between specific encantations
    !! of a tensor is the rank of the host array.

    integer :: device_id = FGPUTensor_CPU_Device_ID
    type(c_ptr) :: gpu = c_null_ptr
    integer :: rank = 0
    integer,dimension(1:FGPUTensor_MaxDims) :: dims = 0
    logical :: inited = .false.
    logical :: allocd = .false.

  contains

    procedure,public :: init
    procedure,public :: info

    procedure(alloc),private,deferred :: alloc
    procedure(free),public,deferred :: free
    procedure(updategpu),public,deferred :: updategpu
    procedure(updategpu),public,deferred :: updatecpu

  end type tensor

  interface
    subroutine alloc(this,device_id)
      import tensor
      implicit none
      class(tensor),intent(inout) :: this
      integer,optional :: device_id
    end subroutine alloc
  end interface

  interface
    subroutine free(this)
      import tensor
      implicit none
      class(tensor),intent(inout) :: this
    end subroutine free
  end interface

  interface
    subroutine updategpu(this)
      import tensor
      implicit none
      class(tensor),intent(inout) :: this
    end subroutine updategpu
  end interface

  type,extends(tensor) :: tensor_f32_r1
  !! Data type for storing one-dimensional real arrays on the host and the device
    real(real32),pointer :: cpu(:)

  contains

    procedure,private :: alloc => alloc_tensor_f32_r1
    procedure,public :: free => free_tensor_f32_r1

    procedure,public :: updatecpu => updatecpu_tensor_f32_r1
    procedure,public :: updategpu => updategpu_tensor_f32_r1

  end type tensor_f32_r1

  type,extends(tensor) :: tensor_f32_r2
  !! Data type for storing two-dimensional real arrays on the host and the device
    real(real32),pointer :: cpu(:,:)

  contains

    procedure,private :: alloc => alloc_tensor_f32_r2
    procedure,public :: free => free_tensor_f32_r2

    procedure,public :: updatecpu => updatecpu_tensor_f32_r2
    procedure,public :: updategpu => updategpu_tensor_f32_r2

  end type tensor_f32_r2

  type,extends(tensor) :: tensor_f32_r3
  !! Data type for storing three-dimensional real arrays on the host and the device
    real(real32),pointer :: cpu(:,:,:)

  contains

    procedure,private :: alloc => alloc_tensor_f32_r3
    procedure,public :: free => free_tensor_f32_r3

    procedure,public :: updatecpu => updatecpu_tensor_f32_r3
    procedure,public :: updategpu => updategpu_tensor_f32_r3

  end type tensor_f32_r3

  type,extends(tensor) :: tensor_f32_r4
  !! Data type for storing four-dimensional real arrays on the host and the device
    real(real32),pointer :: cpu(:,:,:,:)

  contains

    procedure,private :: alloc => alloc_tensor_f32_r4
    procedure,public :: free => free_tensor_f32_r4

    procedure,public :: updatecpu => updatecpu_tensor_f32_r4
    procedure,public :: updategpu => updategpu_tensor_f32_r4

  end type tensor_f32_r4

  type,extends(tensor) :: tensor_f32_r5
  !! Data type for storing five-dimensional real arrays on the host and the device
    real(real32),pointer :: cpu(:,:,:,:,:)

  contains

    procedure,private :: alloc => alloc_tensor_f32_r5
    procedure,public :: free => free_tensor_f32_r5

    procedure,public :: updatecpu => updatecpu_tensor_f32_r5
    procedure,public :: updategpu => updategpu_tensor_f32_r5

  end type tensor_f32_r5

  type,extends(tensor) :: tensor_f32_r6
  !! Data type for storing one-dimensional real arrays on the host and the device
    real(real32),pointer :: cpu(:,:,:,:,:,:)

  contains

    procedure,private :: alloc => alloc_tensor_f32_r6
    procedure,public :: free => free_tensor_f32_r6

    procedure,public :: updatecpu => updatecpu_tensor_f32_r6
    procedure,public :: updategpu => updategpu_tensor_f32_r6

  end type tensor_f32_r6

  type,extends(tensor) :: tensor_f32_r7
  !! Data type for storing seven-dimensional real arrays on the host and the device
    real(real32),pointer :: cpu(:,:,:,:,:,:,:)

  contains

    procedure,private :: alloc => alloc_tensor_f32_r7
    procedure,public :: free => free_tensor_f32_r7

    procedure,public :: updatecpu => updatecpu_tensor_f32_r7
    procedure,public :: updategpu => updategpu_tensor_f32_r7

  end type tensor_f32_r7

  type,extends(tensor) :: tensor_i32_r1
  !! Data type for storing one-dimensional int32 arrays on the host and the device
    integer(int32),pointer :: cpu(:)

  contains

    procedure,private :: alloc => alloc_tensor_i32_r1
    procedure,public :: free => free_tensor_i32_r1

    procedure,public :: updatecpu => updatecpu_tensor_i32_r1
    procedure,public :: updategpu => updategpu_tensor_i32_r1

  end type tensor_i32_r1

  type,extends(tensor) :: tensor_i32_r2
  !! Data type for storing two-dimensional int32 arrays on the host and the device
    integer(int32),pointer :: cpu(:,:)

  contains

    procedure,private :: alloc => alloc_tensor_i32_r2
    procedure,public :: free => free_tensor_i32_r2

    procedure,public :: updatecpu => updatecpu_tensor_i32_r2
    procedure,public :: updategpu => updategpu_tensor_i32_r2

  end type tensor_i32_r2

  type,extends(tensor) :: tensor_i32_r3
  !! Data type for storing three-dimensional int32 arrays on the host and the device
    integer(int32),pointer :: cpu(:,:,:)

  contains

    procedure,private :: alloc => alloc_tensor_i32_r3
    procedure,public :: free => free_tensor_i32_r3

    procedure,public :: updatecpu => updatecpu_tensor_i32_r3
    procedure,public :: updategpu => updategpu_tensor_i32_r3

  end type tensor_i32_r3

  type,extends(tensor) :: tensor_i32_r4
  !! Data type for storing four-dimensional int32 arrays on the host and the device
    integer(int32),pointer :: cpu(:,:,:,:)

  contains

    procedure,private :: alloc => alloc_tensor_i32_r4
    procedure,public :: free => free_tensor_i32_r4

    procedure,public :: updatecpu => updatecpu_tensor_i32_r4
    procedure,public :: updategpu => updategpu_tensor_i32_r4

  end type tensor_i32_r4

contains

  subroutine gpumalloc(gpu_ptr,size,exit_code)
  !! this is a simple wrapper around hipmalloc
  !! if HIP is available at compile time and the HIP CPP flag is defined,
  !! then this code calls hipmalloc to allocate gpu data. Otherwise,
  !! nothing is done and exit_code is set to -1.
    implicit none
    type(c_ptr),intent(inout) :: gpu_ptr
    integer(c_size_t),intent(in) :: size
    integer(kind(hipSuccess)),intent(out) :: exit_code

    !#ifdef HIP
    exit_code = hipMalloc(gpu_ptr,size)
    !#else
    !exit_code = 0
    !return
    !#endif
  end subroutine gpumalloc

  subroutine init(this,dims,device_id)
    implicit none
    class(tensor),intent(out) :: this
    integer,intent(in) :: dims(:)
    integer,intent(in),optional :: device_id
    this % rank = ubound(dims,dim=1)
    this % dims(1:ubound(dims,dim=1)) = dims
    this % inited = .true.
    if (present(device_id)) then
      call this % alloc(device_id)
    else
      call this % alloc()
    end if
    this % allocd = .true.
  end subroutine init

  subroutine info(this)
    implicit none
    class(tensor),intent(in) :: this

    write (output_unit,*) "   Initialized : ",this % inited
    write (output_unit,*) "   Allocated   : ",this % allocd
    if (this % inited) then
      write (output_unit,*) "   rank : ",this % rank
      write (output_unit,*) "   dims : ",this % dims(1:this % rank)
    end if

  end subroutine info

  subroutine alloc_tensor_f32_r1(this,device_id)
    implicit none
    class(tensor_f32_r1),intent(inout) :: this
    integer,optional :: device_id
    ! Local
    integer(kind(hipSuccess)) :: exit_code

    if (this % inited) then
      if (.not. associated(this % cpu)) allocate (this % cpu(1:this % dims(1)))
      this % cpu = 0.0_real32

      if (present(device_id)) then
        this % device_id = device_id
        exit_code = init_device(device_id)
        if (exit_code == 0) then
          call gpumalloc(this % gpu,sizeof(this % cpu),exit_code)
          call hipcheck(exit_code)
        end if
      end if
    else
      write (error_unit,*) "Error : FGPUTensor tensor not init'ed before alloc!"
      stop 1
    end if

  end subroutine alloc_tensor_f32_r1

  subroutine alloc_tensor_f32_r2(this,device_id)
    implicit none
    class(tensor_f32_r2),intent(inout) :: this
    integer,optional :: device_id
    ! Local
    integer(kind(hipSuccess)) :: exit_code

    if (this % inited) then
      if (.not. associated(this % cpu)) allocate (this % cpu(1:this % dims(1), &
                                                             1:this % dims(2)))
      this % cpu = 0.0_real32

      if (present(device_id)) then
        this % device_id = device_id
        exit_code = init_device(device_id)
        if (exit_code == 0) then
          call gpumalloc(this % gpu,sizeof(this % cpu),exit_code)
          call hipcheck(exit_code)
        end if
      end if
    else
      write (error_unit,*) "Error : FGPUTensor tensor not init'ed before alloc!"
      stop 1
    end if

  end subroutine alloc_tensor_f32_r2

  subroutine alloc_tensor_f32_r3(this,device_id)
    implicit none
    class(tensor_f32_r3),intent(inout) :: this
    integer,optional :: device_id
    ! Local
    integer(kind(hipSuccess)) :: exit_code

    if (this % inited) then
      if (.not. associated(this % cpu)) allocate (this % cpu(1:this % dims(1), &
                                                             1:this % dims(2), &
                                                             1:this % dims(3)))
      this % cpu = 0.0_real32

      if (present(device_id)) then
        this % device_id = device_id
        exit_code = init_device(device_id)
        if (exit_code == 0) then
          call gpumalloc(this % gpu,sizeof(this % cpu),exit_code)
          call hipcheck(exit_code)
        end if
      end if
    else
      write (error_unit,*) "Error : FGPUTensor tensor not init'ed before alloc!"
      stop 1
    end if

  end subroutine alloc_tensor_f32_r3

  subroutine alloc_tensor_f32_r4(this,device_id)
    implicit none
    class(tensor_f32_r4),intent(inout) :: this
    integer,optional :: device_id
    ! Local
    integer(kind(hipSuccess)) :: exit_code

    if (this % inited) then
      if (.not. associated(this % cpu)) allocate (this % cpu(1:this % dims(1), &
                                                             1:this % dims(2), &
                                                             1:this % dims(3), &
                                                             1:this % dims(4)))
      this % cpu = 0.0_real32

      if (present(device_id)) then
        this % device_id = device_id
        exit_code = init_device(device_id)
        if (exit_code == 0) then
          call gpumalloc(this % gpu,sizeof(this % cpu),exit_code)
          call hipcheck(exit_code)
        end if
      end if
    else
      write (error_unit,*) "Error : FGPUTensor tensor not init'ed before alloc!"
      stop 1
    end if

  end subroutine alloc_tensor_f32_r4

  subroutine alloc_tensor_f32_r5(this,device_id)
    implicit none
    class(tensor_f32_r5),intent(inout) :: this
    integer,optional :: device_id
    ! Local
    integer(kind(hipSuccess)) :: exit_code

    if (this % inited) then
      if (.not. associated(this % cpu)) allocate (this % cpu(1:this % dims(1), &
                                                             1:this % dims(2), &
                                                             1:this % dims(3), &
                                                             1:this % dims(4), &
                                                             1:this % dims(5)))
      this % cpu = 0.0_real32

      if (present(device_id)) then
        this % device_id = device_id
        exit_code = init_device(device_id)
        if (exit_code == 0) then
          call gpumalloc(this % gpu,sizeof(this % cpu),exit_code)
          call hipcheck(exit_code)
        end if
      end if
    else
      write (error_unit,*) "Error : FGPUTensor tensor not init'ed before alloc!"
      stop 1
    end if

  end subroutine alloc_tensor_f32_r5

  subroutine alloc_tensor_f32_r6(this,device_id)
    implicit none
    class(tensor_f32_r6),intent(inout) :: this
    integer,optional :: device_id
    ! Local
    integer(kind(hipSuccess)) :: exit_code

    if (this % inited) then
      if (.not. associated(this % cpu)) allocate (this % cpu(1:this % dims(1), &
                                                             1:this % dims(2), &
                                                             1:this % dims(3), &
                                                             1:this % dims(4), &
                                                             1:this % dims(5), &
                                                             1:this % dims(6)))
      this % cpu = 0.0_real32

      if (present(device_id)) then
        this % device_id = device_id
        exit_code = init_device(device_id)
        if (exit_code == 0) then
          call gpumalloc(this % gpu,sizeof(this % cpu),exit_code)
          call hipcheck(exit_code)
        end if
      end if
    else
      write (error_unit,*) "Error : FGPUTensor tensor not init'ed before alloc!"
      stop 1
    end if

  end subroutine alloc_tensor_f32_r6

  subroutine alloc_tensor_f32_r7(this,device_id)
    implicit none
    class(tensor_f32_r7),intent(inout) :: this
    integer,optional :: device_id
    ! Local
    integer(kind(hipSuccess)) :: exit_code

    if (this % inited) then
      if (.not. associated(this % cpu)) allocate (this % cpu(1:this % dims(1), &
                                                             1:this % dims(2), &
                                                             1:this % dims(3), &
                                                             1:this % dims(4), &
                                                             1:this % dims(5), &
                                                             1:this % dims(6), &
                                                             1:this % dims(7)))
      this % cpu = 0.0_real32

      if (present(device_id)) then
        this % device_id = device_id
        exit_code = init_device(device_id)
        if (exit_code == 0) then
          call gpumalloc(this % gpu,sizeof(this % cpu),exit_code)
          call hipcheck(exit_code)
        end if
      end if
    else
      write (error_unit,*) "Error : FGPUTensor tensor not init'ed before alloc!"
      stop 1
    end if

  end subroutine alloc_tensor_f32_r7

  subroutine alloc_tensor_i32_r1(this,device_id)
    implicit none
    class(tensor_i32_r1),intent(inout) :: this
    integer,optional :: device_id
    ! Local
    integer(kind(hipSuccess)) :: exit_code

    if (this % inited) then
      if (.not. associated(this % cpu)) allocate (this % cpu(1:this % dims(1)))
      this % cpu = 0

      if (present(device_id)) then
        this % device_id = device_id
        exit_code = init_device(device_id)
        if (exit_code == 0) then
          call gpumalloc(this % gpu,sizeof(this % cpu),exit_code)
          call hipcheck(exit_code)
        end if
      end if
    else
      write (error_unit,*) "Error : FGPUTensor tensor not init'ed before alloc!"
      stop 1
    end if

  end subroutine alloc_tensor_i32_r1

  subroutine alloc_tensor_i32_r2(this,device_id)
    implicit none
    class(tensor_i32_r2),intent(inout) :: this
    integer,optional :: device_id
    ! Local
    integer(kind(hipSuccess)) :: exit_code

    if (this % inited) then
      if (.not. associated(this % cpu)) allocate (this % cpu(1:this % dims(1), &
                                                             1:this % dims(2)))
      this % cpu = 0

      if (present(device_id)) then
        this % device_id = device_id
        exit_code = init_device(device_id)
        if (exit_code == 0) then
          call gpumalloc(this % gpu,sizeof(this % cpu),exit_code)
          call hipcheck(exit_code)
        end if
      end if
    else
      write (error_unit,*) "Error : FGPUTensor tensor not init'ed before alloc!"
      stop 1
    end if

  end subroutine alloc_tensor_i32_r2

  subroutine alloc_tensor_i32_r3(this,device_id)
    implicit none
    class(tensor_i32_r3),intent(inout) :: this
    integer,optional :: device_id
    ! Local
    integer(kind(hipSuccess)) :: exit_code

    if (this % inited) then
      if (.not. associated(this % cpu)) allocate (this % cpu(1:this % dims(1), &
                                                             1:this % dims(2), &
                                                             1:this % dims(3)))
      this % cpu = 0

      if (present(device_id)) then
        this % device_id = device_id
        exit_code = init_device(device_id)
        if (exit_code == 0) then
          call gpumalloc(this % gpu,sizeof(this % cpu),exit_code)
          call hipcheck(exit_code)
        end if
      end if
    else
      write (error_unit,*) "Error : FGPUTensor tensor not init'ed before alloc!"
      stop 1
    end if

  end subroutine alloc_tensor_i32_r3

  subroutine alloc_tensor_i32_r4(this,device_id)
    implicit none
    class(tensor_i32_r4),intent(inout) :: this
    integer,optional :: device_id
    ! Local
    integer(kind(hipSuccess)) :: exit_code

    if (this % inited) then
      if (.not. associated(this % cpu)) allocate (this % cpu(1:this % dims(1), &
                                                             1:this % dims(2), &
                                                             1:this % dims(3), &
                                                             1:this % dims(4)))
      this % cpu = 0

      if (present(device_id)) then
        this % device_id = device_id
        exit_code = init_device(device_id)
        if (exit_code == 0) then
          call gpumalloc(this % gpu,sizeof(this % cpu),exit_code)
          call hipcheck(exit_code)
        end if
      end if
    else
      write (error_unit,*) "Error : FGPUTensor tensor not init'ed before alloc!"
      stop 1
    end if

  end subroutine alloc_tensor_i32_r4

  subroutine free_tensor_f32_r1(this)
    implicit none
    class(tensor_f32_r1),intent(inout) :: this

    if (associated(this % cpu)) deallocate (this % cpu)

    if (this % device_id >= 0) then
      call hipCheck(hipfree(this % gpu))
    end if

  end subroutine free_tensor_f32_r1

  subroutine free_tensor_f32_r2(this)
    implicit none
    class(tensor_f32_r2),intent(inout) :: this

    if (associated(this % cpu)) deallocate (this % cpu)

    if (this % device_id >= 0) then
      call hipCheck(hipfree(this % gpu))
    end if

  end subroutine free_tensor_f32_r2

  subroutine free_tensor_f32_r3(this)
    implicit none
    class(tensor_f32_r3),intent(inout) :: this

    if (associated(this % cpu)) deallocate (this % cpu)

    if (this % device_id >= 0) then
      call hipCheck(hipfree(this % gpu))
    end if

  end subroutine free_tensor_f32_r3

  subroutine free_tensor_f32_r4(this)
    implicit none
    class(tensor_f32_r4),intent(inout) :: this

    if (associated(this % cpu)) deallocate (this % cpu)

    if (this % device_id >= 0) then
      call hipCheck(hipfree(this % gpu))
    end if

  end subroutine free_tensor_f32_r4

  subroutine free_tensor_f32_r5(this)
    implicit none
    class(tensor_f32_r5),intent(inout) :: this

    if (associated(this % cpu)) deallocate (this % cpu)

    if (this % device_id >= 0) then
      call hipCheck(hipfree(this % gpu))
    end if

  end subroutine free_tensor_f32_r5

  subroutine free_tensor_f32_r6(this)
    implicit none
    class(tensor_f32_r6),intent(inout) :: this

    if (associated(this % cpu)) deallocate (this % cpu)

    if (this % device_id >= 0) then
      call hipCheck(hipfree(this % gpu))
    end if

  end subroutine free_tensor_f32_r6

  subroutine free_tensor_f32_r7(this)
    implicit none
    class(tensor_f32_r7),intent(inout) :: this

    if (associated(this % cpu)) deallocate (this % cpu)

    if (this % device_id >= 0) then
      call hipCheck(hipfree(this % gpu))
    end if

  end subroutine free_tensor_f32_r7

  subroutine free_tensor_i32_r1(this)
    implicit none
    class(tensor_i32_r1),intent(inout) :: this

    if (associated(this % cpu)) deallocate (this % cpu)

    if (this % device_id >= 0) then
      call hipCheck(hipfree(this % gpu))
    end if

  end subroutine free_tensor_i32_r1

  subroutine free_tensor_i32_r2(this)
    implicit none
    class(tensor_i32_r2),intent(inout) :: this

    if (associated(this % cpu)) deallocate (this % cpu)

    if (this % device_id >= 0) then
      call hipCheck(hipfree(this % gpu))
    end if

  end subroutine free_tensor_i32_r2

  subroutine free_tensor_i32_r3(this)
    implicit none
    class(tensor_i32_r3),intent(inout) :: this

    if (associated(this % cpu)) deallocate (this % cpu)

    if (this % device_id >= 0) then
      call hipCheck(hipfree(this % gpu))
    end if

  end subroutine free_tensor_i32_r3

  subroutine free_tensor_i32_r4(this)
    implicit none
    class(tensor_i32_r4),intent(inout) :: this

    if (associated(this % cpu)) deallocate (this % cpu)

    if (this % device_id >= 0) then
      call hipCheck(hipfree(this % gpu))
    end if

  end subroutine free_tensor_i32_r4

  subroutine updatecpu_tensor_f32_r1(this)
    implicit none
    class(tensor_f32_r1),intent(inout) :: this

    if (this % device_id >= 0) then
      call hipCheck(hipMemcpy(c_loc(this % cpu), &
                              this % gpu, &
                              sizeof(this % cpu), &
                              hipMemcpyDeviceToHost))
    end if

  end subroutine updatecpu_tensor_f32_r1

  subroutine updatecpu_tensor_f32_r2(this)
    implicit none
    class(tensor_f32_r2),intent(inout) :: this

    if (this % device_id >= 0) then
      call hipCheck(hipMemcpy(c_loc(this % cpu), &
                              this % gpu, &
                              sizeof(this % cpu), &
                              hipMemcpyDeviceToHost))
    end if

  end subroutine updatecpu_tensor_f32_r2

  subroutine updatecpu_tensor_f32_r3(this)
    implicit none
    class(tensor_f32_r3),intent(inout) :: this

    if (this % device_id >= 0) then
      call hipCheck(hipMemcpy(c_loc(this % cpu), &
                              this % gpu, &
                              sizeof(this % cpu), &
                              hipMemcpyDeviceToHost))
    end if

  end subroutine updatecpu_tensor_f32_r3

  subroutine updatecpu_tensor_f32_r4(this)
    implicit none
    class(tensor_f32_r4),intent(inout) :: this

    if (this % device_id >= 0) then
      call hipCheck(hipMemcpy(c_loc(this % cpu), &
                              this % gpu, &
                              sizeof(this % cpu), &
                              hipMemcpyDeviceToHost))
    end if

  end subroutine updatecpu_tensor_f32_r4

  subroutine updatecpu_tensor_f32_r5(this)
    implicit none
    class(tensor_f32_r5),intent(inout) :: this

    if (this % device_id >= 0) then
      call hipCheck(hipMemcpy(c_loc(this % cpu), &
                              this % gpu, &
                              sizeof(this % cpu), &
                              hipMemcpyDeviceToHost))
    end if

  end subroutine updatecpu_tensor_f32_r5

  subroutine updatecpu_tensor_f32_r6(this)
    implicit none
    class(tensor_f32_r6),intent(inout) :: this

    if (this % device_id >= 0) then
      call hipCheck(hipMemcpy(c_loc(this % cpu), &
                              this % gpu, &
                              sizeof(this % cpu), &
                              hipMemcpyDeviceToHost))
    end if

  end subroutine updatecpu_tensor_f32_r6

  subroutine updatecpu_tensor_f32_r7(this)
    implicit none
    class(tensor_f32_r7),intent(inout) :: this

    if (this % device_id >= 0) then
      call hipCheck(hipMemcpy(c_loc(this % cpu), &
                              this % gpu, &
                              sizeof(this % cpu), &
                              hipMemcpyDeviceToHost))
    end if

  end subroutine updatecpu_tensor_f32_r7

  subroutine updatecpu_tensor_i32_r1(this)
    implicit none
    class(tensor_i32_r1),intent(inout) :: this

    if (this % device_id >= 0) then
      call hipCheck(hipMemcpy(c_loc(this % cpu), &
                              this % gpu, &
                              sizeof(this % cpu), &
                              hipMemcpyDeviceToHost))
    end if

  end subroutine updatecpu_tensor_i32_r1

  subroutine updatecpu_tensor_i32_r2(this)
    implicit none
    class(tensor_i32_r2),intent(inout) :: this

    if (this % device_id >= 0) then
      call hipCheck(hipMemcpy(c_loc(this % cpu), &
                              this % gpu, &
                              sizeof(this % cpu), &
                              hipMemcpyDeviceToHost))
    end if

  end subroutine updatecpu_tensor_i32_r2

  subroutine updatecpu_tensor_i32_r3(this)
    implicit none
    class(tensor_i32_r3),intent(inout) :: this

    if (this % device_id >= 0) then
      call hipCheck(hipMemcpy(c_loc(this % cpu), &
                              this % gpu, &
                              sizeof(this % cpu), &
                              hipMemcpyDeviceToHost))
    end if

  end subroutine updatecpu_tensor_i32_r3

  subroutine updatecpu_tensor_i32_r4(this)
    implicit none
    class(tensor_i32_r4),intent(inout) :: this

    if (this % device_id >= 0) then
      call hipCheck(hipMemcpy(c_loc(this % cpu), &
                              this % gpu, &
                              sizeof(this % cpu), &
                              hipMemcpyDeviceToHost))
    end if

  end subroutine updatecpu_tensor_i32_r4

  subroutine updategpu_tensor_f32_r1(this)
    implicit none
    class(tensor_f32_r1),intent(inout) :: this

    if (this % device_id >= 0) then
      call hipCheck(hipMemcpy(this % gpu, &
                              c_loc(this % cpu), &
                              sizeof(this % cpu), &
                              hipMemcpyHostToDevice))
    end if

  end subroutine updategpu_tensor_f32_r1

  subroutine updategpu_tensor_f32_r2(this)
    implicit none
    class(tensor_f32_r2),intent(inout) :: this

    if (this % device_id >= 0) then
      call hipCheck(hipMemcpy(this % gpu, &
                              c_loc(this % cpu), &
                              sizeof(this % cpu), &
                              hipMemcpyHostToDevice))
    end if

  end subroutine updategpu_tensor_f32_r2

  subroutine updategpu_tensor_f32_r3(this)
    implicit none
    class(tensor_f32_r3),intent(inout) :: this

    if (this % device_id >= 0) then
      call hipCheck(hipMemcpy(this % gpu, &
                              c_loc(this % cpu), &
                              sizeof(this % cpu), &
                              hipMemcpyHostToDevice))
    end if

  end subroutine updategpu_tensor_f32_r3

  subroutine updategpu_tensor_f32_r4(this)
    implicit none
    class(tensor_f32_r4),intent(inout) :: this

    if (this % device_id >= 0) then
      call hipCheck(hipMemcpy(this % gpu, &
                              c_loc(this % cpu), &
                              sizeof(this % cpu), &
                              hipMemcpyHostToDevice))
    end if

  end subroutine updategpu_tensor_f32_r4

  subroutine updategpu_tensor_f32_r5(this)
    implicit none
    class(tensor_f32_r5),intent(inout) :: this

    if (this % device_id >= 0) then
      call hipCheck(hipMemcpy(this % gpu, &
                              c_loc(this % cpu), &
                              sizeof(this % cpu), &
                              hipMemcpyHostToDevice))
    end if

  end subroutine updategpu_tensor_f32_r5

  subroutine updategpu_tensor_f32_r6(this)
    implicit none
    class(tensor_f32_r6),intent(inout) :: this

    if (this % device_id >= 0) then
      call hipCheck(hipMemcpy(this % gpu, &
                              c_loc(this % cpu), &
                              sizeof(this % cpu), &
                              hipMemcpyHostToDevice))
    end if

  end subroutine updategpu_tensor_f32_r6

  subroutine updategpu_tensor_f32_r7(this)
    implicit none
    class(tensor_f32_r7),intent(inout) :: this

    if (this % device_id >= 0) then
      call hipCheck(hipMemcpy(this % gpu, &
                              c_loc(this % cpu), &
                              sizeof(this % cpu), &
                              hipMemcpyHostToDevice))
    end if

  end subroutine updategpu_tensor_f32_r7

  subroutine updategpu_tensor_i32_r1(this)
    implicit none
    class(tensor_i32_r1),intent(inout) :: this

    if (this % device_id >= 0) then
      call hipCheck(hipMemcpy(this % gpu, &
                              c_loc(this % cpu), &
                              sizeof(this % cpu), &
                              hipMemcpyHostToDevice))
    end if

  end subroutine updategpu_tensor_i32_r1

  subroutine updategpu_tensor_i32_r2(this)
    implicit none
    class(tensor_i32_r2),intent(inout) :: this

    if (this % device_id >= 0) then
      call hipCheck(hipMemcpy(this % gpu, &
                              c_loc(this % cpu), &
                              sizeof(this % cpu), &
                              hipMemcpyHostToDevice))
    end if

  end subroutine updategpu_tensor_i32_r2

  subroutine updategpu_tensor_i32_r3(this)
    implicit none
    class(tensor_i32_r3),intent(inout) :: this

    if (this % device_id >= 0) then
      call hipCheck(hipMemcpy(this % gpu, &
                              c_loc(this % cpu), &
                              sizeof(this % cpu), &
                              hipMemcpyHostToDevice))
    end if

  end subroutine updategpu_tensor_i32_r3

  subroutine updategpu_tensor_i32_r4(this)
    implicit none
    class(tensor_i32_r4),intent(inout) :: this

    if (this % device_id >= 0) then
      call hipCheck(hipMemcpy(this % gpu, &
                              c_loc(this % cpu), &
                              sizeof(this % cpu), &
                              hipMemcpyHostToDevice))
    end if

  end subroutine updategpu_tensor_i32_r4

end module FGPUTensor
