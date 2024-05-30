module FGPUTensor_Utils

  use FGPUTensor_HIP
  use FGPUTensor_HIP_enums
  use iso_c_binding
  use iso_fortran_env

  implicit none

  enum,bind(c)
    enumerator :: FGPUTensor_Utils_exit_no_error = 0
    enumerator :: FGPUTensor_Utils_exit_invalid_device_id = 1
    enumerator :: FGPUTensor_Utils_exit_hip_error_block = 10000 ! HIP Error codes vary from 0 to 1053.
  end enum

  logical,private :: acquired = .false.

contains

  function init_device(dev_id) result(exit_code)
    !! Initialise HIP and set device device to use
    !! Returns integer exit code indicating status
    !!
    !!   0 - No errors encountered
    !!   1 -

    ! The id of the device to use
    integer,intent(in) :: dev_id
    integer(kind(hipSuccess)) :: exit_code

    ! Number of compute devices
    integer :: ndevices
    integer(kind(hipSuccess)) :: hipError_t

    ! ! Initialise resources the best practice way
    ! if (.not. acquired) then
    !     ! Initialise HIP
    !     hipError_t = hipinit(0)
    !     if( hipError_t /= 0 )then
    !       exit_code = FGPUTensor_Utils_exit_hip_error_block + hipError_t
    !       return
    !     end if
    !     ! We have now acquired HIP
    !     acquired = .true.
    ! end if

    ! Get the number of compute devices
    hipError_t = hipgetdevicecount(ndevices)
    if (hipError_t /= 0) then
      exit_code = FGPUTensor_Utils_exit_hip_error_block + hipError_t
      return
    end if

    if ((dev_id .ge. 0) .and. (dev_id .lt. ndevices)) then
      hipError_t = hipsetdevice(dev_id)
      if (hipError_t /= 0) then
        exit_code = FGPUTensor_Utils_exit_hip_error_block + hipError_t
        return
      end if
    else
      write (error_unit,*) 'Error :  dev_id was not inside the range of available devices.'
      exit_code = FGPUTensor_Utils_exit_invalid_device_id
      return
    end if

    exit_code = FGPUTensor_Utils_exit_no_error

  end function init_device

  function GPUAvailable() result(avail)
    implicit none
    logical :: avail
! Local
    integer(c_int) :: gpuCount
    integer(kind(hipSuccess)) :: err

    err = hipGetDeviceCount(gpuCount)
    if (gpuCount > 0 .and. err == hipSuccess) then
      avail = .true.
    else
      avail = .false.
    end if

  end function GPUAvailable

end module FGPUTensor_Utils
