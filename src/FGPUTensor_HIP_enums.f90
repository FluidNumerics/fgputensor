module FGPUTensor_HIP_enums

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

  !> Derived TYPE that can be mapped directly to a CUDA/HIP C++ dim3.
  type,bind(c) :: dim3
    integer(c_int) :: x = 1,y = 1,z = 1
  end type dim3

  type,bind(c) :: hipDeviceProp_t ! as of ROCm 4.4
    character(kind=c_char) :: name(256)            !< Device name.
    integer(c_size_t) :: totalGlobalMem     !< Size of global memory region (in bytes).
    integer(c_size_t) :: sharedMemPerBlock  !< Size of shared memory region (in bytes).
    integer(c_int) :: regsPerBlock          !< Registers per block.
    integer(c_int) :: warpSize              !< Warp size.
    integer(c_int) :: maxThreadsPerBlock    !< Max work items per work group or workgroup max size.
    integer(c_int) :: maxThreadsDim(3)      !< Max number of threads in each dimension (XYZ) of a block.
    integer(c_int) :: maxGridSize(3)        !< Max grid dimensions (XYZ).
    integer(c_int) :: clockRate             !< Max clock frequency of the multiProcessors in khz.
    integer(c_int) :: memoryClockRate       !< Max global memory clock frequency in khz.
    integer(c_int) :: memoryBusWidth        !< Global memory bus width in bits.
    integer(c_size_t) :: totalConstMem      !< Size of shared memory region (in bytes).
    integer(c_int) :: major  !< Major compute capability.  On HCC, this is an approximation and features may
    !< differ from CUDA CC.  See the arch feature flags for portable ways to query
    !< feature caps.
    integer(c_int) :: minor  !< Minor compute capability.  On HCC, this is an approximation and features may
    !< differ from CUDA CC.  See the arch feature flags for portable ways to query
    !< feature caps.
    integer(c_int) :: multiProcessorCount          !< Number of multi-processors (compute units).
    integer(c_int) :: l2CacheSize                  !< L2 cache size.
    integer(c_int) :: maxThreadsPerMultiProcessor  !< Maximum resident threads per multi-processor.
    integer(c_int) :: computeMode                  !< Compute mode.
    integer(c_int) :: clockInstructionRate  !< Frequency in khz of the timer used by the device-side "clock*"
    !< instructions.  New for HIP.
    integer(c_int) arch       !< Architectural feature flags.  New for HIP.
    integer(c_int) :: concurrentKernels     !< Device can possibly execute multiple kernels concurrently.
    integer(c_int) :: pciDomainID           !< PCI Domain ID
    integer(c_int) :: pciBusID              !< PCI Bus ID.
    integer(c_int) :: pciDeviceID           !< PCI Device ID.
    integer(c_size_t) :: maxSharedMemoryPerMultiProcessor  !< Maximum Shared Memory Per Multiprocessor.
    integer(c_int) :: isMultiGpuBoard                      !< 1 if device is on a multi-GPU board, 0 if not.
    integer(c_int) :: canMapHostMemory                     !< Check whether HIP can map host memory
    integer(c_int) :: gcnArch                              !< DEPRECATED: use gcnArchName instead
    character(kind=c_char) :: gcnArchName(256)                    !< AMD GCN Arch Name.
    integer(c_int) :: integrated            !< APU vs dGPU
    integer(c_int) :: cooperativeLaunch            !< HIP device supports cooperative launch
    integer(c_int) :: cooperativeMultiDeviceLaunch !< HIP device supports cooperative launch on multiple devices
    integer(c_int) :: maxTexture1DLinear    !< Maximum size for 1D textures bound to linear memory
    integer(c_int) :: maxTexture1D          !< Maximum number of elements in 1D images
    integer(c_int) :: maxTexture2D(2)       !< Maximum dimensions (width, height) of 2D images, in image elements
    integer(c_int) :: maxTexture3D(3)       !< Maximum dimensions (width, height, depth) of 3D images, in image elements
    type(c_ptr) :: hdpMemFlushCntl      !< Addres of HDP_MEM_COHERENCY_FLUSH_CNTL register
    type(c_ptr) :: hdpRegFlushCntl      !< Addres of HDP_REG_COHERENCY_FLUSH_CNTL register
    integer(c_size_t) :: memPitch                 !<Maximum pitch in bytes allowed by memory copies
    integer(c_size_t) :: textureAlignment         !<Alignment requirement for textures
    integer(c_size_t) :: texturePitchAlignment    !<Pitch alignment requirement for texture references bound to pitched memory
    integer(c_int) :: kernelExecTimeoutEnabled    !<Run time limit for kernels executed on the device
    integer(c_int) :: ECCEnabled                  !<Device has ECC support enabled
    integer(c_int) :: tccDriver                   !< 1:If device is Tesla device using TCC driver, else 0
    integer(c_int) :: cooperativeMultiDeviceUnmatchedFunc        !< HIP device supports cooperative launch on multiple
    !devices with unmatched FUNCTIONs
    integer(c_int) :: cooperativeMultiDeviceUnmatchedGridDim     !< HIP device supports cooperative launch on multiple
    !devices with unmatched grid dimensions
    integer(c_int) :: cooperativeMultiDeviceUnmatchedBlockDim    !< HIP device supports cooperative launch on multiple
    !devices with unmatched block dimensions
    integer(c_int) :: cooperativeMultiDeviceUnmatchedSharedMem   !< HIP device supports cooperative launch on multiple
    !devices with unmatched shared memories
    integer(c_int) :: isLargeBar                  !< 1: if it is a large PCI bar device, else 0
    integer(c_int) :: asicRevision                !< Revision of the GPU in this device
    integer(c_int) :: managedMemory               !< Device supports allocating managed memory on this system
    integer(c_int) :: directManagedMemAccessFromHost !< Host can directly access managed memory on the device without migration
    integer(c_int) :: concurrentManagedAccess     !< Device can coherently access managed memory concurrently with the CPU
    integer(c_int) :: pageableMemoryAccess        !< Device supports coherently accessing pageable memory
    !< without calling hipHostRegister on it
    integer(c_int) :: pageableMemoryAccessUsesHostPageTables !< Device accesses pageable memory via the host's page tables
    character(kind=c_char) :: GPUFORT_PADDING(256) !< GPUFORT :Some extra bytes to prevent seg faults in newer versions
  end type hipDeviceProp_t

  ! runtime api parameters

  integer,parameter :: hipIpcMemLazyEnablePeerAccess = 0

  integer,parameter :: hipStreamDefault = &
                       0  !< Default stream creation flags. These are used with hipStreamCreate = ().
  integer,parameter :: hipStreamNonBlocking = 1  !< Stream does not implicitly synchronize with null stream

  integer,parameter :: hipEventDefault = 0  !< Default flags
  integer,parameter :: hipEventBlockingSync = &
                       1  !< Waiting will yield CPU.  Power-friENDly and usage-friENDly but may increase latency.
  integer,parameter :: hipEventDisableTiming = &
                       2  !< Disable event's capability to record timing information.  May improve performance.
  integer,parameter :: hipEventInterprocess = 4  !< Event can support IPC.  @warning - not supported in HIP.
  integer,parameter :: hipEventReleaseToDevice = &
                       1073741824 !< 0x40000000 - Use a device-scope release when recording this event.  This flag is useful to
  !INTEGER, parameter :: hipEventReleaseToSystem = &
  !    2147483648 !< 0x80000000 - Use a system-scope release that when recording this event.  This flag is
  integer,parameter :: hipHostMallocDefault = 0
  integer,parameter :: hipHostMallocPortable = 1  !< Memory is considered allocated by all contexts.
  integer,parameter :: hipHostMallocMapped = &
                       2  !< Map the allocation into the address space for the current device.  The device pointer
  integer,parameter :: hipHostMallocWriteCombined = 4
  integer,parameter :: hipHostMallocNumaUser = &
                       536870912 !< 0x20000000 - Host memory allocation will follow numa policy set by user
  integer,parameter :: hipHostMallocCoherent = &
                       1073741824 !< 0x40000000 - Allocate coherent memory. Overrides HIP_COHERENT_HOST_ALLOC for specific
  !INTEGER, parameter :: hipHostMallocNonCoherent = &
  !    2147483648 !< 0x80000000 - Allocate non-coherent memory. Overrides HIP_COHERENT_HOST_ALLOC for specific
  integer,parameter :: hipMemAttachGlobal = 1    !< Memory can be accessed by any stream on any device
  integer,parameter :: hipMemAttachHost = 2    !< Memory cannot be accessed by any stream on any device
  integer,parameter :: hipMemAttachSingle = 4    !< Memory can only be accessed by a single stream on
  !< the associated device
  integer,parameter :: hipDeviceMallocDefault = 0
  integer,parameter :: hipDeviceMallocFinegrained = 1  !< Memory is allocated in fine grained region of device.

  integer,parameter :: hipHostRegisterDefault = 0   !< Memory is Mapped and Portable
  integer,parameter :: hipHostRegisterPortable = 1  !< Memory is considered registered by all contexts.
  integer,parameter :: hipHostRegisterMapped = &
                       2  !< Map the allocation into the address space for the current device.  The device pointer
  integer,parameter :: hipHostRegisterIoMemory = 4  !< Not supported.
  integer,parameter :: hipExtHostRegisterCoarseGrained = 8  !< Coarse Grained host memory lock

  integer,parameter :: hipDeviceScheduleAuto = 0  !< Automatically select between Spin and Yield
  integer,parameter :: hipDeviceScheduleSpin = &
                       1  !< Dedicate a CPU core to spin-wait.  Provides lowest latency, but burns a CPU core and
  integer,parameter :: hipDeviceScheduleYield = &
                       2  !< Yield the CPU to the operating system when waiting.  May increase latency, but lowers
  integer,parameter :: hipDeviceScheduleBlockingSync = 4
  integer,parameter :: hipDeviceScheduleMask = 7

  integer,parameter :: hipDeviceMapHost = 8
  integer,parameter :: hipDeviceLmemResizeToMax = 22 ! 16

  integer,parameter :: hipArrayDefault = 0  !< Default HIP array allocation flag
  integer,parameter :: hipArrayLayered = 1
  integer,parameter :: hipArraySurfaceLoadStore = 2
  integer,parameter :: hipArrayCubemap = 4
  integer,parameter :: hipArrayTextureGather = 8

  integer,parameter :: hipOccupancyDefault = 0

  integer,parameter :: hipCooperativeLaunchMultiDeviceNoPreSync = 1
  integer,parameter :: hipCooperativeLaunchMultiDeviceNoPostSync = 2

  enum,bind(c)
    enumerator :: HIP_SUCCESS = 0
    enumerator :: HIP_ERROR_INVALID_VALUE
    enumerator :: HIP_ERROR_NOT_INITIALIZED
    enumerator :: HIP_ERROR_LAUNCH_OUT_OF_RESOURCES
  end enum

  enum,bind(c)
    enumerator :: hipMemoryTYPEHost
    enumerator :: hipMemoryTYPEDevice
    enumerator :: hipMemoryTYPEArray
    enumerator :: hipMemoryTYPEUnified
  end enum

  enum,bind(c)
    enumerator :: hipSuccess = 0
    enumerator :: hipErrorInvalidValue = 1
    enumerator :: hipErrorOutOfMemory = 2
    enumerator :: hipErrorMemoryAllocation = 2
    enumerator :: hipErrorNotInitialized = 3
    enumerator :: hipErrorInitializationError = 3
    enumerator :: hipErrorDeinitialized = 4
    enumerator :: hipErrorProfilerDisabled = 5
    enumerator :: hipErrorProfilerNotInitialized = 6
    enumerator :: hipErrorProfilerAlreadyStarted = 7
    enumerator :: hipErrorProfilerAlreadyStopped = 8
    enumerator :: hipErrorInvalidConfiguration = 9
    enumerator :: hipErrorInvalidPitchValue = 12
    enumerator :: hipErrorInvalidSymbol = 13
    enumerator :: hipErrorInvalidDevicePointer = 17
    enumerator :: hipErrorInvalidMemcpyDirection = 21
    enumerator :: hipErrorInsufficientDriver = 35
    enumerator :: hipErrorMissingConfiguration = 52
    enumerator :: hipErrorPriorLaunchFailure = 53
    enumerator :: hipErrorInvalidDeviceFUNCTION = 98
    enumerator :: hipErrorNoDevice = 100
    enumerator :: hipErrorInvalidDevice = 101
    enumerator :: hipErrorInvalidImage = 200
    enumerator :: hipErrorInvalidContext = 201
    enumerator :: hipErrorContextAlreadyCurrent = 202
    enumerator :: hipErrorMapFailed = 205
    enumerator :: hipErrorMapBufferObjectFailed = 205
    enumerator :: hipErrorUnmapFailed = 206
    enumerator :: hipErrorArrayIsMapped = 207
    enumerator :: hipErrorAlreadyMapped = 208
    enumerator :: hipErrorNoBinaryForGpu = 209
    enumerator :: hipErrorAlreadyAcquired = 210
    enumerator :: hipErrorNotMapped = 211
    enumerator :: hipErrorNotMappedAsArray = 212
    enumerator :: hipErrorNotMappedAsPointer = 213
    enumerator :: hipErrorECCNotCorrectable = 214
    enumerator :: hipErrorUnsupportedLimit = 215
    enumerator :: hipErrorContextAlreadyInUse = 216
    enumerator :: hipErrorPeerAccessUnsupported = 217
    enumerator :: hipErrorInvalidKernelFile = 218
    enumerator :: hipErrorInvalidGraphicsContext = 219
    enumerator :: hipErrorInvalidSource = 300
    enumerator :: hipErrorFileNotFound = 301
    enumerator :: hipErrorSharedObjectSymbolNotFound = 302
    enumerator :: hipErrorSharedObjectInitFailed = 303
    enumerator :: hipErrorOperatingSystem = 304
    enumerator :: hipErrorInvalidHandle = 400
    enumerator :: hipErrorInvalidResourceHandle = 400
    enumerator :: hipErrorIllegalState = 401
    enumerator :: hipErrorNotFound = 500
    enumerator :: hipErrorNotReady = 600
    enumerator :: hipErrorIllegalAddress = 700
    enumerator :: hipErrorLaunchOutOfResources = 701
    enumerator :: hipErrorLaunchTimeOut = 702
    enumerator :: hipErrorPeerAccessAlreadyEnabled = 704
    enumerator :: hipErrorPeerAccessNotEnabled = 705
    enumerator :: hipErrorSetOnActiveProcess = 708
    enumerator :: hipErrorContextIsDestroyed = 709
    enumerator :: hipErrorAssert = 710
    enumerator :: hipErrorHostMemoryAlreadyRegistered = 712
    enumerator :: hipErrorHostMemoryNotRegistered = 713
    enumerator :: hipErrorLaunchFailure = 719
    enumerator :: hipErrorCooperativeLaunchTooLarge = 720
    enumerator :: hipErrorNotSupported = 801
    enumerator :: hipErrorStreamCaptureUnsupported = 900
    enumerator :: hipErrorStreamCaptureInvalidated = 901
    enumerator :: hipErrorStreamCaptureMerge = 902
    enumerator :: hipErrorStreamCaptureUnmatched = 903
    enumerator :: hipErrorStreamCaptureUnjoined = 904
    enumerator :: hipErrorStreamCaptureIsolation = 905
    enumerator :: hipErrorStreamCaptureImplicit = 906
    enumerator :: hipErrorCapturedEvent = 907
    enumerator :: hipErrorStreamCaptureWrongThread = 908
    enumerator :: hipErrorGraphExecUpdateFailure = 910
    enumerator :: hipErrorUnknown = 999
    enumerator :: hipErrorRuntimeMemory = 1052
    enumerator :: hipErrorRuntimeOther = 1053
    enumerator :: hipErrorTbd
  end enum

  enum,bind(c)
    enumerator :: hipDeviceAttributeCudaCompatibleBegin = 0
    enumerator :: hipDeviceAttributeEccEnabled = hipDeviceAttributeCudaCompatibleBegin
    enumerator :: hipDeviceAttributeAccessPolicyMaxWindowSize
    enumerator :: hipDeviceAttributeAsyncEngineCount
    enumerator :: hipDeviceAttributeCanMapHostMemory
    enumerator :: hipDeviceAttributeCanUseHostPointerForRegisteredMem
    enumerator :: hipDeviceAttributeClockRate
    enumerator :: hipDeviceAttributeComputeMode
    enumerator :: hipDeviceAttributeComputePreemptionSupported
    enumerator :: hipDeviceAttributeConcurrentKernels
    enumerator :: hipDeviceAttributeConcurrentManagedAccess
    enumerator :: hipDeviceAttributeCooperativeLaunch
    enumerator :: hipDeviceAttributeCooperativeMultiDeviceLaunch
    enumerator :: hipDeviceAttributeDeviceOverlap
    enumerator :: hipDeviceAttributeDirectManagedMemAccessFromHost
    enumerator :: hipDeviceAttributeGlobalL1CacheSupported
    enumerator :: hipDeviceAttributeHostNativeAtomicSupported
    enumerator :: hipDeviceAttributeIntegrated
    enumerator :: hipDeviceAttributeIsMultiGpuBoard
    enumerator :: hipDeviceAttributeKernelExecTimeout
    enumerator :: hipDeviceAttributeL2CacheSize
    enumerator :: hipDeviceAttributeLocalL1CacheSupported
    enumerator :: hipDeviceAttributeLuid
    enumerator :: hipDeviceAttributeLuidDeviceNodeMask
    enumerator :: hipDeviceAttributeComputeCapabilityMajor
    enumerator :: hipDeviceAttributeManagedMemory
    enumerator :: hipDeviceAttributeMaxBlocksPerMultiProcessor
    enumerator :: hipDeviceAttributeMaxBlockDimX
    enumerator :: hipDeviceAttributeMaxBlockDimY
    enumerator :: hipDeviceAttributeMaxBlockDimZ
    enumerator :: hipDeviceAttributeMaxGridDimX
    enumerator :: hipDeviceAttributeMaxGridDimY
    enumerator :: hipDeviceAttributeMaxGridDimZ
    enumerator :: hipDeviceAttributeMaxSurface1D
    enumerator :: hipDeviceAttributeMaxSurface1DLayered
    enumerator :: hipDeviceAttributeMaxSurface2D
    enumerator :: hipDeviceAttributeMaxSurface2DLayered
    enumerator :: hipDeviceAttributeMaxSurface3D
    enumerator :: hipDeviceAttributeMaxSurfaceCubemap
    enumerator :: hipDeviceAttributeMaxSurfaceCubemapLayered
    enumerator :: hipDeviceAttributeMaxTexture1DWidth
    enumerator :: hipDeviceAttributeMaxTexture1DLayered
    enumerator :: hipDeviceAttributeMaxTexture1DLinear
    enumerator :: hipDeviceAttributeMaxTexture1DMipmap
    enumerator :: hipDeviceAttributeMaxTexture2DWidth
    enumerator :: hipDeviceAttributeMaxTexture2DHeight
    enumerator :: hipDeviceAttributeMaxTexture2DGather
    enumerator :: hipDeviceAttributeMaxTexture2DLayered
    enumerator :: hipDeviceAttributeMaxTexture2DLinear
    enumerator :: hipDeviceAttributeMaxTexture2DMipmap
    enumerator :: hipDeviceAttributeMaxTexture3DWidth
    enumerator :: hipDeviceAttributeMaxTexture3DHeight
    enumerator :: hipDeviceAttributeMaxTexture3DDepth
    enumerator :: hipDeviceAttributeMaxTexture3DAlt
    enumerator :: hipDeviceAttributeMaxTextureCubemap
    enumerator :: hipDeviceAttributeMaxTextureCubemapLayered
    enumerator :: hipDeviceAttributeMaxThreadsDim
    enumerator :: hipDeviceAttributeMaxThreadsPerBlock
    enumerator :: hipDeviceAttributeMaxThreadsPerMultiProcessor
    enumerator :: hipDeviceAttributeMaxPitch
    enumerator :: hipDeviceAttributeMemoryBusWidth
    enumerator :: hipDeviceAttributeMemoryClockRate
    enumerator :: hipDeviceAttributeComputeCapabilityMinor
    enumerator :: hipDeviceAttributeMultiGpuBoardGroupID
    enumerator :: hipDeviceAttributeMultiprocessorCount
    enumerator :: hipDeviceAttributeName
    enumerator :: hipDeviceAttributePageableMemoryAccess
    enumerator :: hipDeviceAttributePageableMemoryAccessUsesHostPageTables
    enumerator :: hipDeviceAttributePciBusId
    enumerator :: hipDeviceAttributePciDeviceId
    enumerator :: hipDeviceAttributePciDomainID
    enumerator :: hipDeviceAttributePersistingL2CacheMaxSize
    enumerator :: hipDeviceAttributeMaxRegistersPerBlock
    enumerator :: hipDeviceAttributeMaxRegistersPerMultiprocessor
    enumerator :: hipDeviceAttributeReservedSharedMemPerBlock
    enumerator :: hipDeviceAttributeMaxSharedMemoryPerBlock
    enumerator :: hipDeviceAttributeSharedMemPerBlockOptin
    enumerator :: hipDeviceAttributeSharedMemPerMultiprocessor
    enumerator :: hipDeviceAttributeSingleToDoublePrecisionPerfRatio
    enumerator :: hipDeviceAttributeStreamPrioritiesSupported
    enumerator :: hipDeviceAttributeSurfaceAlignment
    enumerator :: hipDeviceAttributeTccDriver
    enumerator :: hipDeviceAttributeTextureAlignment
    enumerator :: hipDeviceAttributeTexturePitchAlignment
    enumerator :: hipDeviceAttributeTotalConstantMemory
    enumerator :: hipDeviceAttributeTotalGlobalMem
    enumerator :: hipDeviceAttributeUnifiedAddressing
    enumerator :: hipDeviceAttributeUuid
    enumerator :: hipDeviceAttributeWarpSize
    enumerator :: hipDeviceAttributeCudaCompatibleEND = 9999
    enumerator :: hipDeviceAttributeAmdSpecificBegin = 10000
    enumerator :: hipDeviceAttributeClockInstructionRate = hipDeviceAttributeAmdSpecificBegin
    enumerator :: hipDeviceAttributeArch
    enumerator :: hipDeviceAttributeMaxSharedMemoryPerMultiprocessor
    enumerator :: hipDeviceAttributeGcnArch
    enumerator :: hipDeviceAttributeGcnArchName
    enumerator :: hipDeviceAttributeHdpMemFlushCntl
    enumerator :: hipDeviceAttributeHdpRegFlushCntl
    enumerator :: hipDeviceAttributeCooperativeMultiDeviceUnmatchedFunc
    enumerator :: hipDeviceAttributeCooperativeMultiDeviceUnmatchedGridDim
    enumerator :: hipDeviceAttributeCooperativeMultiDeviceUnmatchedBlockDim
    enumerator :: hipDeviceAttributeCooperativeMultiDeviceUnmatchedSharedMem
    enumerator :: hipDeviceAttributeIsLargeBar
    enumerator :: hipDeviceAttributeAsicRevision
    enumerator :: hipDeviceAttributeCanUseStreamWaitValue
    enumerator :: hipDeviceAttributeImageSupport
    enumerator :: hipDeviceAttributeAmdSpecificEND = 19999
    enumerator :: hipDeviceAttributeVENDorSpecificBegin = 20000
  end enum

  enum,bind(c)
    enumerator :: hipComputeModeDefault = 0
    enumerator :: hipComputeModeExclusive = 1
    enumerator :: hipComputeModeProhibited = 2
    enumerator :: hipComputeModeExclusiveProcess = 3
  end enum

  enum,bind(c)
    enumerator :: hipDevP2PAttrPerformanceRank = 0
    enumerator :: hipDevP2PAttrAccessSupported
    enumerator :: hipDevP2PAttrNativeAtomicSupported
    enumerator :: hipDevP2PAttrHipArrayAccessSupported
  end enum

  enum,bind(c)
    enumerator :: hipLimitPrintfFifoSize = 1
    enumerator :: hipLimitMallocHeapSize = 2
  end enum

  enum,bind(c)
    enumerator :: hipMemAdviseSetReadMostly = 1
    enumerator :: hipMemAdviseUnsetReadMostly = 2
    enumerator :: hipMemAdviseSetPreferredLocation = 3
    enumerator :: hipMemAdviseUnsetPreferredLocation = 4
    enumerator :: hipMemAdviseSetAccessedBy = 5
    enumerator :: hipMemAdviseUnsetAccessedBy = 6
    enumerator :: hipMemAdviseSetCoarseGrain = 100
    enumerator :: hipMemAdviseUnsetCoarseGrain = 101
  end enum

  enum,bind(c)
    enumerator :: hipMemRangeCoherencyModeFineGrain = 0
    enumerator :: hipMemRangeCoherencyModeCoarseGrain = 1
    enumerator :: hipMemRangeCoherencyModeIndeterminate = 2
  end enum

  enum,bind(c)
    enumerator :: hipMemRangeAttributeReadMostly = 1
    enumerator :: hipMemRangeAttributePreferredLocation = 2
    enumerator :: hipMemRangeAttributeAccessedBy = 3
    enumerator :: hipMemRangeAttributeLastPrefetchLocation = 4
    enumerator :: hipMemRangeAttributeCoherencyMode = 100
  end enum

  enum,bind(c)
    enumerator :: hipJitOptionMaxRegisters = 0
    enumerator :: hipJitOptionThreadsPerBlock
    enumerator :: hipJitOptionWallTime
    enumerator :: hipJitOptionInfoLogBuffer
    enumerator :: hipJitOptionInfoLogBufferSizeBytes
    enumerator :: hipJitOptionErrorLogBuffer
    enumerator :: hipJitOptionErrorLogBufferSizeBytes
    enumerator :: hipJitOptionOptimizationLevel
    enumerator :: hipJitOptionTargetFromContext
    enumerator :: hipJitOptionTarget
    enumerator :: hipJitOptionFallbackStrategy
    enumerator :: hipJitOptionGenerateDebugInfo
    enumerator :: hipJitOptionLogVerbose
    enumerator :: hipJitOptionGenerateLineInfo
    enumerator :: hipJitOptionCacheMode
    enumerator :: hipJitOptionSm3xOpt
    enumerator :: hipJitOptionFastCompile
    enumerator :: hipJitOptionNumOptions
  end enum

  enum,bind(c)
    enumerator :: hipFuncAttributeMaxDynamicSharedMemorySize = 8
    enumerator :: hipFuncAttributePreferredSharedMemoryCarveout = 9
    enumerator :: hipFuncAttributeMax
  end enum

  enum,bind(c)
    enumerator :: hipFuncCachePreferNone
    enumerator :: hipFuncCachePreferShared
    enumerator :: hipFuncCachePreferL1
    enumerator :: hipFuncCachePreferEqual
  end enum

  enum,bind(c)
    enumerator :: hipSharedMemBankSizeDefault
    enumerator :: hipSharedMemBankSizeFourByte
    enumerator :: hipSharedMemBankSizeEightByte
  end enum

  enum,bind(c)
    enumerator :: hipExternalMemoryHandleTYPEOpaqueFd = 1
    enumerator :: hipExternalMemoryHandleTYPEOpaqueWin32 = 2
    enumerator :: hipExternalMemoryHandleTYPEOpaqueWin32Kmt = 3
    enumerator :: hipExternalMemoryHandleTYPED3D12Heap = 4
    enumerator :: hipExternalMemoryHandleTYPED3D12Resource = 5
    enumerator :: hipExternalMemoryHandleTYPED3D11Resource = 6
    enumerator :: hipExternalMemoryHandleTYPED3D11ResourceKmt = 7
  end enum

  enum,bind(c)
    enumerator :: hipExternalSemaphoreHandleTYPEOpaqueFd = 1
    enumerator :: hipExternalSemaphoreHandleTYPEOpaqueWin32 = 2
    enumerator :: hipExternalSemaphoreHandleTYPEOpaqueWin32Kmt = 3
    enumerator :: hipExternalSemaphoreHandleTYPED3D12Fence = 4
  end enum

  enum,bind(c)
    enumerator :: hipGLDeviceListAll = 1
    enumerator :: hipGLDeviceListCurrentFrame = 2
    enumerator :: hipGLDeviceListNextFrame = 3
  end enum

  enum,bind(c)
    enumerator :: hipGraphicsRegisterFlagsNone = 0
    enumerator :: hipGraphicsRegisterFlagsReadOnly = 1
    enumerator :: hipGraphicsRegisterFlagsWriteDiscard = 2
    enumerator :: hipGraphicsRegisterFlagsSurfaceLoadStore = 4
    enumerator :: hipGraphicsRegisterFlagsTextureGather = 8
  end enum

  enum,bind(c)
    enumerator :: hipGraphNodeTYPEKernel = 1
    enumerator :: hipGraphNodeTYPEMemcpy = 2
    enumerator :: hipGraphNodeTYPEMemset = 3
    enumerator :: hipGraphNodeTYPEHost = 4
    enumerator :: hipGraphNodeTYPEGraph = 5
    enumerator :: hipGraphNodeTYPEEmpty = 6
    enumerator :: hipGraphNodeTYPEWaitEvent = 7
    enumerator :: hipGraphNodeTYPEEventRecord = 8
    enumerator :: hipGraphNodeTYPEMemcpy1D = 9
    enumerator :: hipGraphNodeTYPEMemcpyFromSymbol = 10
    enumerator :: hipGraphNodeTYPEMemcpyToSymbol = 11
    enumerator :: hipGraphNodeTYPECount
  end enum

  enum,bind(c)
    enumerator :: hipGraphExecUpdateSuccess = 0
    enumerator :: hipGraphExecUpdateError = 1
    enumerator :: hipGraphExecUpdateErrorTopologyChanged = 2
    enumerator :: hipGraphExecUpdateErrorNodeTYPEChanged = 3
    enumerator :: hipGraphExecUpdateErrorFUNCTIONChanged = 4
    enumerator :: hipGraphExecUpdateErrorParametersChanged = 5
    enumerator :: hipGraphExecUpdateErrorNotSupported = 6
    enumerator :: hipGraphExecUpdateErrorUnsupportedFUNCTIONChange = 7
  end enum

  enum,bind(c)
    enumerator :: hipStreamCaptureModeGlobal = 0
    enumerator :: hipStreamCaptureModeThreadLocal
    enumerator :: hipStreamCaptureModeRelaxed
  end enum

  enum,bind(c)
    enumerator :: hipStreamCaptureStatusNone = 0
    enumerator :: hipStreamCaptureStatusActive
    enumerator :: hipStreamCaptureStatusInvalidated
  end enum

  enum,bind(c)
    enumerator :: hipStreamAddCaptureDepENDencies = 0
    enumerator :: hipStreamSetCaptureDepENDencies
  end enum

  enum,bind(c)
    enumerator :: hipChannelFormatKindSigned = 0
    enumerator :: hipChannelFormatKindUnsigned = 1
    enumerator :: hipChannelFormatKindFloat = 2
    enumerator :: hipChannelFormatKindNone = 3
  end enum

  enum,bind(c)
    enumerator :: HIP_AD_FORMAT_UNSIGNED_INT8 = 1
    enumerator :: HIP_AD_FORMAT_UNSIGNED_INT16 = 2
    enumerator :: HIP_AD_FORMAT_UNSIGNED_INT32 = 3
    enumerator :: HIP_AD_FORMAT_SIGNED_INT8 = 8
    enumerator :: HIP_AD_FORMAT_SIGNED_INT16 = 9
    enumerator :: HIP_AD_FORMAT_SIGNED_INT32 = 10
    enumerator :: HIP_AD_FORMAT_HALF = 16
    enumerator :: HIP_AD_FORMAT_FLOAT = 32
  end enum

  enum,bind(c)
    enumerator :: hipResourceTYPEArray = 0
    enumerator :: hipResourceTYPEMipmappedArray = 1
    enumerator :: hipResourceTYPELinear = 2
    enumerator :: hipResourceTYPEPitch2D = 3
  end enum

  enum,bind(c)
    enumerator :: HIP_RESOURCE_TYPE_ARRAY = 0
    enumerator :: HIP_RESOURCE_TYPE_MIPMAPPED_ARRAY = 1
    enumerator :: HIP_RESOURCE_TYPE_LINEAR = 2
    enumerator :: HIP_RESOURCE_TYPE_PITCH2D = 3
  end enum

  enum,bind(c)
    enumerator :: HIP_TR_ADDRESS_MODE_WRAP = 0
    enumerator :: HIP_TR_ADDRESS_MODE_CLAMP = 1
    enumerator :: HIP_TR_ADDRESS_MODE_MIRROR = 2
    enumerator :: HIP_TR_ADDRESS_MODE_BORDER = 3
  end enum

  enum,bind(c)
    enumerator :: HIP_TR_FILTER_MODE_POINT = 0
    enumerator :: HIP_TR_FILTER_MODE_LINEAR = 1
  end enum

  enum,bind(c)
    enumerator :: hipResViewFormatNone = 0
    enumerator :: hipResViewFormatUnsignedChar1 = 1
    enumerator :: hipResViewFormatUnsignedChar2 = 2
    enumerator :: hipResViewFormatUnsignedChar4 = 3
    enumerator :: hipResViewFormatSignedChar1 = 4
    enumerator :: hipResViewFormatSignedChar2 = 5
    enumerator :: hipResViewFormatSignedChar4 = 6
    enumerator :: hipResViewFormatUnsignedShort1 = 7
    enumerator :: hipResViewFormatUnsignedShort2 = 8
    enumerator :: hipResViewFormatUnsignedShort4 = 9
    enumerator :: hipResViewFormatSignedShort1 = 10
    enumerator :: hipResViewFormatSignedShort2 = 11
    enumerator :: hipResViewFormatSignedShort4 = 12
    enumerator :: hipResViewFormatUnsignedInt1 = 13
    enumerator :: hipResViewFormatUnsignedInt2 = 14
    enumerator :: hipResViewFormatUnsignedInt4 = 15
    enumerator :: hipResViewFormatSignedInt1 = 16
    enumerator :: hipResViewFormatSignedInt2 = 17
    enumerator :: hipResViewFormatSignedInt4 = 18
    enumerator :: hipResViewFormatHalf1 = 19
    enumerator :: hipResViewFormatHalf2 = 20
    enumerator :: hipResViewFormatHalf4 = 21
    enumerator :: hipResViewFormatFloat1 = 22
    enumerator :: hipResViewFormatFloat2 = 23
    enumerator :: hipResViewFormatFloat4 = 24
    enumerator :: hipResViewFormatUnsignedBlockCompressed1 = 25
    enumerator :: hipResViewFormatUnsignedBlockCompressed2 = 26
    enumerator :: hipResViewFormatUnsignedBlockCompressed3 = 27
    enumerator :: hipResViewFormatUnsignedBlockCompressed4 = 28
    enumerator :: hipResViewFormatSignedBlockCompressed4 = 29
    enumerator :: hipResViewFormatUnsignedBlockCompressed5 = 30
    enumerator :: hipResViewFormatSignedBlockCompressed5 = 31
    enumerator :: hipResViewFormatUnsignedBlockCompressed6H = 32
    enumerator :: hipResViewFormatSignedBlockCompressed6H = 33
    enumerator :: hipResViewFormatUnsignedBlockCompressed7 = 34
  end enum

  enum,bind(c)
    enumerator :: HIP_RES_VIEW_FORMAT_NONE = 0
    enumerator :: HIP_RES_VIEW_FORMAT_UINT_1X8 = 1
    enumerator :: HIP_RES_VIEW_FORMAT_UINT_2X8 = 2
    enumerator :: HIP_RES_VIEW_FORMAT_UINT_4X8 = 3
    enumerator :: HIP_RES_VIEW_FORMAT_SINT_1X8 = 4
    enumerator :: HIP_RES_VIEW_FORMAT_SINT_2X8 = 5
    enumerator :: HIP_RES_VIEW_FORMAT_SINT_4X8 = 6
    enumerator :: HIP_RES_VIEW_FORMAT_UINT_1X16 = 7
    enumerator :: HIP_RES_VIEW_FORMAT_UINT_2X16 = 8
    enumerator :: HIP_RES_VIEW_FORMAT_UINT_4X16 = 9
    enumerator :: HIP_RES_VIEW_FORMAT_SINT_1X16 = 10
    enumerator :: HIP_RES_VIEW_FORMAT_SINT_2X16 = 11
    enumerator :: HIP_RES_VIEW_FORMAT_SINT_4X16 = 12
    enumerator :: HIP_RES_VIEW_FORMAT_UINT_1X32 = 13
    enumerator :: HIP_RES_VIEW_FORMAT_UINT_2X32 = 14
    enumerator :: HIP_RES_VIEW_FORMAT_UINT_4X32 = 15
    enumerator :: HIP_RES_VIEW_FORMAT_SINT_1X32 = 16
    enumerator :: HIP_RES_VIEW_FORMAT_SINT_2X32 = 17
    enumerator :: HIP_RES_VIEW_FORMAT_SINT_4X32 = 18
    enumerator :: HIP_RES_VIEW_FORMAT_FLOAT_1X16 = 19
    enumerator :: HIP_RES_VIEW_FORMAT_FLOAT_2X16 = 20
    enumerator :: HIP_RES_VIEW_FORMAT_FLOAT_4X16 = 21
    enumerator :: HIP_RES_VIEW_FORMAT_FLOAT_1X32 = 22
    enumerator :: HIP_RES_VIEW_FORMAT_FLOAT_2X32 = 23
    enumerator :: HIP_RES_VIEW_FORMAT_FLOAT_4X32 = 24
    enumerator :: HIP_RES_VIEW_FORMAT_UNSIGNED_BC1 = 25
    enumerator :: HIP_RES_VIEW_FORMAT_UNSIGNED_BC2 = 26
    enumerator :: HIP_RES_VIEW_FORMAT_UNSIGNED_BC3 = 27
    enumerator :: HIP_RES_VIEW_FORMAT_UNSIGNED_BC4 = 28
    enumerator :: HIP_RES_VIEW_FORMAT_SIGNED_BC4 = 29
    enumerator :: HIP_RES_VIEW_FORMAT_UNSIGNED_BC5 = 30
    enumerator :: HIP_RES_VIEW_FORMAT_SIGNED_BC5 = 31
    enumerator :: HIP_RES_VIEW_FORMAT_UNSIGNED_BC6H = 32
    enumerator :: HIP_RES_VIEW_FORMAT_SIGNED_BC6H = 33
    enumerator :: HIP_RES_VIEW_FORMAT_UNSIGNED_BC7 = 34
  end enum

  enum,bind(c)
    enumerator :: hipMemcpyHostToHost = 0
    enumerator :: hipMemcpyHostToDevice = 1
    enumerator :: hipMemcpyDeviceToHost = 2
    enumerator :: hipMemcpyDeviceToDevice = 3
    enumerator :: hipMemcpyDefault = 4
  end enum

  enum,bind(c)
    enumerator :: HIP_FUNC_ATTRIBUTE_MAX_THREADS_PER_BLOCK
    enumerator :: HIP_FUNC_ATTRIBUTE_SHARED_SIZE_BYTES
    enumerator :: HIP_FUNC_ATTRIBUTE_CONST_SIZE_BYTES
    enumerator :: HIP_FUNC_ATTRIBUTE_LOCAL_SIZE_BYTES
    enumerator :: HIP_FUNC_ATTRIBUTE_NUM_REGS
    enumerator :: HIP_FUNC_ATTRIBUTE_PTX_VERSION
    enumerator :: HIP_FUNC_ATTRIBUTE_BINARY_VERSION
    enumerator :: HIP_FUNC_ATTRIBUTE_CACHE_MODE_CA
    enumerator :: HIP_FUNC_ATTRIBUTE_MAX_DYNAMIC_SHARED_SIZE_BYTES
    enumerator :: HIP_FUNC_ATTRIBUTE_PREFERRED_SHARED_MEMORY_CARVEOUT
    enumerator :: HIP_FUNC_ATTRIBUTE_MAX
  end enum

  enum,bind(c)
    enumerator :: HIP_POINTER_ATTRIBUTE_CONTEXT = 1
    enumerator :: HIP_POINTER_ATTRIBUTE_MEMORY_TYPE
    enumerator :: HIP_POINTER_ATTRIBUTE_DEVICE_POINTER
    enumerator :: HIP_POINTER_ATTRIBUTE_HOST_POINTER
    enumerator :: HIP_POINTER_ATTRIBUTE_P2P_TOKENS
    enumerator :: HIP_POINTER_ATTRIBUTE_SYNC_MEMOPS
    enumerator :: HIP_POINTER_ATTRIBUTE_BUFFER_ID
    enumerator :: HIP_POINTER_ATTRIBUTE_IS_MANAGED
    enumerator :: HIP_POINTER_ATTRIBUTE_DEVICE_ORDINAL
    enumerator :: HIP_POINTER_ATTRIBUTE_IS_LEGACY_HIP_IPC_CAPABLE
    enumerator :: HIP_POINTER_ATTRIBUTE_RANGE_START_ADDR
    enumerator :: HIP_POINTER_ATTRIBUTE_RANGE_SIZE
    enumerator :: HIP_POINTER_ATTRIBUTE_MAPPED
    enumerator :: HIP_POINTER_ATTRIBUTE_ALLOWED_HANDLE_TYPES
    enumerator :: HIP_POINTER_ATTRIBUTE_IS_GPU_DIRECT_RDMA_CAPABLE
    enumerator :: HIP_POINTER_ATTRIBUTE_ACCESS_FLAGS
    enumerator :: HIP_POINTER_ATTRIBUTE_MEMPOOL_HANDLE
  end enum

  enum,bind(c)
    enumerator :: hipAddressModeWrap = 0
    enumerator :: hipAddressModeClamp = 1
    enumerator :: hipAddressModeMirror = 2
    enumerator :: hipAddressModeBorder = 3
  end enum

  enum,bind(c)
    enumerator :: hipFilterModePoint = 0
    enumerator :: hipFilterModeLinear = 1
  end enum

  enum,bind(c)
    enumerator :: hipReadModeElementTYPE = 0
    enumerator :: hipReadModeNormalizedFloat = 1
  end enum

  enum,bind(c)
    enumerator :: HIP_R_16F = 2
    enumerator :: HIP_R_32F = 0
    enumerator :: HIP_R_64F = 1
    enumerator :: HIP_C_16F = 6
    enumerator :: HIP_C_32F = 4
    enumerator :: HIP_C_64F = 5
  end enum

  enum,bind(c)
    enumerator :: HIP_LIBRARY_MAJOR_VERSION
    enumerator :: HIP_LIBRARY_MINOR_VERSION
    enumerator :: HIP_LIBRARY_PATCH_LEVEL
  end enum

end module FGPUTensor_HIP_enums
