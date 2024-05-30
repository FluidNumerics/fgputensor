program simple_2d_array
  use FGPUTensor

  type(tensor_f32_r2) :: t

  call t % init((/100,100/),0)

  call t % info()

  call t % free()

end program simple_2d_array
