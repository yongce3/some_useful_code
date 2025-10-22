! ==========================================================
! 编译：gfortran hello.f90 -o hello
! 运行：./hello
! ==========================================================

program basics
  implicit none                          


  integer            :: i, n
  real               :: x, y
  real               :: arr(5)           
  logical            :: ok
  character(len=20)  :: name
  integer, parameter :: NMAX = 5         
  real,    parameter :: PI   = 3.14159   


  x = 1.5
  y = 2.0
  arr = (/ 1.0, 2.0, 3.0, 4.0, 5.0 /)

  
  write(*,*) 'Hello, Fortran 90!'                    
  write(*,'(A,F7.4)') 'PI ≈ ', PI                    

  
  write(*,*) '请输入你的名字（回车结束）：'
  read(*,'(A)') name
  write(*,*) '你好，', trim(name), '！'

  
  ok = (x < y)
  if (ok) then
     write(*,*) '判断：x < y 成立'
  else
     write(*,*) '判断：x < y 不成立'
  end if

  
  select case (NMAX)
  case (1:3)
     write(*,*) 'NMAX 在 1~3 之间'
  case (4,5)
     write(*,*) 'NMAX 是 4 或 5'
  case default
     write(*,*) 'NMAX 是其他值'
  end select

  
  write(*,*) '数组元素：'
  do i = 1, NMAX
     write(*,'(A,I0,A,F6.2)') 'arr(', i, ') = ', arr(i)
  end do

  ! DO WHILE 风格
  i = 1
  do while (i <= NMAX)
     i = i + 1
  end do

 
  write(*,'(A,F6.2)') 'sum(arr)  = ', sum(arr)
  write(*,'(A,F6.2)') 'mean(arr) = ', sum(arr)/real(NMAX)
  write(*,'(A,F6.2)') 'max(arr)  = ', maxval(arr)
  write(*,'(A,F6.2)') 'min(arr)  = ', minval(arr)

  
  call swap(x, y)    
  write(*,'(A,F6.2,A,F6.2)') '交换后：x=', x, ' y=', y
  write(*,'(A,F6.2)') '二维范数：norm2(arr) = ', norm2(arr)

  
  open(unit=10, file='out.txt', status='replace')   
  write(10,*) 'name = ', trim(name)
  write(10,*) 'sum(arr) = ', sum(arr)
  close(10)
  write(*,*) '已写出 out.txt'

contains
  

  subroutine swap(a, b)
    real, intent(inout) :: a, b
    real :: t
    t = a; a = b; b = t
  end subroutine swap

  real function norm2(v)
    real, intent(in) :: v(:)         
    norm2 = sqrt(sum(v * v))
  end function norm2

end program basics
