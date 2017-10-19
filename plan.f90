program plan
   implicit none
   real(8) :: pt1(1:3)
   real(8) :: pt2(1:3)
   real(8) :: pt3(1:3)
   real(8) :: v1(1:3),v2(1:3),v3(1:3),d
   
   write(*,'(3x,"pt1: ")',advance='no') ; read(*,*)pt1(1:3)
   write(*,'(3x,"pt2: ")',advance='no') ; read(*,*)pt2(1:3)
   write(*,'(3x,"pt3: ")',advance='no') ; read(*,*)pt3(1:3)
   
   v2(1:3)=pt2(1:3)-pt1(1:3)
   v3(1:3)=pt3(1:3)-pt1(1:3)
   
   !> v1= v2 ^v3
   
   v1(1) =  v2(2)*v3(3)-v2(3)*v3(2)
   v1(2) =-(v2(1)*v3(3)-v2(3)*v3(1))
   v1(3) =  v2(1)*v3(2)-v2(2)*v3(1)
   
   print '(/"vecteur normal au plan v=",3(f12.5,1x))',v1(1:3)
   
   d=-(v1(1)*pt1(1)+v1(2)*pt1(2)+v1(3)*pt1(3))
   print '(/"plan: ",f12.5," x + ",f12.5," y + ",f12.5," z + ",f12.5," = 0")',v1(1:3),d
   
   !d=-(v1(1)*pt2(1)+v1(2)*pt2(2)+v1(3)*pt2(3))
   !print '(/"plan: ",f12.5," x + ",f12.5," y + ",f12.5," z + ",f12.5," = 0")',v1(1:3),d
   
   !d=-(v1(1)*pt3(1)+v1(2)*pt3(2)+v1(3)*pt3(3))
   !print '(/"plan: ",f12.5," x + ",f12.5," y + ",f12.5," z + ",f12.5," = 0")',v1(1:3),d
   
end program plan