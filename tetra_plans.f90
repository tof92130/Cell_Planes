program plan
   implicit none
   
   real(8) :: tetra(1:3,1:4)
   
   integer :: iSide
   integer :: nod(1:3)
   
   real(8) :: pt1(1:3)
   real(8) :: pt2(1:3)
   real(8) :: pt3(1:3)
   real(8) :: v1(1:3),v2(1:3),v3(1:3),d
   
! Vertices
! 6 
! 0.0 0.0 -1.0  1
! 1.0 0.0 -1.0  2
! 0.0 1.0 -1.0  3
! 0.0 0.0 +1.0  4
! 1.0 0.0 +1.0  5
! 0.0 1.0 +1.0  6

! Quadrilaterals
! 3
! 1 2 5 4  1
! 2 3 6 5  2
! 3 1 4 6  3

! Triangles
! 2
! 1 3 2  4
! 4 5 6  5

   write(*,'(3x,"tetra: ")',advance='no') ; read(*,*)tetra(1:3,1:4)
   
   do iSide=1,4
     
     select case(iSide)
     case(1) ; nod(1:3)=[2,3,4] ; print '("# xyz=",3(3(i2,",")," ; ")$)',int(tetra(1:3,nod(1:3)))
     case(2) ; nod(1:3)=[1,4,3] ; print '("# xyz=",3(3(i2,",")," ; ")$)',int(tetra(1:3,nod(1:3)))
     case(3) ; nod(1:3)=[1,2,4] ; print '("# xyz=",3(3(i2,",")," ; ")$)',int(tetra(1:3,nod(1:3)))
     case(4) ; nod(1:3)=[1,3,2] ; print '("# xyz=",3(3(i2,",")," ; ")$)',int(tetra(1:3,nod(1:3)))
     end select
     
     pt1(1:3)=tetra(1:3,nod(1))
     pt2(1:3)=tetra(1:3,nod(2))
     pt3(1:3)=tetra(1:3,nod(3))
     
     v2(1:3)=pt2(1:3)-pt1(1:3)
     v3(1:3)=pt3(1:3)-pt1(1:3)
     
     !> v1= v2 ^v3
     
     v1(1) =  v2(2)*v3(3)-v2(3)*v3(2)
     v1(2) =-(v2(1)*v3(3)-v2(3)*v3(1))
     v1(3) =  v2(1)*v3(2)-v2(2)*v3(1)
    !print '("vecteur normal au plan v=",3(f5.2,1x))',v1(1:3)
     
     d=-(v1(1)*pt1(1)+v1(2)*pt1(2)+v1(3)*pt1(3))
     print '( "Plan: "$)'
     if( .not.v1(1)==0d0 ) print '(    i2," u"$)',int(v1(1))
     if( .not.v1(2)==0d0 ) print '("+",i2," v"$)',int(v1(2))
     if( .not.v1(3)==0d0 ) print '("+",i2," w"$)',int(v1(3))
     if( .not.    d==0d0 ) print '("+",i2,    $)',int(d)
     print '("=0")'
     
    !print '( "Plan: ",i2," u + ",i2," v + ",i2," w + ",i2," = 0")',int(v1(1:3)),int(d)
     
    !d=-(v1(1)*pt2(1)+v1(2)*pt2(2)+v1(3)*pt2(3))
    !print '(/"plan: ",f5.2," x + ",f5.2," y + ",f5.2," z + ",f5.2," = 0")',v1(1:3),d
     
    !d=-(v1(1)*pt3(1)+v1(2)*pt3(2)+v1(3)*pt3(3))
    !print '(/"plan: ",f5.2," x + ",f5.2," y + ",f5.2," z + ",f5.2," = 0")',v1(1:3),d
   enddo
   
end program plan