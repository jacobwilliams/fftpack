!*==PASSF5.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      subroutine passf5(Ido,l1,Cc,Ch,Wa1,Wa2,Wa3,Wa4)
      use fftpack_kind
      implicit none
!*--PASSF51361
!*** Start of declarations inserted by SPAG
      real Cc , Ch , ci2 , ci3 , ci4 , ci5 , cr2 , cr3 , cr4 , cr5 ,    &
         & di2 , di3 , di4 , di5 , dr2 , dr3 , dr4 , dr5 ,              &
         & fftpack_kind , rk
      real ti11 , ti12 , ti2 , ti3 , ti4 , ti5 , tr11 , tr12 , tr2 ,    &
         & tr3 , tr4 , tr5 , Wa1 , Wa2 , Wa3 , Wa4
      integer i , Ido , k , l1
!*** End of declarations inserted by SPAG
      dimension Cc(Ido,5,l1) , Ch(Ido,l1,5) , Wa1(1) , Wa2(1) , Wa3(1) ,&
              & Wa4(1)
!     *** TR11=COS(2*PI/5), TI11=-SIN(2*PI/5)
!     *** TR12=-COS(4*PI/5), TI12=-SIN(4*PI/5)
      data tr11 , ti11 , tr12 , ti12/0.3090169943749474241d0 ,          &
         & -0.95105651629515357212d0 , -0.8090169943749474241d0 ,       &
         & -0.58778525229247312917d0/
      if ( Ido/=2 ) then
         do k = 1 , l1
            do i = 2 , Ido , 2
               ti5 = Cc(i,2,k) - Cc(i,5,k)
               ti2 = Cc(i,2,k) + Cc(i,5,k)
               ti4 = Cc(i,3,k) - Cc(i,4,k)
               ti3 = Cc(i,3,k) + Cc(i,4,k)
               tr5 = Cc(i-1,2,k) - Cc(i-1,5,k)
               tr2 = Cc(i-1,2,k) + Cc(i-1,5,k)
               tr4 = Cc(i-1,3,k) - Cc(i-1,4,k)
               tr3 = Cc(i-1,3,k) + Cc(i-1,4,k)
               Ch(i-1,k,1) = Cc(i-1,1,k) + tr2 + tr3
               Ch(i,k,1) = Cc(i,1,k) + ti2 + ti3
               cr2 = Cc(i-1,1,k) + tr11*tr2 + tr12*tr3
               ci2 = Cc(i,1,k) + tr11*ti2 + tr12*ti3
               cr3 = Cc(i-1,1,k) + tr12*tr2 + tr11*tr3
               ci3 = Cc(i,1,k) + tr12*ti2 + tr11*ti3
               cr5 = ti11*tr5 + ti12*tr4
               ci5 = ti11*ti5 + ti12*ti4
               cr4 = ti12*tr5 - ti11*tr4
               ci4 = ti12*ti5 - ti11*ti4
               dr3 = cr3 - ci4
               dr4 = cr3 + ci4
               di3 = ci3 + cr4
               di4 = ci3 - cr4
               dr5 = cr2 + ci5
               dr2 = cr2 - ci5
               di5 = ci2 - cr5
               di2 = ci2 + cr5
               Ch(i-1,k,2) = Wa1(i-1)*dr2 + Wa1(i)*di2
               Ch(i,k,2) = Wa1(i-1)*di2 - Wa1(i)*dr2
               Ch(i-1,k,3) = Wa2(i-1)*dr3 + Wa2(i)*di3
               Ch(i,k,3) = Wa2(i-1)*di3 - Wa2(i)*dr3
               Ch(i-1,k,4) = Wa3(i-1)*dr4 + Wa3(i)*di4
               Ch(i,k,4) = Wa3(i-1)*di4 - Wa3(i)*dr4
               Ch(i-1,k,5) = Wa4(i-1)*dr5 + Wa4(i)*di5
               Ch(i,k,5) = Wa4(i-1)*di5 - Wa4(i)*dr5
            enddo
         enddo
         goto 99999
      endif
      do k = 1 , l1
         ti5 = Cc(2,2,k) - Cc(2,5,k)
         ti2 = Cc(2,2,k) + Cc(2,5,k)
         ti4 = Cc(2,3,k) - Cc(2,4,k)
         ti3 = Cc(2,3,k) + Cc(2,4,k)
         tr5 = Cc(1,2,k) - Cc(1,5,k)
         tr2 = Cc(1,2,k) + Cc(1,5,k)
         tr4 = Cc(1,3,k) - Cc(1,4,k)
         tr3 = Cc(1,3,k) + Cc(1,4,k)
         Ch(1,k,1) = Cc(1,1,k) + tr2 + tr3
         Ch(2,k,1) = Cc(2,1,k) + ti2 + ti3
         cr2 = Cc(1,1,k) + tr11*tr2 + tr12*tr3
         ci2 = Cc(2,1,k) + tr11*ti2 + tr12*ti3
         cr3 = Cc(1,1,k) + tr12*tr2 + tr11*tr3
         ci3 = Cc(2,1,k) + tr12*ti2 + tr11*ti3
         cr5 = ti11*tr5 + ti12*tr4
         ci5 = ti11*ti5 + ti12*ti4
         cr4 = ti12*tr5 - ti11*tr4
         ci4 = ti12*ti5 - ti11*ti4
         Ch(1,k,2) = cr2 - ci5
         Ch(1,k,5) = cr2 + ci5
         Ch(2,k,2) = ci2 + cr5
         Ch(2,k,3) = ci3 + cr4
         Ch(1,k,3) = cr3 - ci4
         Ch(1,k,4) = cr3 + ci4
         Ch(2,k,4) = ci3 - cr4
         Ch(2,k,5) = ci2 - cr5
      enddo
      return
99999 end subroutine passf5