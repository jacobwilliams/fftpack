!*==CFFTB1.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      subroutine cfftb1(n,c,Ch,Wa,Ifac)
      use fftpack_kind
      implicit none
!*--CFFTB15
!*** Start of declarations inserted by SPAG
      real c , Ch , fftpack_kind , rk , Wa
      integer i , idl1 , ido , idot , Ifac , ip , iw , ix2 , ix3 , ix4 ,&
            & k1 , l1 , l2 , n , n2 , na , nac , nf
!*** End of declarations inserted by SPAG
      dimension Ch(*) , c(*) , Wa(*) , Ifac(*)
      nf = Ifac(2)
      na = 0
      l1 = 1
      iw = 1
      do k1 = 1 , nf
         ip = Ifac(k1+2)
         l2 = ip*l1
         ido = n/l2
         idot = ido + ido
         idl1 = idot*l1
         if ( ip==4 ) then
            ix2 = iw + idot
            ix3 = ix2 + idot
            if ( na/=0 ) then
               call passb4(idot,l1,Ch,c,Wa(iw),Wa(ix2),Wa(ix3))
            else
               call passb4(idot,l1,c,Ch,Wa(iw),Wa(ix2),Wa(ix3))
            endif
            na = 1 - na
         elseif ( ip==2 ) then
            if ( na/=0 ) then
               call passb2(idot,l1,Ch,c,Wa(iw))
            else
               call passb2(idot,l1,c,Ch,Wa(iw))
            endif
            na = 1 - na
         elseif ( ip==3 ) then
            ix2 = iw + idot
            if ( na/=0 ) then
               call passb3(idot,l1,Ch,c,Wa(iw),Wa(ix2))
            else
               call passb3(idot,l1,c,Ch,Wa(iw),Wa(ix2))
            endif
            na = 1 - na
         elseif ( ip/=5 ) then
            if ( na/=0 ) then
               call passb(nac,idot,ip,l1,idl1,Ch,Ch,Ch,c,c,Wa(iw))
            else
               call passb(nac,idot,ip,l1,idl1,c,c,c,Ch,Ch,Wa(iw))
            endif
            if ( nac/=0 ) na = 1 - na
         else
            ix2 = iw + idot
            ix3 = ix2 + idot
            ix4 = ix3 + idot
            if ( na/=0 ) then
               call passb5(idot,l1,Ch,c,Wa(iw),Wa(ix2),Wa(ix3),Wa(ix4))
            else
               call passb5(idot,l1,c,Ch,Wa(iw),Wa(ix2),Wa(ix3),Wa(ix4))
            endif
            na = 1 - na
         endif
         l1 = l2
         iw = iw + (ip-1)*idot
      enddo
      if ( na==0 ) return
      n2 = n + n
      do i = 1 , n2
         c(i) = Ch(i)
      enddo
      end subroutine cfftb1