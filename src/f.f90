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
      end
!*==CFFTF1.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      subroutine cfftf1(n,c,Ch,Wa,Ifac)
      use fftpack_kind
      implicit none
!*--CFFTF177
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
               call passf4(idot,l1,Ch,c,Wa(iw),Wa(ix2),Wa(ix3))
            else
               call passf4(idot,l1,c,Ch,Wa(iw),Wa(ix2),Wa(ix3))
            endif
            na = 1 - na
         elseif ( ip==2 ) then
            if ( na/=0 ) then
               call passf2(idot,l1,Ch,c,Wa(iw))
            else
               call passf2(idot,l1,c,Ch,Wa(iw))
            endif
            na = 1 - na
         elseif ( ip==3 ) then
            ix2 = iw + idot
            if ( na/=0 ) then
               call passf3(idot,l1,Ch,c,Wa(iw),Wa(ix2))
            else
               call passf3(idot,l1,c,Ch,Wa(iw),Wa(ix2))
            endif
            na = 1 - na
         elseif ( ip/=5 ) then
            if ( na/=0 ) then
               call passf(nac,idot,ip,l1,idl1,Ch,Ch,Ch,c,c,Wa(iw))
            else
               call passf(nac,idot,ip,l1,idl1,c,c,c,Ch,Ch,Wa(iw))
            endif
            if ( nac/=0 ) na = 1 - na
         else
            ix2 = iw + idot
            ix3 = ix2 + idot
            ix4 = ix3 + idot
            if ( na/=0 ) then
               call passf5(idot,l1,Ch,c,Wa(iw),Wa(ix2),Wa(ix3),Wa(ix4))
            else
               call passf5(idot,l1,c,Ch,Wa(iw),Wa(ix2),Wa(ix3),Wa(ix4))
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
      end
!*==CFFTI1.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      subroutine cffti1(n,Wa,Ifac)
      use fftpack_kind
      implicit none
!*--CFFTI1149
!*** Start of declarations inserted by SPAG
      real arg , argh , argld , fftpack_kind , fi , rk , tpi , Wa
      integer i , i1 , ib , ido , idot , Ifac , ii , ip , ipm , j , k1 ,&
            & l1 , l2 , ld , n , nf , nl , nq , nr , ntry
      integer ntryh
!*** End of declarations inserted by SPAG
      dimension Wa(*) , Ifac(*) , ntryh(4)
      data ntryh(1) , ntryh(2) , ntryh(3) , ntryh(4)/3 , 4 , 2 , 5/
      nl = n
      nf = 0
      j = 0
 100  j = j + 1
      if ( j<=4 ) then
         ntry = ntryh(j)
      else
         ntry = ntry + 2
      endif
 200  nq = nl/ntry
      nr = nl - ntry*nq
      if ( nr/=0 ) goto 100
      nf = nf + 1
      Ifac(nf+2) = ntry
      nl = nq
      if ( ntry==2 ) then
         if ( nf/=1 ) then
            do i = 2 , nf
               ib = nf - i + 2
               Ifac(ib+2) = Ifac(ib+1)
            enddo
            Ifac(3) = 2
         endif
      endif
      if ( nl/=1 ) goto 200
      Ifac(1) = n
      Ifac(2) = nf
      tpi = 6.28318530717958647692d0
      argh = tpi/real(n,rk)
      i = 2
      l1 = 1
      do k1 = 1 , nf
         ip = Ifac(k1+2)
         ld = 0
         l2 = l1*ip
         ido = n/l2
         idot = ido + ido + 2
         ipm = ip - 1
         do j = 1 , ipm
            i1 = i
            Wa(i-1) = 1.0d0
            Wa(i) = 0.0d0
            ld = ld + l1
            fi = 0.0d0
            argld = real(ld,rk)*argh
            do ii = 4 , idot , 2
               i = i + 2
               fi = fi + 1.d0
               arg = fi*argld
               Wa(i-1) = cos(arg)
               Wa(i) = sin(arg)
            enddo
            if ( ip>5 ) then
               Wa(i1-1) = Wa(i-1)
               Wa(i1) = Wa(i)
            endif
         enddo
         l1 = l2
      enddo
      end
!*==COSQB1.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      subroutine cosqb1(n,x,w,Xh)
      use fftpack_kind
      implicit none
!*--COSQB1222
!*** Start of declarations inserted by SPAG
      real fftpack_kind , rk , w , x , Xh , xim1
      integer i , k , kc , modn , n , np2 , ns2
!*** End of declarations inserted by SPAG
      dimension x(1) , w(1) , Xh(1)
      ns2 = (n+1)/2
      np2 = n + 2
      do i = 3 , n , 2
         xim1 = x(i-1) + x(i)
         x(i) = x(i) - x(i-1)
         x(i-1) = xim1
      enddo
      x(1) = x(1) + x(1)
      modn = mod(n,2)
      if ( modn==0 ) x(n) = x(n) + x(n)
      call dfftb(n,x,Xh)
      do k = 2 , ns2
         kc = np2 - k
         Xh(k) = w(k-1)*x(kc) + w(kc-1)*x(k)
         Xh(kc) = w(k-1)*x(k) - w(kc-1)*x(kc)
      enddo
      if ( modn==0 ) x(ns2+1) = w(ns2)*(x(ns2+1)+x(ns2+1))
      do k = 2 , ns2
         kc = np2 - k
         x(k) = Xh(k) + Xh(kc)
         x(kc) = Xh(k) - Xh(kc)
      enddo
      x(1) = x(1) + x(1)
      end
!*==COSQF1.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      subroutine cosqf1(n,x,w,Xh)
      use fftpack_kind
      implicit none
!*--COSQF1256
!*** Start of declarations inserted by SPAG
      real fftpack_kind , rk , w , x , Xh , xim1
      integer i , k , kc , modn , n , np2 , ns2
!*** End of declarations inserted by SPAG
      dimension x(1) , w(1) , Xh(1)
      ns2 = (n+1)/2
      np2 = n + 2
      do k = 2 , ns2
         kc = np2 - k
         Xh(k) = x(k) + x(kc)
         Xh(kc) = x(k) - x(kc)
      enddo
      modn = mod(n,2)
      if ( modn==0 ) Xh(ns2+1) = x(ns2+1) + x(ns2+1)
      do k = 2 , ns2
         kc = np2 - k
         x(k) = w(k-1)*Xh(kc) + w(kc-1)*Xh(k)
         x(kc) = w(k-1)*Xh(k) - w(kc-1)*Xh(kc)
      enddo
      if ( modn==0 ) x(ns2+1) = w(ns2)*Xh(ns2+1)
      call dfftf(n,x,Xh)
      do i = 3 , n , 2
         xim1 = x(i-1) - x(i)
         x(i) = x(i-1) + x(i)
         x(i-1) = xim1
      enddo
      end
!*==DCOSQB.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      subroutine dcosqb(n,x,Wsave)
      use fftpack_kind
      implicit none
!*--DCOSQB288
!*** Start of declarations inserted by SPAG
      real fftpack_kind , rk , tsqrt2 , Wsave , x , x1
      integer n
!*** End of declarations inserted by SPAG
      dimension x(*) , Wsave(*)
      data tsqrt2/2.82842712474619009760d0/
      if ( n<2 ) then
         x(1) = 4.0d0*x(1)
         return
      elseif ( n==2 ) then
         x1 = 4.0d0*(x(1)+x(2))
         x(2) = tsqrt2*(x(1)-x(2))
         x(1) = x1
         return
      else
         call cosqb1(n,x,Wsave,Wsave(n+1))
      endif
      end
!*==DCOSQF.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      subroutine dcosqf(n,x,Wsave)
      use fftpack_kind
      implicit none
!*--DCOSQF311
!*** Start of declarations inserted by SPAG
      real fftpack_kind , rk , sqrt2 , tsqx , Wsave , x
      integer n
!*** End of declarations inserted by SPAG
      dimension x(*) , Wsave(*)
      data sqrt2/1.41421356237309504880d0/
      if ( n<2 ) then
      elseif ( n==2 ) then
         tsqx = sqrt2*x(2)
         x(2) = x(1) - tsqx
         x(1) = x(1) + tsqx
      else
         call cosqf1(n,x,Wsave,Wsave(n+1))
         goto 99999
      endif
      return
99999 end
!*==DCOSQI.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      subroutine dcosqi(n,Wsave)
      use fftpack_kind
      implicit none
!*--DCOSQI333
!*** Start of declarations inserted by SPAG
      real dt , fftpack_kind , fk , pih , rk , Wsave
      integer k , n
!*** End of declarations inserted by SPAG
      dimension Wsave(1)
      data pih/1.57079632679489661923d0/
      dt = pih/real(n,rk)
      fk = 0.0d0
      do k = 1 , n
         fk = fk + 1.0d0
         Wsave(k) = cos(fk*dt)
      enddo
      call dffti(n,Wsave(n+1))
      end
!*==DCOST.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      subroutine dcost(n,x,Wsave)
      use fftpack_kind
      implicit none
!*--DCOST352
!*** Start of declarations inserted by SPAG
      real c1 , fftpack_kind , rk , t1 , t2 , tx2 , Wsave , x , x1h ,   &
         & x1p3 , xi , xim2
      integer i , k , kc , modn , n , nm1 , np1 , ns2
!*** End of declarations inserted by SPAG
      dimension x(*) , Wsave(*)
      nm1 = n - 1
      np1 = n + 1
      ns2 = n/2
      if ( n<2 ) goto 99999
      if ( n==2 ) then
         x1h = x(1) + x(2)
         x(2) = x(1) - x(2)
         x(1) = x1h
         return
      elseif ( n>3 ) then
         c1 = x(1) - x(n)
         x(1) = x(1) + x(n)
         do k = 2 , ns2
            kc = np1 - k
            t1 = x(k) + x(kc)
            t2 = x(k) - x(kc)
            c1 = c1 + Wsave(kc)*t2
            t2 = Wsave(k)*t2
            x(k) = t1 - t2
            x(kc) = t1 + t2
         enddo
         modn = mod(n,2)
         if ( modn/=0 ) x(ns2+1) = x(ns2+1) + x(ns2+1)
         call dfftf(nm1,x,Wsave(n+1))
         xim2 = x(2)
         x(2) = c1
         do i = 4 , n , 2
            xi = x(i)
            x(i) = x(i-2) - x(i-1)
            x(i-1) = xim2
            xim2 = xi
         enddo
         if ( modn/=0 ) x(n) = xim2
         goto 99999
      endif
      x1p3 = x(1) + x(3)
      tx2 = x(2) + x(2)
      x(2) = x(1) - x(3)
      x(1) = x1p3 + tx2
      x(3) = x1p3 - tx2
      return
99999 end
!*==DCOSTI.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      subroutine dcosti(n,Wsave)
      use fftpack_kind
      implicit none
!*--DCOSTI405
!*** Start of declarations inserted by SPAG
      real dt , fftpack_kind , fk , pi , rk , Wsave
      integer k , kc , n , nm1 , np1 , ns2
!*** End of declarations inserted by SPAG
      dimension Wsave(1)
      data pi/3.14159265358979323846d0/
      if ( n<=3 ) return
      nm1 = n - 1
      np1 = n + 1
      ns2 = n/2
      dt = pi/real(nm1,rk)
      fk = 0.0d0
      do k = 2 , ns2
         kc = np1 - k
         fk = fk + 1.0d0
         Wsave(k) = 2.0d0*sin(fk*dt)
         Wsave(kc) = 2.0d0*cos(fk*dt)
      enddo
      call dffti(nm1,Wsave(n+1))
      end
!*==DFFTB.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      subroutine dfftb(n,r,Wsave)
      use fftpack_kind
      implicit none
!*--DFFTB430
!*** Start of declarations inserted by SPAG
      real fftpack_kind , r , rk , Wsave
      integer n
!*** End of declarations inserted by SPAG
      dimension r(1) , Wsave(1)
      if ( n==1 ) return
      call rfftb1(n,r,Wsave,Wsave(n+1),Wsave(2*n+1))
      end
!*==DFFTF.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      subroutine dfftf(n,r,Wsave)
      use fftpack_kind
      implicit none
!*--DFFTF443
!*** Start of declarations inserted by SPAG
      real fftpack_kind , r , rk , Wsave
      integer n
!*** End of declarations inserted by SPAG
      dimension r(1) , Wsave(1)
      if ( n==1 ) return
      call rfftf1(n,r,Wsave,Wsave(n+1),Wsave(2*n+1))
      end
!*==DFFTI.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      subroutine dffti(n,Wsave)
      use fftpack_kind
      implicit none
!*--DFFTI456
!*** Start of declarations inserted by SPAG
      real fftpack_kind , rk , Wsave
      integer n
!*** End of declarations inserted by SPAG
      dimension Wsave(1)
      if ( n==1 ) return
      call rffti1(n,Wsave(n+1),Wsave(2*n+1))
      end
!*==DSINQB.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      subroutine dsinqb(n,x,Wsave)
      use fftpack_kind
      implicit none
!*--DSINQB469
!*** Start of declarations inserted by SPAG
      real fftpack_kind , rk , Wsave , x , xhold
      integer k , kc , n , ns2
!*** End of declarations inserted by SPAG
      dimension x(1) , Wsave(1)
      if ( n>1 ) then
         ns2 = n/2
         do k = 2 , n , 2
            x(k) = -x(k)
         enddo
         call dcosqb(n,x,Wsave)
         do k = 1 , ns2
            kc = n - k
            xhold = x(k)
            x(k) = x(kc+1)
            x(kc+1) = xhold
         enddo
         goto 99999
      endif
      x(1) = 4.0d0*x(1)
      return
99999 end
!*==DSINQF.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      subroutine dsinqf(n,x,Wsave)
      use fftpack_kind
      implicit none
!*--DSINQF496
!*** Start of declarations inserted by SPAG
      real fftpack_kind , rk , Wsave , x , xhold
      integer k , kc , n , ns2
!*** End of declarations inserted by SPAG
      dimension x(1) , Wsave(1)
      if ( n==1 ) return
      ns2 = n/2
      do k = 1 , ns2
         kc = n - k
         xhold = x(k)
         x(k) = x(kc+1)
         x(kc+1) = xhold
      enddo
      call dcosqf(n,x,Wsave)
      do k = 2 , n , 2
         x(k) = -x(k)
      enddo
      end
!*==DSINQI.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      subroutine dsinqi(n,Wsave)
      use fftpack_kind
      implicit none
!*--DSINQI519
!*** Start of declarations inserted by SPAG
      real fftpack_kind , rk , Wsave
      integer n
!*** End of declarations inserted by SPAG
      dimension Wsave(1)
      call dcosqi(n,Wsave)
      end
!*==DSINT.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      subroutine dsint(n,x,Wsave)
      use fftpack_kind
      implicit none
!*--DSINT531
!*** Start of declarations inserted by SPAG
      real fftpack_kind , rk , Wsave , x
      integer iw1 , iw2 , iw3 , n , np1
!*** End of declarations inserted by SPAG
      dimension x(1) , Wsave(1)
      np1 = n + 1
      iw1 = n/2 + 1
      iw2 = iw1 + np1
      iw3 = iw2 + np1
      call sint1(n,x,Wsave,Wsave(iw1),Wsave(iw2),Wsave(iw3))
      end
!*==DSINTI.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      subroutine dsinti(n,Wsave)
      use fftpack_kind
      implicit none
!*--DSINTI547
!*** Start of declarations inserted by SPAG
      real dt , fftpack_kind , pi , rk , Wsave
      integer k , n , np1 , ns2
!*** End of declarations inserted by SPAG
      dimension Wsave(1)
      data pi/3.14159265358979323846d0/
      if ( n<=1 ) return
      ns2 = n/2
      np1 = n + 1
      dt = pi/real(np1,rk)
      do k = 1 , ns2
         Wsave(k) = 2.0d0*sin(k*dt)
      enddo
      call dffti(np1,Wsave(ns2+1))
      end
!*==DZFFTB.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      subroutine dzfftb(n,r,Azero,a,b,Wsave)
      use fftpack_kind
      implicit none
!*--DZFFTB567
!*** Start of declarations inserted by SPAG
      real a , Azero , b , fftpack_kind , r , rk , Wsave
      integer i , n , ns2
!*** End of declarations inserted by SPAG
      dimension r(*) , a(*) , b(*) , Wsave(*)
      if ( n<2 ) then
         r(1) = Azero
         return
      elseif ( n==2 ) then
         r(1) = Azero + a(1)
         r(2) = Azero - a(1)
         return
      else
         ns2 = (n-1)/2
         do i = 1 , ns2
            r(2*i) = 0.5d0*a(i)
            r(2*i+1) = -0.5d0*b(i)
         enddo
         r(1) = Azero
         if ( mod(n,2)==0 ) r(n) = a(ns2+1)
         call dfftb(n,r,Wsave(n+1))
      endif
      end
!*==DZFFTF.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      subroutine dzfftf(n,r,Azero,a,b,Wsave)
!
!                       VERSION 3  JUNE 1979
!
      use fftpack_kind
      implicit none
!*--DZFFTF598
!*** Start of declarations inserted by SPAG
      real a , Azero , b , cf , cfm , fftpack_kind , r , rk , Wsave
      integer i , n , ns2 , ns2m
!*** End of declarations inserted by SPAG
      dimension r(*) , a(*) , b(*) , Wsave(*)
      if ( n<2 ) then
         Azero = r(1)
         return
      elseif ( n==2 ) then
         Azero = 0.5d0*(r(1)+r(2))
         a(1) = 0.5d0*(r(1)-r(2))
         return
      else
         do i = 1 , n
            Wsave(i) = r(i)
         enddo
         call dfftf(n,Wsave,Wsave(n+1))
         cf = 2.0d0/real(n,rk)
         cfm = -cf
         Azero = 0.5d0*cf*Wsave(1)
         ns2 = (n+1)/2
         ns2m = ns2 - 1
         do i = 1 , ns2m
            a(i) = cf*Wsave(2*i)
            b(i) = cfm*Wsave(2*i+1)
         enddo
         if ( mod(n,2)==1 ) return
         a(ns2) = 0.5d0*cf*Wsave(n)
         b(ns2) = 0.0d0
      endif
      end
!*==DZFFTI.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      subroutine dzffti(n,Wsave)
      use fftpack_kind
      implicit none
!*--DZFFTI634
!*** Start of declarations inserted by SPAG
      real fftpack_kind , rk , Wsave
      integer n
!*** End of declarations inserted by SPAG
      dimension Wsave(1)
      if ( n==1 ) return
      call ezfft1(n,Wsave(2*n+1),Wsave(3*n+1))
      end
!*==EZFFT1.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      subroutine ezfft1(n,Wa,Ifac)
      use fftpack_kind
      implicit none
!*--EZFFT1647
!*** Start of declarations inserted by SPAG
      real arg1 , argh , ch1 , ch1h , dch1 , dsh1 , fftpack_kind , rk , &
         & sh1 , tpi , Wa
      integer i , ib , ido , Ifac , ii , ip , ipm , is , j , k1 , l1 ,  &
            & l2 , n , nf , nfm1 , nl , nq , nr , ntry , ntryh
!*** End of declarations inserted by SPAG
      dimension Wa(*) , Ifac(*) , ntryh(4)
      data ntryh(1) , ntryh(2) , ntryh(3) , ntryh(4)/4 , 2 , 3 , 5/ ,   &
         & tpi/6.28318530717958647692d0/
      nl = n
      nf = 0
      j = 0
 100  j = j + 1
      if ( j<=4 ) then
         ntry = ntryh(j)
      else
         ntry = ntry + 2
      endif
 200  nq = nl/ntry
      nr = nl - ntry*nq
      if ( nr/=0 ) goto 100
      nf = nf + 1
      Ifac(nf+2) = ntry
      nl = nq
      if ( ntry==2 ) then
         if ( nf/=1 ) then
            do i = 2 , nf
               ib = nf - i + 2
               Ifac(ib+2) = Ifac(ib+1)
            enddo
            Ifac(3) = 2
         endif
      endif
      if ( nl/=1 ) goto 200
      Ifac(1) = n
      Ifac(2) = nf
      argh = tpi/real(n,rk)
      is = 0
      nfm1 = nf - 1
      l1 = 1
      if ( nfm1==0 ) return
      do k1 = 1 , nfm1
         ip = Ifac(k1+2)
         l2 = l1*ip
         ido = n/l2
         ipm = ip - 1
         arg1 = real(l1,rk)*argh
         ch1 = 1.0d0
         sh1 = 0.0d0
         dch1 = cos(arg1)
         dsh1 = sin(arg1)
         do j = 1 , ipm
            ch1h = dch1*ch1 - dsh1*sh1
            sh1 = dch1*sh1 + dsh1*ch1
            ch1 = ch1h
            i = is + 2
            Wa(i-1) = ch1
            Wa(i) = sh1
            if ( ido>=5 ) then
               do ii = 5 , ido , 2
                  i = i + 2
                  Wa(i-1) = ch1*Wa(i-3) - sh1*Wa(i-2)
                  Wa(i) = ch1*Wa(i-2) + sh1*Wa(i-3)
               enddo
            endif
            is = is + ido
         enddo
         l1 = l2
      enddo
      end
!*==PASSB.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      subroutine passb(Nac,Ido,Ip,l1,Idl1,Cc,c1,c2,Ch,Ch2,Wa)
      use fftpack_kind
      implicit none
!*--PASSB722
!*** Start of declarations inserted by SPAG
      real c1 , c2 , Cc , Ch , Ch2 , fftpack_kind , rk , Wa , wai , war
      integer i , idij , idj , idl , Idl1 , idlj , Ido , idot , idp ,   &
            & ik , inc , Ip , ipp2 , ipph , j , jc , k , l , l1 , lc
      integer Nac , nt
!*** End of declarations inserted by SPAG
      dimension Ch(Ido,l1,Ip) , Cc(Ido,Ip,l1) , c1(Ido,l1,Ip) , Wa(1) , &
              & c2(Idl1,Ip) , Ch2(Idl1,Ip)
      idot = Ido/2
      nt = Ip*Idl1
      ipp2 = Ip + 2
      ipph = (Ip+1)/2
      idp = Ip*Ido
!
      if ( Ido<l1 ) then
         do j = 2 , ipph
            jc = ipp2 - j
            do i = 1 , Ido
               do k = 1 , l1
                  Ch(i,k,j) = Cc(i,j,k) + Cc(i,jc,k)
                  Ch(i,k,jc) = Cc(i,j,k) - Cc(i,jc,k)
               enddo
            enddo
         enddo
         do i = 1 , Ido
            do k = 1 , l1
               Ch(i,k,1) = Cc(i,1,k)
            enddo
         enddo
      else
         do j = 2 , ipph
            jc = ipp2 - j
            do k = 1 , l1
               do i = 1 , Ido
                  Ch(i,k,j) = Cc(i,j,k) + Cc(i,jc,k)
                  Ch(i,k,jc) = Cc(i,j,k) - Cc(i,jc,k)
               enddo
            enddo
         enddo
         do k = 1 , l1
            do i = 1 , Ido
               Ch(i,k,1) = Cc(i,1,k)
            enddo
         enddo
      endif
      idl = 2 - Ido
      inc = 0
      do l = 2 , ipph
         lc = ipp2 - l
         idl = idl + Ido
         do ik = 1 , Idl1
            c2(ik,l) = Ch2(ik,1) + Wa(idl-1)*Ch2(ik,2)
            c2(ik,lc) = Wa(idl)*Ch2(ik,Ip)
         enddo
         idlj = idl
         inc = inc + Ido
         do j = 3 , ipph
            jc = ipp2 - j
            idlj = idlj + inc
            if ( idlj>idp ) idlj = idlj - idp
            war = Wa(idlj-1)
            wai = Wa(idlj)
            do ik = 1 , Idl1
               c2(ik,l) = c2(ik,l) + war*Ch2(ik,j)
               c2(ik,lc) = c2(ik,lc) + wai*Ch2(ik,jc)
            enddo
         enddo
      enddo
      do j = 2 , ipph
         do ik = 1 , Idl1
            Ch2(ik,1) = Ch2(ik,1) + Ch2(ik,j)
         enddo
      enddo
      do j = 2 , ipph
         jc = ipp2 - j
         do ik = 2 , Idl1 , 2
            Ch2(ik-1,j) = c2(ik-1,j) - c2(ik,jc)
            Ch2(ik-1,jc) = c2(ik-1,j) + c2(ik,jc)
            Ch2(ik,j) = c2(ik,j) + c2(ik-1,jc)
            Ch2(ik,jc) = c2(ik,j) - c2(ik-1,jc)
         enddo
      enddo
      Nac = 1
      if ( Ido==2 ) return
      Nac = 0
      do ik = 1 , Idl1
         c2(ik,1) = Ch2(ik,1)
      enddo
      do j = 2 , Ip
         do k = 1 , l1
            c1(1,k,j) = Ch(1,k,j)
            c1(2,k,j) = Ch(2,k,j)
         enddo
      enddo
      if ( idot>l1 ) then
         idj = 2 - Ido
         do j = 2 , Ip
            idj = idj + Ido
            do k = 1 , l1
               idij = idj
               do i = 4 , Ido , 2
                  idij = idij + 2
                  c1(i-1,k,j) = Wa(idij-1)*Ch(i-1,k,j) - Wa(idij)       &
                              & *Ch(i,k,j)
                  c1(i,k,j) = Wa(idij-1)*Ch(i,k,j) + Wa(idij)           &
                            & *Ch(i-1,k,j)
               enddo
            enddo
         enddo
         goto 99999
      endif
      idij = 0
      do j = 2 , Ip
         idij = idij + 2
         do i = 4 , Ido , 2
            idij = idij + 2
            do k = 1 , l1
               c1(i-1,k,j) = Wa(idij-1)*Ch(i-1,k,j) - Wa(idij)*Ch(i,k,j)
               c1(i,k,j) = Wa(idij-1)*Ch(i,k,j) + Wa(idij)*Ch(i-1,k,j)
            enddo
         enddo
      enddo
      return
99999 end
!*==PASSB2.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      subroutine passb2(Ido,l1,Cc,Ch,Wa1)
      use fftpack_kind
      implicit none
!*--PASSB2851
!*** Start of declarations inserted by SPAG
      real Cc , Ch , fftpack_kind , rk , ti2 , tr2 , Wa1
      integer i , Ido , k , l1
!*** End of declarations inserted by SPAG
      dimension Cc(Ido,2,l1) , Ch(Ido,l1,2) , Wa1(1)
      if ( Ido>2 ) then
         do k = 1 , l1
            do i = 2 , Ido , 2
               Ch(i-1,k,1) = Cc(i-1,1,k) + Cc(i-1,2,k)
               tr2 = Cc(i-1,1,k) - Cc(i-1,2,k)
               Ch(i,k,1) = Cc(i,1,k) + Cc(i,2,k)
               ti2 = Cc(i,1,k) - Cc(i,2,k)
               Ch(i,k,2) = Wa1(i-1)*ti2 + Wa1(i)*tr2
               Ch(i-1,k,2) = Wa1(i-1)*tr2 - Wa1(i)*ti2
            enddo
         enddo
         goto 99999
      endif
      do k = 1 , l1
         Ch(1,k,1) = Cc(1,1,k) + Cc(1,2,k)
         Ch(1,k,2) = Cc(1,1,k) - Cc(1,2,k)
         Ch(2,k,1) = Cc(2,1,k) + Cc(2,2,k)
         Ch(2,k,2) = Cc(2,1,k) - Cc(2,2,k)
      enddo
      return
99999 end
!*==PASSB3.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      subroutine passb3(Ido,l1,Cc,Ch,Wa1,Wa2)
      use fftpack_kind
      implicit none
!*--PASSB3882
!*** Start of declarations inserted by SPAG
      real Cc , Ch , ci2 , ci3 , cr2 , cr3 , di2 , di3 , dr2 , dr3 ,    &
         & fftpack_kind , rk , taui , taur , ti2 , tr2 , Wa1 , Wa2
      integer i , Ido , k , l1
!*** End of declarations inserted by SPAG
      dimension Cc(Ido,3,l1) , Ch(Ido,l1,3) , Wa1(1) , Wa2(1)
!     *** TAUI IS SQRT(3)/2 ***
      data taur , taui/ - 0.5d0 , 0.86602540378443864676d0/
      if ( Ido/=2 ) then
         do k = 1 , l1
            do i = 2 , Ido , 2
               tr2 = Cc(i-1,2,k) + Cc(i-1,3,k)
               cr2 = Cc(i-1,1,k) + taur*tr2
               Ch(i-1,k,1) = Cc(i-1,1,k) + tr2
               ti2 = Cc(i,2,k) + Cc(i,3,k)
               ci2 = Cc(i,1,k) + taur*ti2
               Ch(i,k,1) = Cc(i,1,k) + ti2
               cr3 = taui*(Cc(i-1,2,k)-Cc(i-1,3,k))
               ci3 = taui*(Cc(i,2,k)-Cc(i,3,k))
               dr2 = cr2 - ci3
               dr3 = cr2 + ci3
               di2 = ci2 + cr3
               di3 = ci2 - cr3
               Ch(i,k,2) = Wa1(i-1)*di2 + Wa1(i)*dr2
               Ch(i-1,k,2) = Wa1(i-1)*dr2 - Wa1(i)*di2
               Ch(i,k,3) = Wa2(i-1)*di3 + Wa2(i)*dr3
               Ch(i-1,k,3) = Wa2(i-1)*dr3 - Wa2(i)*di3
            enddo
         enddo
         goto 99999
      endif
      do k = 1 , l1
         tr2 = Cc(1,2,k) + Cc(1,3,k)
         cr2 = Cc(1,1,k) + taur*tr2
         Ch(1,k,1) = Cc(1,1,k) + tr2
         ti2 = Cc(2,2,k) + Cc(2,3,k)
         ci2 = Cc(2,1,k) + taur*ti2
         Ch(2,k,1) = Cc(2,1,k) + ti2
         cr3 = taui*(Cc(1,2,k)-Cc(1,3,k))
         ci3 = taui*(Cc(2,2,k)-Cc(2,3,k))
         Ch(1,k,2) = cr2 - ci3
         Ch(1,k,3) = cr2 + ci3
         Ch(2,k,2) = ci2 + cr3
         Ch(2,k,3) = ci2 - cr3
      enddo
      return
99999 end
!*==PASSB4.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      subroutine passb4(Ido,l1,Cc,Ch,Wa1,Wa2,Wa3)
      use fftpack_kind
      implicit none
!*--PASSB4934
!*** Start of declarations inserted by SPAG
      real Cc , Ch , ci2 , ci3 , ci4 , cr2 , cr3 , cr4 , fftpack_kind , &
         & rk , ti1 , ti2 , ti3 , ti4 , tr1 , tr2 , tr3 , tr4 , Wa1 ,   &
         & Wa2
      real Wa3
      integer i , Ido , k , l1
!*** End of declarations inserted by SPAG
      dimension Cc(Ido,4,l1) , Ch(Ido,l1,4) , Wa1(1) , Wa2(1) , Wa3(1)
      if ( Ido/=2 ) then
         do k = 1 , l1
            do i = 2 , Ido , 2
               ti1 = Cc(i,1,k) - Cc(i,3,k)
               ti2 = Cc(i,1,k) + Cc(i,3,k)
               ti3 = Cc(i,2,k) + Cc(i,4,k)
               tr4 = Cc(i,4,k) - Cc(i,2,k)
               tr1 = Cc(i-1,1,k) - Cc(i-1,3,k)
               tr2 = Cc(i-1,1,k) + Cc(i-1,3,k)
               ti4 = Cc(i-1,2,k) - Cc(i-1,4,k)
               tr3 = Cc(i-1,2,k) + Cc(i-1,4,k)
               Ch(i-1,k,1) = tr2 + tr3
               cr3 = tr2 - tr3
               Ch(i,k,1) = ti2 + ti3
               ci3 = ti2 - ti3
               cr2 = tr1 + tr4
               cr4 = tr1 - tr4
               ci2 = ti1 + ti4
               ci4 = ti1 - ti4
               Ch(i-1,k,2) = Wa1(i-1)*cr2 - Wa1(i)*ci2
               Ch(i,k,2) = Wa1(i-1)*ci2 + Wa1(i)*cr2
               Ch(i-1,k,3) = Wa2(i-1)*cr3 - Wa2(i)*ci3
               Ch(i,k,3) = Wa2(i-1)*ci3 + Wa2(i)*cr3
               Ch(i-1,k,4) = Wa3(i-1)*cr4 - Wa3(i)*ci4
               Ch(i,k,4) = Wa3(i-1)*ci4 + Wa3(i)*cr4
            enddo
         enddo
         goto 99999
      endif
      do k = 1 , l1
         ti1 = Cc(2,1,k) - Cc(2,3,k)
         ti2 = Cc(2,1,k) + Cc(2,3,k)
         tr4 = Cc(2,4,k) - Cc(2,2,k)
         ti3 = Cc(2,2,k) + Cc(2,4,k)
         tr1 = Cc(1,1,k) - Cc(1,3,k)
         tr2 = Cc(1,1,k) + Cc(1,3,k)
         ti4 = Cc(1,2,k) - Cc(1,4,k)
         tr3 = Cc(1,2,k) + Cc(1,4,k)
         Ch(1,k,1) = tr2 + tr3
         Ch(1,k,3) = tr2 - tr3
         Ch(2,k,1) = ti2 + ti3
         Ch(2,k,3) = ti2 - ti3
         Ch(1,k,2) = tr1 + tr4
         Ch(1,k,4) = tr1 - tr4
         Ch(2,k,2) = ti1 + ti4
         Ch(2,k,4) = ti1 - ti4
      enddo
      return
99999 end
!*==PASSB5.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      subroutine passb5(Ido,l1,Cc,Ch,Wa1,Wa2,Wa3,Wa4)
      use fftpack_kind
      implicit none
!*--PASSB5996
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
!     *** TR11=COS(2*PI/5), TI11=SIN(2*PI/5)
!     *** TR12=COS(4*PI/5), TI12=SIN(4*PI/5)
      data tr11 , ti11 , tr12 , ti12/0.3090169943749474241d0 ,          &
         & 0.95105651629515357212d0 , -0.8090169943749474241d0 ,        &
         & 0.58778525229247312917d0/
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
               Ch(i-1,k,2) = Wa1(i-1)*dr2 - Wa1(i)*di2
               Ch(i,k,2) = Wa1(i-1)*di2 + Wa1(i)*dr2
               Ch(i-1,k,3) = Wa2(i-1)*dr3 - Wa2(i)*di3
               Ch(i,k,3) = Wa2(i-1)*di3 + Wa2(i)*dr3
               Ch(i-1,k,4) = Wa3(i-1)*dr4 - Wa3(i)*di4
               Ch(i,k,4) = Wa3(i-1)*di4 + Wa3(i)*dr4
               Ch(i-1,k,5) = Wa4(i-1)*dr5 - Wa4(i)*di5
               Ch(i,k,5) = Wa4(i-1)*di5 + Wa4(i)*dr5
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
99999 end
!*==PASSF.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      subroutine passf(Nac,Ido,Ip,l1,Idl1,Cc,c1,c2,Ch,Ch2,Wa)
      use fftpack_kind
      implicit none
!*--PASSF1087
!*** Start of declarations inserted by SPAG
      real c1 , c2 , Cc , Ch , Ch2 , fftpack_kind , rk , Wa , wai , war
      integer i , idij , idj , idl , Idl1 , idlj , Ido , idot , idp ,   &
            & ik , inc , Ip , ipp2 , ipph , j , jc , k , l , l1 , lc
      integer Nac , nt
!*** End of declarations inserted by SPAG
      dimension Ch(Ido,l1,Ip) , Cc(Ido,Ip,l1) , c1(Ido,l1,Ip) , Wa(1) , &
              & c2(Idl1,Ip) , Ch2(Idl1,Ip)
      idot = Ido/2
      nt = Ip*Idl1
      ipp2 = Ip + 2
      ipph = (Ip+1)/2
      idp = Ip*Ido
!
      if ( Ido<l1 ) then
         do j = 2 , ipph
            jc = ipp2 - j
            do i = 1 , Ido
               do k = 1 , l1
                  Ch(i,k,j) = Cc(i,j,k) + Cc(i,jc,k)
                  Ch(i,k,jc) = Cc(i,j,k) - Cc(i,jc,k)
               enddo
            enddo
         enddo
         do i = 1 , Ido
            do k = 1 , l1
               Ch(i,k,1) = Cc(i,1,k)
            enddo
         enddo
      else
         do j = 2 , ipph
            jc = ipp2 - j
            do k = 1 , l1
               do i = 1 , Ido
                  Ch(i,k,j) = Cc(i,j,k) + Cc(i,jc,k)
                  Ch(i,k,jc) = Cc(i,j,k) - Cc(i,jc,k)
               enddo
            enddo
         enddo
         do k = 1 , l1
            do i = 1 , Ido
               Ch(i,k,1) = Cc(i,1,k)
            enddo
         enddo
      endif
      idl = 2 - Ido
      inc = 0
      do l = 2 , ipph
         lc = ipp2 - l
         idl = idl + Ido
         do ik = 1 , Idl1
            c2(ik,l) = Ch2(ik,1) + Wa(idl-1)*Ch2(ik,2)
            c2(ik,lc) = -Wa(idl)*Ch2(ik,Ip)
         enddo
         idlj = idl
         inc = inc + Ido
         do j = 3 , ipph
            jc = ipp2 - j
            idlj = idlj + inc
            if ( idlj>idp ) idlj = idlj - idp
            war = Wa(idlj-1)
            wai = Wa(idlj)
            do ik = 1 , Idl1
               c2(ik,l) = c2(ik,l) + war*Ch2(ik,j)
               c2(ik,lc) = c2(ik,lc) - wai*Ch2(ik,jc)
            enddo
         enddo
      enddo
      do j = 2 , ipph
         do ik = 1 , Idl1
            Ch2(ik,1) = Ch2(ik,1) + Ch2(ik,j)
         enddo
      enddo
      do j = 2 , ipph
         jc = ipp2 - j
         do ik = 2 , Idl1 , 2
            Ch2(ik-1,j) = c2(ik-1,j) - c2(ik,jc)
            Ch2(ik-1,jc) = c2(ik-1,j) + c2(ik,jc)
            Ch2(ik,j) = c2(ik,j) + c2(ik-1,jc)
            Ch2(ik,jc) = c2(ik,j) - c2(ik-1,jc)
         enddo
      enddo
      Nac = 1
      if ( Ido==2 ) return
      Nac = 0
      do ik = 1 , Idl1
         c2(ik,1) = Ch2(ik,1)
      enddo
      do j = 2 , Ip
         do k = 1 , l1
            c1(1,k,j) = Ch(1,k,j)
            c1(2,k,j) = Ch(2,k,j)
         enddo
      enddo
      if ( idot>l1 ) then
         idj = 2 - Ido
         do j = 2 , Ip
            idj = idj + Ido
            do k = 1 , l1
               idij = idj
               do i = 4 , Ido , 2
                  idij = idij + 2
                  c1(i-1,k,j) = Wa(idij-1)*Ch(i-1,k,j) + Wa(idij)       &
                              & *Ch(i,k,j)
                  c1(i,k,j) = Wa(idij-1)*Ch(i,k,j) - Wa(idij)           &
                            & *Ch(i-1,k,j)
               enddo
            enddo
         enddo
         goto 99999
      endif
      idij = 0
      do j = 2 , Ip
         idij = idij + 2
         do i = 4 , Ido , 2
            idij = idij + 2
            do k = 1 , l1
               c1(i-1,k,j) = Wa(idij-1)*Ch(i-1,k,j) + Wa(idij)*Ch(i,k,j)
               c1(i,k,j) = Wa(idij-1)*Ch(i,k,j) - Wa(idij)*Ch(i-1,k,j)
            enddo
         enddo
      enddo
      return
99999 end
!*==PASSF2.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      subroutine passf2(Ido,l1,Cc,Ch,Wa1)
      use fftpack_kind
      implicit none
!*--PASSF21216
!*** Start of declarations inserted by SPAG
      real Cc , Ch , fftpack_kind , rk , ti2 , tr2 , Wa1
      integer i , Ido , k , l1
!*** End of declarations inserted by SPAG
      dimension Cc(Ido,2,l1) , Ch(Ido,l1,2) , Wa1(1)
      if ( Ido>2 ) then
         do k = 1 , l1
            do i = 2 , Ido , 2
               Ch(i-1,k,1) = Cc(i-1,1,k) + Cc(i-1,2,k)
               tr2 = Cc(i-1,1,k) - Cc(i-1,2,k)
               Ch(i,k,1) = Cc(i,1,k) + Cc(i,2,k)
               ti2 = Cc(i,1,k) - Cc(i,2,k)
               Ch(i,k,2) = Wa1(i-1)*ti2 - Wa1(i)*tr2
               Ch(i-1,k,2) = Wa1(i-1)*tr2 + Wa1(i)*ti2
            enddo
         enddo
         goto 99999
      endif
      do k = 1 , l1
         Ch(1,k,1) = Cc(1,1,k) + Cc(1,2,k)
         Ch(1,k,2) = Cc(1,1,k) - Cc(1,2,k)
         Ch(2,k,1) = Cc(2,1,k) + Cc(2,2,k)
         Ch(2,k,2) = Cc(2,1,k) - Cc(2,2,k)
      enddo
      return
99999 end
!*==PASSF3.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      subroutine passf3(Ido,l1,Cc,Ch,Wa1,Wa2)
      use fftpack_kind
      implicit none
!*--PASSF31247
!*** Start of declarations inserted by SPAG
      real Cc , Ch , ci2 , ci3 , cr2 , cr3 , di2 , di3 , dr2 , dr3 ,    &
         & fftpack_kind , rk , taui , taur , ti2 , tr2 , Wa1 , Wa2
      integer i , Ido , k , l1
!*** End of declarations inserted by SPAG
      dimension Cc(Ido,3,l1) , Ch(Ido,l1,3) , Wa1(1) , Wa2(1)
!     *** TAUI IS -SQRT(3)/2 ***
      data taur , taui/ - 0.5d0 , -0.86602540378443864676d0/
      if ( Ido/=2 ) then
         do k = 1 , l1
            do i = 2 , Ido , 2
               tr2 = Cc(i-1,2,k) + Cc(i-1,3,k)
               cr2 = Cc(i-1,1,k) + taur*tr2
               Ch(i-1,k,1) = Cc(i-1,1,k) + tr2
               ti2 = Cc(i,2,k) + Cc(i,3,k)
               ci2 = Cc(i,1,k) + taur*ti2
               Ch(i,k,1) = Cc(i,1,k) + ti2
               cr3 = taui*(Cc(i-1,2,k)-Cc(i-1,3,k))
               ci3 = taui*(Cc(i,2,k)-Cc(i,3,k))
               dr2 = cr2 - ci3
               dr3 = cr2 + ci3
               di2 = ci2 + cr3
               di3 = ci2 - cr3
               Ch(i,k,2) = Wa1(i-1)*di2 - Wa1(i)*dr2
               Ch(i-1,k,2) = Wa1(i-1)*dr2 + Wa1(i)*di2
               Ch(i,k,3) = Wa2(i-1)*di3 - Wa2(i)*dr3
               Ch(i-1,k,3) = Wa2(i-1)*dr3 + Wa2(i)*di3
            enddo
         enddo
         goto 99999
      endif
      do k = 1 , l1
         tr2 = Cc(1,2,k) + Cc(1,3,k)
         cr2 = Cc(1,1,k) + taur*tr2
         Ch(1,k,1) = Cc(1,1,k) + tr2
         ti2 = Cc(2,2,k) + Cc(2,3,k)
         ci2 = Cc(2,1,k) + taur*ti2
         Ch(2,k,1) = Cc(2,1,k) + ti2
         cr3 = taui*(Cc(1,2,k)-Cc(1,3,k))
         ci3 = taui*(Cc(2,2,k)-Cc(2,3,k))
         Ch(1,k,2) = cr2 - ci3
         Ch(1,k,3) = cr2 + ci3
         Ch(2,k,2) = ci2 + cr3
         Ch(2,k,3) = ci2 - cr3
      enddo
      return
99999 end
!*==PASSF4.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      subroutine passf4(Ido,l1,Cc,Ch,Wa1,Wa2,Wa3)
      use fftpack_kind
      implicit none
!*--PASSF41299
!*** Start of declarations inserted by SPAG
      real Cc , Ch , ci2 , ci3 , ci4 , cr2 , cr3 , cr4 , fftpack_kind , &
         & rk , ti1 , ti2 , ti3 , ti4 , tr1 , tr2 , tr3 , tr4 , Wa1 ,   &
         & Wa2
      real Wa3
      integer i , Ido , k , l1
!*** End of declarations inserted by SPAG
      dimension Cc(Ido,4,l1) , Ch(Ido,l1,4) , Wa1(1) , Wa2(1) , Wa3(1)
      if ( Ido/=2 ) then
         do k = 1 , l1
            do i = 2 , Ido , 2
               ti1 = Cc(i,1,k) - Cc(i,3,k)
               ti2 = Cc(i,1,k) + Cc(i,3,k)
               ti3 = Cc(i,2,k) + Cc(i,4,k)
               tr4 = Cc(i,2,k) - Cc(i,4,k)
               tr1 = Cc(i-1,1,k) - Cc(i-1,3,k)
               tr2 = Cc(i-1,1,k) + Cc(i-1,3,k)
               ti4 = Cc(i-1,4,k) - Cc(i-1,2,k)
               tr3 = Cc(i-1,2,k) + Cc(i-1,4,k)
               Ch(i-1,k,1) = tr2 + tr3
               cr3 = tr2 - tr3
               Ch(i,k,1) = ti2 + ti3
               ci3 = ti2 - ti3
               cr2 = tr1 + tr4
               cr4 = tr1 - tr4
               ci2 = ti1 + ti4
               ci4 = ti1 - ti4
               Ch(i-1,k,2) = Wa1(i-1)*cr2 + Wa1(i)*ci2
               Ch(i,k,2) = Wa1(i-1)*ci2 - Wa1(i)*cr2
               Ch(i-1,k,3) = Wa2(i-1)*cr3 + Wa2(i)*ci3
               Ch(i,k,3) = Wa2(i-1)*ci3 - Wa2(i)*cr3
               Ch(i-1,k,4) = Wa3(i-1)*cr4 + Wa3(i)*ci4
               Ch(i,k,4) = Wa3(i-1)*ci4 - Wa3(i)*cr4
            enddo
         enddo
         goto 99999
      endif
      do k = 1 , l1
         ti1 = Cc(2,1,k) - Cc(2,3,k)
         ti2 = Cc(2,1,k) + Cc(2,3,k)
         tr4 = Cc(2,2,k) - Cc(2,4,k)
         ti3 = Cc(2,2,k) + Cc(2,4,k)
         tr1 = Cc(1,1,k) - Cc(1,3,k)
         tr2 = Cc(1,1,k) + Cc(1,3,k)
         ti4 = Cc(1,4,k) - Cc(1,2,k)
         tr3 = Cc(1,2,k) + Cc(1,4,k)
         Ch(1,k,1) = tr2 + tr3
         Ch(1,k,3) = tr2 - tr3
         Ch(2,k,1) = ti2 + ti3
         Ch(2,k,3) = ti2 - ti3
         Ch(1,k,2) = tr1 + tr4
         Ch(1,k,4) = tr1 - tr4
         Ch(2,k,2) = ti1 + ti4
         Ch(2,k,4) = ti1 - ti4
      enddo
      return
99999 end
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
99999 end
!*==RADB2.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      subroutine radb2(Ido,l1,Cc,Ch,Wa1)
      use fftpack_kind
      implicit none
!*--RADB21452
!*** Start of declarations inserted by SPAG
      real Cc , Ch , fftpack_kind , rk , ti2 , tr2 , Wa1
      integer i , ic , Ido , idp2 , k , l1
!*** End of declarations inserted by SPAG
      dimension Cc(Ido,2,l1) , Ch(Ido,l1,2) , Wa1(1)
      do k = 1 , l1
         Ch(1,k,1) = Cc(1,1,k) + Cc(Ido,2,k)
         Ch(1,k,2) = Cc(1,1,k) - Cc(Ido,2,k)
      enddo
      if ( Ido<2 ) goto 99999
      if ( Ido/=2 ) then
         idp2 = Ido + 2
         do k = 1 , l1
            do i = 3 , Ido , 2
               ic = idp2 - i
               Ch(i-1,k,1) = Cc(i-1,1,k) + Cc(ic-1,2,k)
               tr2 = Cc(i-1,1,k) - Cc(ic-1,2,k)
               Ch(i,k,1) = Cc(i,1,k) - Cc(ic,2,k)
               ti2 = Cc(i,1,k) + Cc(ic,2,k)
               Ch(i-1,k,2) = Wa1(i-2)*tr2 - Wa1(i-1)*ti2
               Ch(i,k,2) = Wa1(i-2)*ti2 + Wa1(i-1)*tr2
            enddo
         enddo
         if ( mod(Ido,2)==1 ) return
      endif
      do k = 1 , l1
         Ch(Ido,k,1) = Cc(Ido,1,k) + Cc(Ido,1,k)
         Ch(Ido,k,2) = -(Cc(1,2,k)+Cc(1,2,k))
      enddo
99999 end
!*==RADB3.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      subroutine radb3(Ido,l1,Cc,Ch,Wa1,Wa2)
      use fftpack_kind
      implicit none
!*--RADB31487
!*** Start of declarations inserted by SPAG
      real Cc , Ch , ci2 , ci3 , cr2 , cr3 , di2 , di3 , dr2 , dr3 ,    &
         & fftpack_kind , rk , taui , taur , ti2 , tr2 , Wa1 , Wa2
      integer i , ic , Ido , idp2 , k , l1
!*** End of declarations inserted by SPAG
      dimension Cc(Ido,3,l1) , Ch(Ido,l1,3) , Wa1(1) , Wa2(1)
!     *** TAUI IS SQRT(3)/2 ***
      data taur , taui/ - 0.5d0 , 0.86602540378443864676d0/
      do k = 1 , l1
         tr2 = Cc(Ido,2,k) + Cc(Ido,2,k)
         cr2 = Cc(1,1,k) + taur*tr2
         Ch(1,k,1) = Cc(1,1,k) + tr2
         ci3 = taui*(Cc(1,3,k)+Cc(1,3,k))
         Ch(1,k,2) = cr2 - ci3
         Ch(1,k,3) = cr2 + ci3
      enddo
      if ( Ido==1 ) return
      idp2 = Ido + 2
      do k = 1 , l1
         do i = 3 , Ido , 2
            ic = idp2 - i
            tr2 = Cc(i-1,3,k) + Cc(ic-1,2,k)
            cr2 = Cc(i-1,1,k) + taur*tr2
            Ch(i-1,k,1) = Cc(i-1,1,k) + tr2
            ti2 = Cc(i,3,k) - Cc(ic,2,k)
            ci2 = Cc(i,1,k) + taur*ti2
            Ch(i,k,1) = Cc(i,1,k) + ti2
            cr3 = taui*(Cc(i-1,3,k)-Cc(ic-1,2,k))
            ci3 = taui*(Cc(i,3,k)+Cc(ic,2,k))
            dr2 = cr2 - ci3
            dr3 = cr2 + ci3
            di2 = ci2 + cr3
            di3 = ci2 - cr3
            Ch(i-1,k,2) = Wa1(i-2)*dr2 - Wa1(i-1)*di2
            Ch(i,k,2) = Wa1(i-2)*di2 + Wa1(i-1)*dr2
            Ch(i-1,k,3) = Wa2(i-2)*dr3 - Wa2(i-1)*di3
            Ch(i,k,3) = Wa2(i-2)*di3 + Wa2(i-1)*dr3
         enddo
      enddo
      end
!*==RADB4.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      subroutine radb4(Ido,l1,Cc,Ch,Wa1,Wa2,Wa3)
      use fftpack_kind
      implicit none
!*--RADB41532
!*** Start of declarations inserted by SPAG
      real Cc , Ch , ci2 , ci3 , ci4 , cr2 , cr3 , cr4 , fftpack_kind , &
         & rk , sqrt2 , ti1 , ti2 , ti3 , ti4 , tr1 , tr2 , tr3 , tr4 , &
         & Wa1
      real Wa2 , Wa3
      integer i , ic , Ido , idp2 , k , l1
!*** End of declarations inserted by SPAG
      dimension Cc(Ido,4,l1) , Ch(Ido,l1,4) , Wa1(1) , Wa2(1) , Wa3(1)
      data sqrt2/1.41421356237309504880d0/
      do k = 1 , l1
         tr1 = Cc(1,1,k) - Cc(Ido,4,k)
         tr2 = Cc(1,1,k) + Cc(Ido,4,k)
         tr3 = Cc(Ido,2,k) + Cc(Ido,2,k)
         tr4 = Cc(1,3,k) + Cc(1,3,k)
         Ch(1,k,1) = tr2 + tr3
         Ch(1,k,2) = tr1 - tr4
         Ch(1,k,3) = tr2 - tr3
         Ch(1,k,4) = tr1 + tr4
      enddo
      if ( Ido<2 ) goto 99999
      if ( Ido/=2 ) then
         idp2 = Ido + 2
         do k = 1 , l1
            do i = 3 , Ido , 2
               ic = idp2 - i
               ti1 = Cc(i,1,k) + Cc(ic,4,k)
               ti2 = Cc(i,1,k) - Cc(ic,4,k)
               ti3 = Cc(i,3,k) - Cc(ic,2,k)
               tr4 = Cc(i,3,k) + Cc(ic,2,k)
               tr1 = Cc(i-1,1,k) - Cc(ic-1,4,k)
               tr2 = Cc(i-1,1,k) + Cc(ic-1,4,k)
               ti4 = Cc(i-1,3,k) - Cc(ic-1,2,k)
               tr3 = Cc(i-1,3,k) + Cc(ic-1,2,k)
               Ch(i-1,k,1) = tr2 + tr3
               cr3 = tr2 - tr3
               Ch(i,k,1) = ti2 + ti3
               ci3 = ti2 - ti3
               cr2 = tr1 - tr4
               cr4 = tr1 + tr4
               ci2 = ti1 + ti4
               ci4 = ti1 - ti4
               Ch(i-1,k,2) = Wa1(i-2)*cr2 - Wa1(i-1)*ci2
               Ch(i,k,2) = Wa1(i-2)*ci2 + Wa1(i-1)*cr2
               Ch(i-1,k,3) = Wa2(i-2)*cr3 - Wa2(i-1)*ci3
               Ch(i,k,3) = Wa2(i-2)*ci3 + Wa2(i-1)*cr3
               Ch(i-1,k,4) = Wa3(i-2)*cr4 - Wa3(i-1)*ci4
               Ch(i,k,4) = Wa3(i-2)*ci4 + Wa3(i-1)*cr4
            enddo
         enddo
         if ( mod(Ido,2)==1 ) return
      endif
      do k = 1 , l1
         ti1 = Cc(1,2,k) + Cc(1,4,k)
         ti2 = Cc(1,4,k) - Cc(1,2,k)
         tr1 = Cc(Ido,1,k) - Cc(Ido,3,k)
         tr2 = Cc(Ido,1,k) + Cc(Ido,3,k)
         Ch(Ido,k,1) = tr2 + tr2
         Ch(Ido,k,2) = sqrt2*(tr1-ti1)
         Ch(Ido,k,3) = ti2 + ti2
         Ch(Ido,k,4) = -sqrt2*(tr1+ti1)
      enddo
99999 end
!*==RADB5.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      subroutine radb5(Ido,l1,Cc,Ch,Wa1,Wa2,Wa3,Wa4)
      use fftpack_kind
      implicit none
!*--RADB51599
!*** Start of declarations inserted by SPAG
      real Cc , Ch , ci2 , ci3 , ci4 , ci5 , cr2 , cr3 , cr4 , cr5 ,    &
         & di2 , di3 , di4 , di5 , dr2 , dr3 , dr4 , dr5 ,              &
         & fftpack_kind , rk
      real ti11 , ti12 , ti2 , ti3 , ti4 , ti5 , tr11 , tr12 , tr2 ,    &
         & tr3 , tr4 , tr5 , Wa1 , Wa2 , Wa3 , Wa4
      integer i , ic , Ido , idp2 , k , l1
!*** End of declarations inserted by SPAG
      dimension Cc(Ido,5,l1) , Ch(Ido,l1,5) , Wa1(1) , Wa2(1) , Wa3(1) ,&
              & Wa4(1)
!     *** TR11=COS(2*PI/5), TI11=SIN(2*PI/5)
!     *** TR12=COS(4*PI/5), TI12=SIN(4*PI/5)
      data tr11 , ti11 , tr12 , ti12/0.3090169943749474241d0 ,          &
         & 0.95105651629515357212d0 , -0.8090169943749474241d0 ,        &
         & 0.58778525229247312917d0/
      do k = 1 , l1
         ti5 = Cc(1,3,k) + Cc(1,3,k)
         ti4 = Cc(1,5,k) + Cc(1,5,k)
         tr2 = Cc(Ido,2,k) + Cc(Ido,2,k)
         tr3 = Cc(Ido,4,k) + Cc(Ido,4,k)
         Ch(1,k,1) = Cc(1,1,k) + tr2 + tr3
         cr2 = Cc(1,1,k) + tr11*tr2 + tr12*tr3
         cr3 = Cc(1,1,k) + tr12*tr2 + tr11*tr3
         ci5 = ti11*ti5 + ti12*ti4
         ci4 = ti12*ti5 - ti11*ti4
         Ch(1,k,2) = cr2 - ci5
         Ch(1,k,3) = cr3 - ci4
         Ch(1,k,4) = cr3 + ci4
         Ch(1,k,5) = cr2 + ci5
      enddo
      if ( Ido==1 ) return
      idp2 = Ido + 2
      do k = 1 , l1
         do i = 3 , Ido , 2
            ic = idp2 - i
            ti5 = Cc(i,3,k) + Cc(ic,2,k)
            ti2 = Cc(i,3,k) - Cc(ic,2,k)
            ti4 = Cc(i,5,k) + Cc(ic,4,k)
            ti3 = Cc(i,5,k) - Cc(ic,4,k)
            tr5 = Cc(i-1,3,k) - Cc(ic-1,2,k)
            tr2 = Cc(i-1,3,k) + Cc(ic-1,2,k)
            tr4 = Cc(i-1,5,k) - Cc(ic-1,4,k)
            tr3 = Cc(i-1,5,k) + Cc(ic-1,4,k)
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
            Ch(i-1,k,2) = Wa1(i-2)*dr2 - Wa1(i-1)*di2
            Ch(i,k,2) = Wa1(i-2)*di2 + Wa1(i-1)*dr2
            Ch(i-1,k,3) = Wa2(i-2)*dr3 - Wa2(i-1)*di3
            Ch(i,k,3) = Wa2(i-2)*di3 + Wa2(i-1)*dr3
            Ch(i-1,k,4) = Wa3(i-2)*dr4 - Wa3(i-1)*di4
            Ch(i,k,4) = Wa3(i-2)*di4 + Wa3(i-1)*dr4
            Ch(i-1,k,5) = Wa4(i-2)*dr5 - Wa4(i-1)*di5
            Ch(i,k,5) = Wa4(i-2)*di5 + Wa4(i-1)*dr5
         enddo
      enddo
      end
!*==RADBG.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      subroutine radbg(Ido,Ip,l1,Idl1,Cc,c1,c2,Ch,Ch2,Wa)
      use fftpack_kind
      implicit none
!*--RADBG1676
!*** Start of declarations inserted by SPAG
      real ai1 , ai2 , ar1 , ar1h , ar2 , ar2h , arg , c1 , c2 , Cc ,   &
         & Ch , Ch2 , dc2 , dcp , ds2 , dsp , fftpack_kind , rk , tpi , &
         & Wa
      integer i , ic , idij , Idl1 , Ido , idp2 , ik , Ip , ipp2 ,      &
            & ipph , is , j , j2 , jc , k , l , l1 , lc , nbd
!*** End of declarations inserted by SPAG
      dimension Ch(Ido,l1,Ip) , Cc(Ido,Ip,l1) , c1(Ido,l1,Ip) ,         &
              & c2(Idl1,Ip) , Ch2(Idl1,Ip) , Wa(1)
      data tpi/6.28318530717958647692d0/
      arg = tpi/real(Ip,rk)
      dcp = cos(arg)
      dsp = sin(arg)
      idp2 = Ido + 2
      nbd = (Ido-1)/2
      ipp2 = Ip + 2
      ipph = (Ip+1)/2
      if ( Ido<l1 ) then
         do i = 1 , Ido
            do k = 1 , l1
               Ch(i,k,1) = Cc(i,1,k)
            enddo
         enddo
      else
         do k = 1 , l1
            do i = 1 , Ido
               Ch(i,k,1) = Cc(i,1,k)
            enddo
         enddo
      endif
      do j = 2 , ipph
         jc = ipp2 - j
         j2 = j + j
         do k = 1 , l1
            Ch(1,k,j) = Cc(Ido,j2-2,k) + Cc(Ido,j2-2,k)
            Ch(1,k,jc) = Cc(1,j2-1,k) + Cc(1,j2-1,k)
         enddo
      enddo
      if ( Ido/=1 ) then
         if ( nbd<l1 ) then
            do j = 2 , ipph
               jc = ipp2 - j
               do i = 3 , Ido , 2
                  ic = idp2 - i
                  do k = 1 , l1
                     Ch(i-1,k,j) = Cc(i-1,2*j-1,k) + Cc(ic-1,2*j-2,k)
                     Ch(i-1,k,jc) = Cc(i-1,2*j-1,k) - Cc(ic-1,2*j-2,k)
                     Ch(i,k,j) = Cc(i,2*j-1,k) - Cc(ic,2*j-2,k)
                     Ch(i,k,jc) = Cc(i,2*j-1,k) + Cc(ic,2*j-2,k)
                  enddo
               enddo
            enddo
         else
            do j = 2 , ipph
               jc = ipp2 - j
               do k = 1 , l1
                  do i = 3 , Ido , 2
                     ic = idp2 - i
                     Ch(i-1,k,j) = Cc(i-1,2*j-1,k) + Cc(ic-1,2*j-2,k)
                     Ch(i-1,k,jc) = Cc(i-1,2*j-1,k) - Cc(ic-1,2*j-2,k)
                     Ch(i,k,j) = Cc(i,2*j-1,k) - Cc(ic,2*j-2,k)
                     Ch(i,k,jc) = Cc(i,2*j-1,k) + Cc(ic,2*j-2,k)
                  enddo
               enddo
            enddo
         endif
      endif
      ar1 = 1.0d0
      ai1 = 0.0d0
      do l = 2 , ipph
         lc = ipp2 - l
         ar1h = dcp*ar1 - dsp*ai1
         ai1 = dcp*ai1 + dsp*ar1
         ar1 = ar1h
         do ik = 1 , Idl1
            c2(ik,l) = Ch2(ik,1) + ar1*Ch2(ik,2)
            c2(ik,lc) = ai1*Ch2(ik,Ip)
         enddo
         dc2 = ar1
         ds2 = ai1
         ar2 = ar1
         ai2 = ai1
         do j = 3 , ipph
            jc = ipp2 - j
            ar2h = dc2*ar2 - ds2*ai2
            ai2 = dc2*ai2 + ds2*ar2
            ar2 = ar2h
            do ik = 1 , Idl1
               c2(ik,l) = c2(ik,l) + ar2*Ch2(ik,j)
               c2(ik,lc) = c2(ik,lc) + ai2*Ch2(ik,jc)
            enddo
         enddo
      enddo
      do j = 2 , ipph
         do ik = 1 , Idl1
            Ch2(ik,1) = Ch2(ik,1) + Ch2(ik,j)
         enddo
      enddo
      do j = 2 , ipph
         jc = ipp2 - j
         do k = 1 , l1
            Ch(1,k,j) = c1(1,k,j) - c1(1,k,jc)
            Ch(1,k,jc) = c1(1,k,j) + c1(1,k,jc)
         enddo
      enddo
      if ( Ido/=1 ) then
         if ( nbd<l1 ) then
            do j = 2 , ipph
               jc = ipp2 - j
               do i = 3 , Ido , 2
                  do k = 1 , l1
                     Ch(i-1,k,j) = c1(i-1,k,j) - c1(i,k,jc)
                     Ch(i-1,k,jc) = c1(i-1,k,j) + c1(i,k,jc)
                     Ch(i,k,j) = c1(i,k,j) + c1(i-1,k,jc)
                     Ch(i,k,jc) = c1(i,k,j) - c1(i-1,k,jc)
                  enddo
               enddo
            enddo
         else
            do j = 2 , ipph
               jc = ipp2 - j
               do k = 1 , l1
                  do i = 3 , Ido , 2
                     Ch(i-1,k,j) = c1(i-1,k,j) - c1(i,k,jc)
                     Ch(i-1,k,jc) = c1(i-1,k,j) + c1(i,k,jc)
                     Ch(i,k,j) = c1(i,k,j) + c1(i-1,k,jc)
                     Ch(i,k,jc) = c1(i,k,j) - c1(i-1,k,jc)
                  enddo
               enddo
            enddo
         endif
      endif
      if ( Ido==1 ) return
      do ik = 1 , Idl1
         c2(ik,1) = Ch2(ik,1)
      enddo
      do j = 2 , Ip
         do k = 1 , l1
            c1(1,k,j) = Ch(1,k,j)
         enddo
      enddo
      if ( nbd>l1 ) then
         is = -Ido
         do j = 2 , Ip
            is = is + Ido
            do k = 1 , l1
               idij = is
               do i = 3 , Ido , 2
                  idij = idij + 2
                  c1(i-1,k,j) = Wa(idij-1)*Ch(i-1,k,j) - Wa(idij)       &
                              & *Ch(i,k,j)
                  c1(i,k,j) = Wa(idij-1)*Ch(i,k,j) + Wa(idij)           &
                            & *Ch(i-1,k,j)
               enddo
            enddo
         enddo
      else
         is = -Ido
         do j = 2 , Ip
            is = is + Ido
            idij = is
            do i = 3 , Ido , 2
               idij = idij + 2
               do k = 1 , l1
                  c1(i-1,k,j) = Wa(idij-1)*Ch(i-1,k,j) - Wa(idij)       &
                              & *Ch(i,k,j)
                  c1(i,k,j) = Wa(idij-1)*Ch(i,k,j) + Wa(idij)           &
                            & *Ch(i-1,k,j)
               enddo
            enddo
         enddo
      endif
      end
!*==RADF2.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      subroutine radf2(Ido,l1,Cc,Ch,Wa1)
      use fftpack_kind
      implicit none
!*--RADF21854
!*** Start of declarations inserted by SPAG
      real Cc , Ch , fftpack_kind , rk , ti2 , tr2 , Wa1
      integer i , ic , Ido , idp2 , k , l1
!*** End of declarations inserted by SPAG
      dimension Ch(Ido,2,l1) , Cc(Ido,l1,2) , Wa1(1)
      do k = 1 , l1
         Ch(1,1,k) = Cc(1,k,1) + Cc(1,k,2)
         Ch(Ido,2,k) = Cc(1,k,1) - Cc(1,k,2)
      enddo
      if ( Ido<2 ) goto 99999
      if ( Ido/=2 ) then
         idp2 = Ido + 2
         do k = 1 , l1
            do i = 3 , Ido , 2
               ic = idp2 - i
               tr2 = Wa1(i-2)*Cc(i-1,k,2) + Wa1(i-1)*Cc(i,k,2)
               ti2 = Wa1(i-2)*Cc(i,k,2) - Wa1(i-1)*Cc(i-1,k,2)
               Ch(i,1,k) = Cc(i,k,1) + ti2
               Ch(ic,2,k) = ti2 - Cc(i,k,1)
               Ch(i-1,1,k) = Cc(i-1,k,1) + tr2
               Ch(ic-1,2,k) = Cc(i-1,k,1) - tr2
            enddo
         enddo
         if ( mod(Ido,2)==1 ) return
      endif
      do k = 1 , l1
         Ch(1,2,k) = -Cc(Ido,k,2)
         Ch(Ido,1,k) = Cc(Ido,k,1)
      enddo
99999 end
!*==RADF3.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      subroutine radf3(Ido,l1,Cc,Ch,Wa1,Wa2)
      use fftpack_kind
      implicit none
!*--RADF31889
!*** Start of declarations inserted by SPAG
      real Cc , Ch , ci2 , cr2 , di2 , di3 , dr2 , dr3 , fftpack_kind , &
         & rk , taui , taur , ti2 , ti3 , tr2 , tr3 , Wa1 , Wa2
      integer i , ic , Ido , idp2 , k , l1
!*** End of declarations inserted by SPAG
      dimension Ch(Ido,3,l1) , Cc(Ido,l1,3) , Wa1(1) , Wa2(1)
!     *** TAUI IS -SQRT(3)/2 ***
      data taur , taui/ - 0.5d0 , 0.86602540378443864676d0/
      do k = 1 , l1
         cr2 = Cc(1,k,2) + Cc(1,k,3)
         Ch(1,1,k) = Cc(1,k,1) + cr2
         Ch(1,3,k) = taui*(Cc(1,k,3)-Cc(1,k,2))
         Ch(Ido,2,k) = Cc(1,k,1) + taur*cr2
      enddo
      if ( Ido==1 ) return
      idp2 = Ido + 2
      do k = 1 , l1
         do i = 3 , Ido , 2
            ic = idp2 - i
            dr2 = Wa1(i-2)*Cc(i-1,k,2) + Wa1(i-1)*Cc(i,k,2)
            di2 = Wa1(i-2)*Cc(i,k,2) - Wa1(i-1)*Cc(i-1,k,2)
            dr3 = Wa2(i-2)*Cc(i-1,k,3) + Wa2(i-1)*Cc(i,k,3)
            di3 = Wa2(i-2)*Cc(i,k,3) - Wa2(i-1)*Cc(i-1,k,3)
            cr2 = dr2 + dr3
            ci2 = di2 + di3
            Ch(i-1,1,k) = Cc(i-1,k,1) + cr2
            Ch(i,1,k) = Cc(i,k,1) + ci2
            tr2 = Cc(i-1,k,1) + taur*cr2
            ti2 = Cc(i,k,1) + taur*ci2
            tr3 = taui*(di2-di3)
            ti3 = taui*(dr3-dr2)
            Ch(i-1,3,k) = tr2 + tr3
            Ch(ic-1,2,k) = tr2 - tr3
            Ch(i,3,k) = ti2 + ti3
            Ch(ic,2,k) = ti3 - ti2
         enddo
      enddo
      end
!*==RADF4.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      subroutine radf4(Ido,l1,Cc,Ch,Wa1,Wa2,Wa3)
      use fftpack_kind
      implicit none
!*--RADF41932
!*** Start of declarations inserted by SPAG
      real Cc , Ch , ci2 , ci3 , ci4 , cr2 , cr3 , cr4 , fftpack_kind , &
         & hsqt2 , rk , ti1 , ti2 , ti3 , ti4 , tr1 , tr2 , tr3 , tr4 , &
         & Wa1
      real Wa2 , Wa3
      integer i , ic , Ido , idp2 , k , l1
!*** End of declarations inserted by SPAG
      dimension Cc(Ido,l1,4) , Ch(Ido,4,l1) , Wa1(1) , Wa2(1) , Wa3(1)
      data hsqt2/0.70710678118654752440d0/
      do k = 1 , l1
         tr1 = Cc(1,k,2) + Cc(1,k,4)
         tr2 = Cc(1,k,1) + Cc(1,k,3)
         Ch(1,1,k) = tr1 + tr2
         Ch(Ido,4,k) = tr2 - tr1
         Ch(Ido,2,k) = Cc(1,k,1) - Cc(1,k,3)
         Ch(1,3,k) = Cc(1,k,4) - Cc(1,k,2)
      enddo
      if ( Ido<2 ) goto 99999
      if ( Ido/=2 ) then
         idp2 = Ido + 2
         do k = 1 , l1
            do i = 3 , Ido , 2
               ic = idp2 - i
               cr2 = Wa1(i-2)*Cc(i-1,k,2) + Wa1(i-1)*Cc(i,k,2)
               ci2 = Wa1(i-2)*Cc(i,k,2) - Wa1(i-1)*Cc(i-1,k,2)
               cr3 = Wa2(i-2)*Cc(i-1,k,3) + Wa2(i-1)*Cc(i,k,3)
               ci3 = Wa2(i-2)*Cc(i,k,3) - Wa2(i-1)*Cc(i-1,k,3)
               cr4 = Wa3(i-2)*Cc(i-1,k,4) + Wa3(i-1)*Cc(i,k,4)
               ci4 = Wa3(i-2)*Cc(i,k,4) - Wa3(i-1)*Cc(i-1,k,4)
               tr1 = cr2 + cr4
               tr4 = cr4 - cr2
               ti1 = ci2 + ci4
               ti4 = ci2 - ci4
               ti2 = Cc(i,k,1) + ci3
               ti3 = Cc(i,k,1) - ci3
               tr2 = Cc(i-1,k,1) + cr3
               tr3 = Cc(i-1,k,1) - cr3
               Ch(i-1,1,k) = tr1 + tr2
               Ch(ic-1,4,k) = tr2 - tr1
               Ch(i,1,k) = ti1 + ti2
               Ch(ic,4,k) = ti1 - ti2
               Ch(i-1,3,k) = ti4 + tr3
               Ch(ic-1,2,k) = tr3 - ti4
               Ch(i,3,k) = tr4 + ti3
               Ch(ic,2,k) = tr4 - ti3
            enddo
         enddo
         if ( mod(Ido,2)==1 ) return
      endif
      do k = 1 , l1
         ti1 = -hsqt2*(Cc(Ido,k,2)+Cc(Ido,k,4))
         tr1 = hsqt2*(Cc(Ido,k,2)-Cc(Ido,k,4))
         Ch(Ido,1,k) = tr1 + Cc(Ido,k,1)
         Ch(Ido,3,k) = Cc(Ido,k,1) - tr1
         Ch(1,2,k) = ti1 - Cc(Ido,k,3)
         Ch(1,4,k) = ti1 + Cc(Ido,k,3)
      enddo
99999 end
!*==RADF5.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      subroutine radf5(Ido,l1,Cc,Ch,Wa1,Wa2,Wa3,Wa4)
      use fftpack_kind
      implicit none
!*--RADF51995
!*** Start of declarations inserted by SPAG
      real Cc , Ch , ci2 , ci3 , ci4 , ci5 , cr2 , cr3 , cr4 , cr5 ,    &
         & di2 , di3 , di4 , di5 , dr2 , dr3 , dr4 , dr5 ,              &
         & fftpack_kind , rk
      real ti11 , ti12 , ti2 , ti3 , ti4 , ti5 , tr11 , tr12 , tr2 ,    &
         & tr3 , tr4 , tr5 , Wa1 , Wa2 , Wa3 , Wa4
      integer i , ic , Ido , idp2 , k , l1
!*** End of declarations inserted by SPAG
      dimension Cc(Ido,l1,5) , Ch(Ido,5,l1) , Wa1(1) , Wa2(1) , Wa3(1) ,&
              & Wa4(1)
      data tr11 , ti11 , tr12 , ti12/0.3090169943749474241d0 ,          &
         & 0.95105651629515357212d0 , -0.8090169943749474241d0 ,        &
         & 0.58778525229247312917d0/
      do k = 1 , l1
         cr2 = Cc(1,k,5) + Cc(1,k,2)
         ci5 = Cc(1,k,5) - Cc(1,k,2)
         cr3 = Cc(1,k,4) + Cc(1,k,3)
         ci4 = Cc(1,k,4) - Cc(1,k,3)
         Ch(1,1,k) = Cc(1,k,1) + cr2 + cr3
         Ch(Ido,2,k) = Cc(1,k,1) + tr11*cr2 + tr12*cr3
         Ch(1,3,k) = ti11*ci5 + ti12*ci4
         Ch(Ido,4,k) = Cc(1,k,1) + tr12*cr2 + tr11*cr3
         Ch(1,5,k) = ti12*ci5 - ti11*ci4
      enddo
      if ( Ido==1 ) return
      idp2 = Ido + 2
      do k = 1 , l1
         do i = 3 , Ido , 2
            ic = idp2 - i
            dr2 = Wa1(i-2)*Cc(i-1,k,2) + Wa1(i-1)*Cc(i,k,2)
            di2 = Wa1(i-2)*Cc(i,k,2) - Wa1(i-1)*Cc(i-1,k,2)
            dr3 = Wa2(i-2)*Cc(i-1,k,3) + Wa2(i-1)*Cc(i,k,3)
            di3 = Wa2(i-2)*Cc(i,k,3) - Wa2(i-1)*Cc(i-1,k,3)
            dr4 = Wa3(i-2)*Cc(i-1,k,4) + Wa3(i-1)*Cc(i,k,4)
            di4 = Wa3(i-2)*Cc(i,k,4) - Wa3(i-1)*Cc(i-1,k,4)
            dr5 = Wa4(i-2)*Cc(i-1,k,5) + Wa4(i-1)*Cc(i,k,5)
            di5 = Wa4(i-2)*Cc(i,k,5) - Wa4(i-1)*Cc(i-1,k,5)
            cr2 = dr2 + dr5
            ci5 = dr5 - dr2
            cr5 = di2 - di5
            ci2 = di2 + di5
            cr3 = dr3 + dr4
            ci4 = dr4 - dr3
            cr4 = di3 - di4
            ci3 = di3 + di4
            Ch(i-1,1,k) = Cc(i-1,k,1) + cr2 + cr3
            Ch(i,1,k) = Cc(i,k,1) + ci2 + ci3
            tr2 = Cc(i-1,k,1) + tr11*cr2 + tr12*cr3
            ti2 = Cc(i,k,1) + tr11*ci2 + tr12*ci3
            tr3 = Cc(i-1,k,1) + tr12*cr2 + tr11*cr3
            ti3 = Cc(i,k,1) + tr12*ci2 + tr11*ci3
            tr5 = ti11*cr5 + ti12*cr4
            ti5 = ti11*ci5 + ti12*ci4
            tr4 = ti12*cr5 - ti11*cr4
            ti4 = ti12*ci5 - ti11*ci4
            Ch(i-1,3,k) = tr2 + tr5
            Ch(ic-1,2,k) = tr2 - tr5
            Ch(i,3,k) = ti2 + ti5
            Ch(ic,2,k) = ti5 - ti2
            Ch(i-1,5,k) = tr3 + tr4
            Ch(ic-1,4,k) = tr3 - tr4
            Ch(i,5,k) = ti3 + ti4
            Ch(ic,4,k) = ti4 - ti3
         enddo
      enddo
      end
!*==RADFG.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      subroutine radfg(Ido,Ip,l1,Idl1,Cc,c1,c2,Ch,Ch2,Wa)
      use fftpack_kind
      implicit none
!*--RADFG2066
!*** Start of declarations inserted by SPAG
      real ai1 , ai2 , ar1 , ar1h , ar2 , ar2h , arg , c1 , c2 , Cc ,   &
         & Ch , Ch2 , dc2 , dcp , ds2 , dsp , fftpack_kind , rk , tpi , &
         & Wa
      integer i , ic , idij , Idl1 , Ido , idp2 , ik , Ip , ipp2 ,      &
            & ipph , is , j , j2 , jc , k , l , l1 , lc , nbd
!*** End of declarations inserted by SPAG
      dimension Ch(Ido,l1,Ip) , Cc(Ido,Ip,l1) , c1(Ido,l1,Ip) ,         &
              & c2(Idl1,Ip) , Ch2(Idl1,Ip) , Wa(1)
      data tpi/6.28318530717958647692d0/
      arg = tpi/real(Ip,rk)
      dcp = cos(arg)
      dsp = sin(arg)
      ipph = (Ip+1)/2
      ipp2 = Ip + 2
      idp2 = Ido + 2
      nbd = (Ido-1)/2
      if ( Ido==1 ) then
         do ik = 1 , Idl1
            c2(ik,1) = Ch2(ik,1)
         enddo
      else
         do ik = 1 , Idl1
            Ch2(ik,1) = c2(ik,1)
         enddo
         do j = 2 , Ip
            do k = 1 , l1
               Ch(1,k,j) = c1(1,k,j)
            enddo
         enddo
         if ( nbd>l1 ) then
            is = -Ido
            do j = 2 , Ip
               is = is + Ido
               do k = 1 , l1
                  idij = is
                  do i = 3 , Ido , 2
                     idij = idij + 2
                     Ch(i-1,k,j) = Wa(idij-1)*c1(i-1,k,j) + Wa(idij)    &
                                 & *c1(i,k,j)
                     Ch(i,k,j) = Wa(idij-1)*c1(i,k,j) - Wa(idij)        &
                               & *c1(i-1,k,j)
                  enddo
               enddo
            enddo
         else
            is = -Ido
            do j = 2 , Ip
               is = is + Ido
               idij = is
               do i = 3 , Ido , 2
                  idij = idij + 2
                  do k = 1 , l1
                     Ch(i-1,k,j) = Wa(idij-1)*c1(i-1,k,j) + Wa(idij)    &
                                 & *c1(i,k,j)
                     Ch(i,k,j) = Wa(idij-1)*c1(i,k,j) - Wa(idij)        &
                               & *c1(i-1,k,j)
                  enddo
               enddo
            enddo
         endif
         if ( nbd<l1 ) then
            do j = 2 , ipph
               jc = ipp2 - j
               do i = 3 , Ido , 2
                  do k = 1 , l1
                     c1(i-1,k,j) = Ch(i-1,k,j) + Ch(i-1,k,jc)
                     c1(i-1,k,jc) = Ch(i,k,j) - Ch(i,k,jc)
                     c1(i,k,j) = Ch(i,k,j) + Ch(i,k,jc)
                     c1(i,k,jc) = Ch(i-1,k,jc) - Ch(i-1,k,j)
                  enddo
               enddo
            enddo
         else
            do j = 2 , ipph
               jc = ipp2 - j
               do k = 1 , l1
                  do i = 3 , Ido , 2
                     c1(i-1,k,j) = Ch(i-1,k,j) + Ch(i-1,k,jc)
                     c1(i-1,k,jc) = Ch(i,k,j) - Ch(i,k,jc)
                     c1(i,k,j) = Ch(i,k,j) + Ch(i,k,jc)
                     c1(i,k,jc) = Ch(i-1,k,jc) - Ch(i-1,k,j)
                  enddo
               enddo
            enddo
         endif
      endif
      do j = 2 , ipph
         jc = ipp2 - j
         do k = 1 , l1
            c1(1,k,j) = Ch(1,k,j) + Ch(1,k,jc)
            c1(1,k,jc) = Ch(1,k,jc) - Ch(1,k,j)
         enddo
      enddo
!
      ar1 = 1.0d0
      ai1 = 0.0d0
      do l = 2 , ipph
         lc = ipp2 - l
         ar1h = dcp*ar1 - dsp*ai1
         ai1 = dcp*ai1 + dsp*ar1
         ar1 = ar1h
         do ik = 1 , Idl1
            Ch2(ik,l) = c2(ik,1) + ar1*c2(ik,2)
            Ch2(ik,lc) = ai1*c2(ik,Ip)
         enddo
         dc2 = ar1
         ds2 = ai1
         ar2 = ar1
         ai2 = ai1
         do j = 3 , ipph
            jc = ipp2 - j
            ar2h = dc2*ar2 - ds2*ai2
            ai2 = dc2*ai2 + ds2*ar2
            ar2 = ar2h
            do ik = 1 , Idl1
               Ch2(ik,l) = Ch2(ik,l) + ar2*c2(ik,j)
               Ch2(ik,lc) = Ch2(ik,lc) + ai2*c2(ik,jc)
            enddo
         enddo
      enddo
      do j = 2 , ipph
         do ik = 1 , Idl1
            Ch2(ik,1) = Ch2(ik,1) + c2(ik,j)
         enddo
      enddo
!
      if ( Ido<l1 ) then
         do i = 1 , Ido
            do k = 1 , l1
               Cc(i,1,k) = Ch(i,k,1)
            enddo
         enddo
      else
         do k = 1 , l1
            do i = 1 , Ido
               Cc(i,1,k) = Ch(i,k,1)
            enddo
         enddo
      endif
      do j = 2 , ipph
         jc = ipp2 - j
         j2 = j + j
         do k = 1 , l1
            Cc(Ido,j2-2,k) = Ch(1,k,j)
            Cc(1,j2-1,k) = Ch(1,k,jc)
         enddo
      enddo
      if ( Ido==1 ) return
      if ( nbd<l1 ) then
         do j = 2 , ipph
            jc = ipp2 - j
            j2 = j + j
            do i = 3 , Ido , 2
               ic = idp2 - i
               do k = 1 , l1
                  Cc(i-1,j2-1,k) = Ch(i-1,k,j) + Ch(i-1,k,jc)
                  Cc(ic-1,j2-2,k) = Ch(i-1,k,j) - Ch(i-1,k,jc)
                  Cc(i,j2-1,k) = Ch(i,k,j) + Ch(i,k,jc)
                  Cc(ic,j2-2,k) = Ch(i,k,jc) - Ch(i,k,j)
               enddo
            enddo
         enddo
         goto 99999
      endif
      do j = 2 , ipph
         jc = ipp2 - j
         j2 = j + j
         do k = 1 , l1
            do i = 3 , Ido , 2
               ic = idp2 - i
               Cc(i-1,j2-1,k) = Ch(i-1,k,j) + Ch(i-1,k,jc)
               Cc(ic-1,j2-2,k) = Ch(i-1,k,j) - Ch(i-1,k,jc)
               Cc(i,j2-1,k) = Ch(i,k,j) + Ch(i,k,jc)
               Cc(ic,j2-2,k) = Ch(i,k,jc) - Ch(i,k,j)
            enddo
         enddo
      enddo
      return
99999 end
!*==RFFTB1.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      subroutine rfftb1(n,c,Ch,Wa,Ifac)
      use fftpack_kind
      implicit none
!*--RFFTB12251
!*** Start of declarations inserted by SPAG
      real c , Ch , fftpack_kind , rk , Wa
      integer i , idl1 , ido , Ifac , ip , iw , ix2 , ix3 , ix4 , k1 ,  &
            & l1 , l2 , n , na , nf
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
         idl1 = ido*l1
         if ( ip==4 ) then
            ix2 = iw + ido
            ix3 = ix2 + ido
            if ( na/=0 ) then
               call radb4(ido,l1,Ch,c,Wa(iw),Wa(ix2),Wa(ix3))
            else
               call radb4(ido,l1,c,Ch,Wa(iw),Wa(ix2),Wa(ix3))
            endif
            na = 1 - na
         elseif ( ip==2 ) then
            if ( na/=0 ) then
               call radb2(ido,l1,Ch,c,Wa(iw))
            else
               call radb2(ido,l1,c,Ch,Wa(iw))
            endif
            na = 1 - na
         elseif ( ip==3 ) then
            ix2 = iw + ido
            if ( na/=0 ) then
               call radb3(ido,l1,Ch,c,Wa(iw),Wa(ix2))
            else
               call radb3(ido,l1,c,Ch,Wa(iw),Wa(ix2))
            endif
            na = 1 - na
         elseif ( ip/=5 ) then
            if ( na/=0 ) then
               call radbg(ido,ip,l1,idl1,Ch,Ch,Ch,c,c,Wa(iw))
            else
               call radbg(ido,ip,l1,idl1,c,c,c,Ch,Ch,Wa(iw))
            endif
            if ( ido==1 ) na = 1 - na
         else
            ix2 = iw + ido
            ix3 = ix2 + ido
            ix4 = ix3 + ido
            if ( na/=0 ) then
               call radb5(ido,l1,Ch,c,Wa(iw),Wa(ix2),Wa(ix3),Wa(ix4))
            else
               call radb5(ido,l1,c,Ch,Wa(iw),Wa(ix2),Wa(ix3),Wa(ix4))
            endif
            na = 1 - na
         endif
         l1 = l2
         iw = iw + (ip-1)*ido
      enddo
      if ( na==0 ) return
      do i = 1 , n
         c(i) = Ch(i)
      enddo
      end
!*==RFFTF1.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      subroutine rfftf1(n,c,Ch,Wa,Ifac)
      use fftpack_kind
      implicit none
!*--RFFTF12321
!*** Start of declarations inserted by SPAG
      real c , Ch , fftpack_kind , rk , Wa
      integer i , idl1 , ido , Ifac , ip , iw , ix2 , ix3 , ix4 , k1 ,  &
            & kh , l1 , l2 , n , na , nf
!*** End of declarations inserted by SPAG
      dimension Ch(*) , c(*) , Wa(*) , Ifac(*)
      nf = Ifac(2)
      na = 1
      l2 = n
      iw = n
      do k1 = 1 , nf
         kh = nf - k1
         ip = Ifac(kh+3)
         l1 = l2/ip
         ido = n/l2
         idl1 = ido*l1
         iw = iw - (ip-1)*ido
         na = 1 - na
         if ( ip==4 ) then
            ix2 = iw + ido
            ix3 = ix2 + ido
            if ( na/=0 ) then
               call radf4(ido,l1,Ch,c,Wa(iw),Wa(ix2),Wa(ix3))
            else
               call radf4(ido,l1,c,Ch,Wa(iw),Wa(ix2),Wa(ix3))
            endif
         elseif ( ip/=2 ) then
            if ( ip==3 ) then
               ix2 = iw + ido
               if ( na/=0 ) then
                  call radf3(ido,l1,Ch,c,Wa(iw),Wa(ix2))
               else
                  call radf3(ido,l1,c,Ch,Wa(iw),Wa(ix2))
               endif
            elseif ( ip/=5 ) then
               if ( ido==1 ) na = 1 - na
               if ( na/=0 ) then
                  call radfg(ido,ip,l1,idl1,Ch,Ch,Ch,c,c,Wa(iw))
                  na = 0
               else
                  call radfg(ido,ip,l1,idl1,c,c,c,Ch,Ch,Wa(iw))
                  na = 1
               endif
            else
               ix2 = iw + ido
               ix3 = ix2 + ido
               ix4 = ix3 + ido
               if ( na/=0 ) then
                  call radf5(ido,l1,Ch,c,Wa(iw),Wa(ix2),Wa(ix3),Wa(ix4))
               else
                  call radf5(ido,l1,c,Ch,Wa(iw),Wa(ix2),Wa(ix3),Wa(ix4))
               endif
            endif
         elseif ( na/=0 ) then
            call radf2(ido,l1,Ch,c,Wa(iw))
         else
            call radf2(ido,l1,c,Ch,Wa(iw))
         endif
         l2 = l1
      enddo
      if ( na==1 ) return
      do i = 1 , n
         c(i) = Ch(i)
      enddo
      end
!*==RFFTI1.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      subroutine rffti1(n,Wa,Ifac)
      use fftpack_kind
      implicit none
!*--RFFTI12391
!*** Start of declarations inserted by SPAG
      real arg , argh , argld , fftpack_kind , fi , rk , tpi , Wa
      integer i , ib , ido , Ifac , ii , ip , ipm , is , j , k1 , l1 ,  &
            & l2 , ld , n , nf , nfm1 , nl , nq , nr , ntry
      integer ntryh
!*** End of declarations inserted by SPAG
      dimension Wa(*) , Ifac(*) , ntryh(4)
      data ntryh(1) , ntryh(2) , ntryh(3) , ntryh(4)/4 , 2 , 3 , 5/
      nl = n
      nf = 0
      j = 0
 100  j = j + 1
      if ( j<=4 ) then
         ntry = ntryh(j)
      else
         ntry = ntry + 2
      endif
 200  nq = nl/ntry
      nr = nl - ntry*nq
      if ( nr/=0 ) goto 100
      nf = nf + 1
      Ifac(nf+2) = ntry
      nl = nq
      if ( ntry==2 ) then
         if ( nf/=1 ) then
            do i = 2 , nf
               ib = nf - i + 2
               Ifac(ib+2) = Ifac(ib+1)
            enddo
            Ifac(3) = 2
         endif
      endif
      if ( nl/=1 ) goto 200
      Ifac(1) = n
      Ifac(2) = nf
      tpi = 6.28318530717958647692d0
      argh = tpi/real(n,rk)
      is = 0
      nfm1 = nf - 1
      l1 = 1
      if ( nfm1==0 ) return
      do k1 = 1 , nfm1
         ip = Ifac(k1+2)
         ld = 0
         l2 = l1*ip
         ido = n/l2
         ipm = ip - 1
         do j = 1 , ipm
            ld = ld + l1
            i = is
            argld = real(ld,rk)*argh
            fi = 0.0d0
            do ii = 3 , ido , 2
               i = i + 2
               fi = fi + 1.0d0
               arg = fi*argld
               Wa(i-1) = cos(arg)
               Wa(i) = sin(arg)
            enddo
            is = is + ido
         enddo
         l1 = l2
      enddo
      end
!*==FFTPACK_KIND.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      module fftpack_kind
      implicit none
!*--FFTPACK_KIND2459
      integer , parameter :: rk = kind(1.0d0)
      end
!*==SINT1.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      subroutine sint1(n,War,Was,Xh,x,Ifac)
      use fftpack_kind
      implicit none
!*--SINT12466
!*** Start of declarations inserted by SPAG
      integer i , Ifac , k , kc , modn , n , np1 , ns2
      real sqrt3 , t1 , t2 , War , Was , x , Xh , xhold
!*** End of declarations inserted by SPAG
      dimension War(*) , Was(*) , x(*) , Xh(*) , Ifac(*)
      data sqrt3/1.73205080756887729352d0/
      do i = 1 , n
         Xh(i) = War(i)
         War(i) = x(i)
      enddo
      if ( n<2 ) then
         Xh(1) = Xh(1) + Xh(1)
      elseif ( n==2 ) then
         xhold = sqrt3*(Xh(1)+Xh(2))
         Xh(2) = sqrt3*(Xh(1)-Xh(2))
         Xh(1) = xhold
      else
         np1 = n + 1
         ns2 = n/2
         x(1) = 0.0d0
         do k = 1 , ns2
            kc = np1 - k
            t1 = Xh(k) - Xh(kc)
            t2 = Was(k)*(Xh(k)+Xh(kc))
            x(k+1) = t1 + t2
            x(kc+1) = t2 - t1
         enddo
         modn = mod(n,2)
         if ( modn/=0 ) x(ns2+2) = 4.0d0*Xh(ns2+1)
         call rfftf1(np1,x,Xh,War,Ifac)
         Xh(1) = 0.5d0*x(1)
         do i = 3 , n , 2
            Xh(i-1) = -x(i)
            Xh(i) = Xh(i-2) + x(i-1)
         enddo
         if ( modn==0 ) Xh(n) = -x(n+1)
      endif
      do i = 1 , n
         x(i) = War(i)
         War(i) = Xh(i)
      enddo
      end
!*==ZFFTB.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      subroutine zfftb(n,c,Wsave)
      use fftpack_kind
      implicit none
!*--ZFFTB2513
!*** Start of declarations inserted by SPAG
      real c , Wsave
      integer iw1 , iw2 , n
!*** End of declarations inserted by SPAG
      dimension c(1) , Wsave(1)
      if ( n==1 ) return
      iw1 = n + n + 1
      iw2 = iw1 + n + n
      call cfftb1(n,c,Wsave,Wsave(iw1),Wsave(iw2))
      end
!*==ZFFTF.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      subroutine zfftf(n,c,Wsave)
      use fftpack_kind
      implicit none
!*--ZFFTF2528
!*** Start of declarations inserted by SPAG
      real c , Wsave
      integer iw1 , iw2 , n
!*** End of declarations inserted by SPAG
      dimension c(1) , Wsave(1)
      if ( n==1 ) return
      iw1 = n + n + 1
      iw2 = iw1 + n + n
      call cfftf1(n,c,Wsave,Wsave(iw1),Wsave(iw2))
      end
!*==ZFFTI.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      subroutine zffti(n,Wsave)
      use fftpack_kind
      implicit none
!*--ZFFTI2543
!*** Start of declarations inserted by SPAG
      integer iw1 , iw2 , n
      real Wsave
!*** End of declarations inserted by SPAG
      dimension Wsave(1)
      if ( n==1 ) return
      iw1 = n + n + 1
      iw2 = iw1 + n + n
      call cffti1(n,Wsave(iw1),Wsave(iw2))
      end
