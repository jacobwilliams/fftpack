!*==CFFTB1.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      SUBROUTINE CFFTB1(N,C,Ch,Wa,Ifac)
      USE FFTPACK_KIND
      IMPLICIT NONE
!*--CFFTB15
!*** Start of declarations inserted by SPAG
      REAL C , Ch , FFTPACK_KIND , rk , Wa
      INTEGER i , idl1 , ido , idot , Ifac , ip , iw , ix2 , ix3 , ix4 ,&
            & k1 , l1 , l2 , N , n2 , na , nac , nf
!*** End of declarations inserted by SPAG
      DIMENSION Ch(*) , C(*) , Wa(*) , Ifac(*)
      nf = Ifac(2)
      na = 0
      l1 = 1
      iw = 1
      DO k1 = 1 , nf
         ip = Ifac(k1+2)
         l2 = ip*l1
         ido = N/l2
         idot = ido + ido
         idl1 = idot*l1
         IF ( ip==4 ) THEN
            ix2 = iw + idot
            ix3 = ix2 + idot
            IF ( na/=0 ) THEN
               CALL PASSB4(idot,l1,Ch,C,Wa(iw),Wa(ix2),Wa(ix3))
            ELSE
               CALL PASSB4(idot,l1,C,Ch,Wa(iw),Wa(ix2),Wa(ix3))
            ENDIF
            na = 1 - na
         ELSEIF ( ip==2 ) THEN
            IF ( na/=0 ) THEN
               CALL PASSB2(idot,l1,Ch,C,Wa(iw))
            ELSE
               CALL PASSB2(idot,l1,C,Ch,Wa(iw))
            ENDIF
            na = 1 - na
         ELSEIF ( ip==3 ) THEN
            ix2 = iw + idot
            IF ( na/=0 ) THEN
               CALL PASSB3(idot,l1,Ch,C,Wa(iw),Wa(ix2))
            ELSE
               CALL PASSB3(idot,l1,C,Ch,Wa(iw),Wa(ix2))
            ENDIF
            na = 1 - na
         ELSEIF ( ip/=5 ) THEN
            IF ( na/=0 ) THEN
               CALL PASSB(nac,idot,ip,l1,idl1,Ch,Ch,Ch,C,C,Wa(iw))
            ELSE
               CALL PASSB(nac,idot,ip,l1,idl1,C,C,C,Ch,Ch,Wa(iw))
            ENDIF
            IF ( nac/=0 ) na = 1 - na
         ELSE
            ix2 = iw + idot
            ix3 = ix2 + idot
            ix4 = ix3 + idot
            IF ( na/=0 ) THEN
               CALL PASSB5(idot,l1,Ch,C,Wa(iw),Wa(ix2),Wa(ix3),Wa(ix4))
            ELSE
               CALL PASSB5(idot,l1,C,Ch,Wa(iw),Wa(ix2),Wa(ix3),Wa(ix4))
            ENDIF
            na = 1 - na
         ENDIF
         l1 = l2
         iw = iw + (ip-1)*idot
      ENDDO
      IF ( na==0 ) RETURN
      n2 = N + N
      DO i = 1 , n2
         C(i) = Ch(i)
      ENDDO
      END
!*==CFFTF1.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      SUBROUTINE CFFTF1(N,C,Ch,Wa,Ifac)
      USE FFTPACK_KIND
      IMPLICIT NONE
!*--CFFTF177
!*** Start of declarations inserted by SPAG
      REAL C , Ch , FFTPACK_KIND , rk , Wa
      INTEGER i , idl1 , ido , idot , Ifac , ip , iw , ix2 , ix3 , ix4 ,&
            & k1 , l1 , l2 , N , n2 , na , nac , nf
!*** End of declarations inserted by SPAG
      DIMENSION Ch(*) , C(*) , Wa(*) , Ifac(*)
      nf = Ifac(2)
      na = 0
      l1 = 1
      iw = 1
      DO k1 = 1 , nf
         ip = Ifac(k1+2)
         l2 = ip*l1
         ido = N/l2
         idot = ido + ido
         idl1 = idot*l1
         IF ( ip==4 ) THEN
            ix2 = iw + idot
            ix3 = ix2 + idot
            IF ( na/=0 ) THEN
               CALL PASSF4(idot,l1,Ch,C,Wa(iw),Wa(ix2),Wa(ix3))
            ELSE
               CALL PASSF4(idot,l1,C,Ch,Wa(iw),Wa(ix2),Wa(ix3))
            ENDIF
            na = 1 - na
         ELSEIF ( ip==2 ) THEN
            IF ( na/=0 ) THEN
               CALL PASSF2(idot,l1,Ch,C,Wa(iw))
            ELSE
               CALL PASSF2(idot,l1,C,Ch,Wa(iw))
            ENDIF
            na = 1 - na
         ELSEIF ( ip==3 ) THEN
            ix2 = iw + idot
            IF ( na/=0 ) THEN
               CALL PASSF3(idot,l1,Ch,C,Wa(iw),Wa(ix2))
            ELSE
               CALL PASSF3(idot,l1,C,Ch,Wa(iw),Wa(ix2))
            ENDIF
            na = 1 - na
         ELSEIF ( ip/=5 ) THEN
            IF ( na/=0 ) THEN
               CALL PASSF(nac,idot,ip,l1,idl1,Ch,Ch,Ch,C,C,Wa(iw))
            ELSE
               CALL PASSF(nac,idot,ip,l1,idl1,C,C,C,Ch,Ch,Wa(iw))
            ENDIF
            IF ( nac/=0 ) na = 1 - na
         ELSE
            ix2 = iw + idot
            ix3 = ix2 + idot
            ix4 = ix3 + idot
            IF ( na/=0 ) THEN
               CALL PASSF5(idot,l1,Ch,C,Wa(iw),Wa(ix2),Wa(ix3),Wa(ix4))
            ELSE
               CALL PASSF5(idot,l1,C,Ch,Wa(iw),Wa(ix2),Wa(ix3),Wa(ix4))
            ENDIF
            na = 1 - na
         ENDIF
         l1 = l2
         iw = iw + (ip-1)*idot
      ENDDO
      IF ( na==0 ) RETURN
      n2 = N + N
      DO i = 1 , n2
         C(i) = Ch(i)
      ENDDO
      END
!*==CFFTI1.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      SUBROUTINE CFFTI1(N,Wa,Ifac)
      USE FFTPACK_KIND
      IMPLICIT NONE
!*--CFFTI1149
!*** Start of declarations inserted by SPAG
      REAL arg , argh , argld , FFTPACK_KIND , fi , rk , tpi , Wa
      INTEGER i , i1 , ib , ido , idot , Ifac , ii , ip , ipm , j , k1 ,&
            & l1 , l2 , ld , N , nf , nl , nq , nr , ntry
      INTEGER ntryh
!*** End of declarations inserted by SPAG
      DIMENSION Wa(*) , Ifac(*) , ntryh(4)
      DATA ntryh(1) , ntryh(2) , ntryh(3) , ntryh(4)/3 , 4 , 2 , 5/
      nl = N
      nf = 0
      j = 0
 100  j = j + 1
      IF ( j<=4 ) THEN
         ntry = ntryh(j)
      ELSE
         ntry = ntry + 2
      ENDIF
 200  nq = nl/ntry
      nr = nl - ntry*nq
      IF ( nr/=0 ) GOTO 100
      nf = nf + 1
      Ifac(nf+2) = ntry
      nl = nq
      IF ( ntry==2 ) THEN
         IF ( nf/=1 ) THEN
            DO i = 2 , nf
               ib = nf - i + 2
               Ifac(ib+2) = Ifac(ib+1)
            ENDDO
            Ifac(3) = 2
         ENDIF
      ENDIF
      IF ( nl/=1 ) GOTO 200
      Ifac(1) = N
      Ifac(2) = nf
      tpi = 6.28318530717958647692D0
      argh = tpi/REAL(N,rk)
      i = 2
      l1 = 1
      DO k1 = 1 , nf
         ip = Ifac(k1+2)
         ld = 0
         l2 = l1*ip
         ido = N/l2
         idot = ido + ido + 2
         ipm = ip - 1
         DO j = 1 , ipm
            i1 = i
            Wa(i-1) = 1.0D0
            Wa(i) = 0.0D0
            ld = ld + l1
            fi = 0.0D0
            argld = REAL(ld,rk)*argh
            DO ii = 4 , idot , 2
               i = i + 2
               fi = fi + 1.D0
               arg = fi*argld
               Wa(i-1) = COS(arg)
               Wa(i) = SIN(arg)
            ENDDO
            IF ( ip>5 ) THEN
               Wa(i1-1) = Wa(i-1)
               Wa(i1) = Wa(i)
            ENDIF
         ENDDO
         l1 = l2
      ENDDO
      END
!*==COSQB1.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      SUBROUTINE COSQB1(N,X,W,Xh)
      USE FFTPACK_KIND
      IMPLICIT NONE
!*--COSQB1222
!*** Start of declarations inserted by SPAG
      REAL FFTPACK_KIND , rk , W , X , Xh , xim1
      INTEGER i , k , kc , modn , N , np2 , ns2
!*** End of declarations inserted by SPAG
      DIMENSION X(1) , W(1) , Xh(1)
      ns2 = (N+1)/2
      np2 = N + 2
      DO i = 3 , N , 2
         xim1 = X(i-1) + X(i)
         X(i) = X(i) - X(i-1)
         X(i-1) = xim1
      ENDDO
      X(1) = X(1) + X(1)
      modn = MOD(N,2)
      IF ( modn==0 ) X(N) = X(N) + X(N)
      CALL DFFTB(N,X,Xh)
      DO k = 2 , ns2
         kc = np2 - k
         Xh(k) = W(k-1)*X(kc) + W(kc-1)*X(k)
         Xh(kc) = W(k-1)*X(k) - W(kc-1)*X(kc)
      ENDDO
      IF ( modn==0 ) X(ns2+1) = W(ns2)*(X(ns2+1)+X(ns2+1))
      DO k = 2 , ns2
         kc = np2 - k
         X(k) = Xh(k) + Xh(kc)
         X(kc) = Xh(k) - Xh(kc)
      ENDDO
      X(1) = X(1) + X(1)
      END
!*==COSQF1.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      SUBROUTINE COSQF1(N,X,W,Xh)
      USE FFTPACK_KIND
      IMPLICIT NONE
!*--COSQF1256
!*** Start of declarations inserted by SPAG
      REAL FFTPACK_KIND , rk , W , X , Xh , xim1
      INTEGER i , k , kc , modn , N , np2 , ns2
!*** End of declarations inserted by SPAG
      DIMENSION X(1) , W(1) , Xh(1)
      ns2 = (N+1)/2
      np2 = N + 2
      DO k = 2 , ns2
         kc = np2 - k
         Xh(k) = X(k) + X(kc)
         Xh(kc) = X(k) - X(kc)
      ENDDO
      modn = MOD(N,2)
      IF ( modn==0 ) Xh(ns2+1) = X(ns2+1) + X(ns2+1)
      DO k = 2 , ns2
         kc = np2 - k
         X(k) = W(k-1)*Xh(kc) + W(kc-1)*Xh(k)
         X(kc) = W(k-1)*Xh(k) - W(kc-1)*Xh(kc)
      ENDDO
      IF ( modn==0 ) X(ns2+1) = W(ns2)*Xh(ns2+1)
      CALL DFFTF(N,X,Xh)
      DO i = 3 , N , 2
         xim1 = X(i-1) - X(i)
         X(i) = X(i-1) + X(i)
         X(i-1) = xim1
      ENDDO
      END
!*==DCOSQB.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      SUBROUTINE DCOSQB(N,X,Wsave)
      USE FFTPACK_KIND
      IMPLICIT NONE
!*--DCOSQB288
!*** Start of declarations inserted by SPAG
      REAL FFTPACK_KIND , rk , tsqrt2 , Wsave , X , x1
      INTEGER N
!*** End of declarations inserted by SPAG
      DIMENSION X(*) , Wsave(*)
      DATA tsqrt2/2.82842712474619009760D0/
      IF ( N<2 ) THEN
         X(1) = 4.0D0*X(1)
         RETURN
      ELSEIF ( N==2 ) THEN
         x1 = 4.0D0*(X(1)+X(2))
         X(2) = tsqrt2*(X(1)-X(2))
         X(1) = x1
         RETURN
      ELSE
         CALL COSQB1(N,X,Wsave,Wsave(N+1))
      ENDIF
      END
!*==DCOSQF.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      SUBROUTINE DCOSQF(N,X,Wsave)
      USE FFTPACK_KIND
      IMPLICIT NONE
!*--DCOSQF311
!*** Start of declarations inserted by SPAG
      REAL FFTPACK_KIND , rk , sqrt2 , tsqx , Wsave , X
      INTEGER N
!*** End of declarations inserted by SPAG
      DIMENSION X(*) , Wsave(*)
      DATA sqrt2/1.41421356237309504880D0/
      IF ( N<2 ) THEN
      ELSEIF ( N==2 ) THEN
         tsqx = sqrt2*X(2)
         X(2) = X(1) - tsqx
         X(1) = X(1) + tsqx
      ELSE
         CALL COSQF1(N,X,Wsave,Wsave(N+1))
         GOTO 99999
      ENDIF
      RETURN
99999 END
!*==DCOSQI.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      SUBROUTINE DCOSQI(N,Wsave)
      USE FFTPACK_KIND
      IMPLICIT NONE
!*--DCOSQI333
!*** Start of declarations inserted by SPAG
      REAL dt , FFTPACK_KIND , fk , pih , rk , Wsave
      INTEGER k , N
!*** End of declarations inserted by SPAG
      DIMENSION Wsave(1)
      DATA pih/1.57079632679489661923D0/
      dt = pih/REAL(N,rk)
      fk = 0.0D0
      DO k = 1 , N
         fk = fk + 1.0D0
         Wsave(k) = COS(fk*dt)
      ENDDO
      CALL DFFTI(N,Wsave(N+1))
      END
!*==DCOST.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      SUBROUTINE DCOST(N,X,Wsave)
      USE FFTPACK_KIND
      IMPLICIT NONE
!*--DCOST352
!*** Start of declarations inserted by SPAG
      REAL c1 , FFTPACK_KIND , rk , t1 , t2 , tx2 , Wsave , X , x1h ,   &
         & x1p3 , xi , xim2
      INTEGER i , k , kc , modn , N , nm1 , np1 , ns2
!*** End of declarations inserted by SPAG
      DIMENSION X(*) , Wsave(*)
      nm1 = N - 1
      np1 = N + 1
      ns2 = N/2
      IF ( N<2 ) GOTO 99999
      IF ( N==2 ) THEN
         x1h = X(1) + X(2)
         X(2) = X(1) - X(2)
         X(1) = x1h
         RETURN
      ELSEIF ( N>3 ) THEN
         c1 = X(1) - X(N)
         X(1) = X(1) + X(N)
         DO k = 2 , ns2
            kc = np1 - k
            t1 = X(k) + X(kc)
            t2 = X(k) - X(kc)
            c1 = c1 + Wsave(kc)*t2
            t2 = Wsave(k)*t2
            X(k) = t1 - t2
            X(kc) = t1 + t2
         ENDDO
         modn = MOD(N,2)
         IF ( modn/=0 ) X(ns2+1) = X(ns2+1) + X(ns2+1)
         CALL DFFTF(nm1,X,Wsave(N+1))
         xim2 = X(2)
         X(2) = c1
         DO i = 4 , N , 2
            xi = X(i)
            X(i) = X(i-2) - X(i-1)
            X(i-1) = xim2
            xim2 = xi
         ENDDO
         IF ( modn/=0 ) X(N) = xim2
         GOTO 99999
      ENDIF
      x1p3 = X(1) + X(3)
      tx2 = X(2) + X(2)
      X(2) = X(1) - X(3)
      X(1) = x1p3 + tx2
      X(3) = x1p3 - tx2
      RETURN
99999 END
!*==DCOSTI.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      SUBROUTINE DCOSTI(N,Wsave)
      USE FFTPACK_KIND
      IMPLICIT NONE
!*--DCOSTI405
!*** Start of declarations inserted by SPAG
      REAL dt , FFTPACK_KIND , fk , pi , rk , Wsave
      INTEGER k , kc , N , nm1 , np1 , ns2
!*** End of declarations inserted by SPAG
      DIMENSION Wsave(1)
      DATA pi/3.14159265358979323846D0/
      IF ( N<=3 ) RETURN
      nm1 = N - 1
      np1 = N + 1
      ns2 = N/2
      dt = pi/REAL(nm1,rk)
      fk = 0.0D0
      DO k = 2 , ns2
         kc = np1 - k
         fk = fk + 1.0D0
         Wsave(k) = 2.0D0*SIN(fk*dt)
         Wsave(kc) = 2.0D0*COS(fk*dt)
      ENDDO
      CALL DFFTI(nm1,Wsave(N+1))
      END
!*==DFFTB.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      SUBROUTINE DFFTB(N,R,Wsave)
      USE FFTPACK_KIND
      IMPLICIT NONE
!*--DFFTB430
!*** Start of declarations inserted by SPAG
      REAL FFTPACK_KIND , R , rk , Wsave
      INTEGER N
!*** End of declarations inserted by SPAG
      DIMENSION R(1) , Wsave(1)
      IF ( N==1 ) RETURN
      CALL RFFTB1(N,R,Wsave,Wsave(N+1),Wsave(2*N+1))
      END
!*==DFFTF.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      SUBROUTINE DFFTF(N,R,Wsave)
      USE FFTPACK_KIND
      IMPLICIT NONE
!*--DFFTF443
!*** Start of declarations inserted by SPAG
      REAL FFTPACK_KIND , R , rk , Wsave
      INTEGER N
!*** End of declarations inserted by SPAG
      DIMENSION R(1) , Wsave(1)
      IF ( N==1 ) RETURN
      CALL RFFTF1(N,R,Wsave,Wsave(N+1),Wsave(2*N+1))
      END
!*==DFFTI.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      SUBROUTINE DFFTI(N,Wsave)
      USE FFTPACK_KIND
      IMPLICIT NONE
!*--DFFTI456
!*** Start of declarations inserted by SPAG
      REAL FFTPACK_KIND , rk , Wsave
      INTEGER N
!*** End of declarations inserted by SPAG
      DIMENSION Wsave(1)
      IF ( N==1 ) RETURN
      CALL RFFTI1(N,Wsave(N+1),Wsave(2*N+1))
      END
!*==DSINQB.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      SUBROUTINE DSINQB(N,X,Wsave)
      USE FFTPACK_KIND
      IMPLICIT NONE
!*--DSINQB469
!*** Start of declarations inserted by SPAG
      REAL FFTPACK_KIND , rk , Wsave , X , xhold
      INTEGER k , kc , N , ns2
!*** End of declarations inserted by SPAG
      DIMENSION X(1) , Wsave(1)
      IF ( N>1 ) THEN
         ns2 = N/2
         DO k = 2 , N , 2
            X(k) = -X(k)
         ENDDO
         CALL DCOSQB(N,X,Wsave)
         DO k = 1 , ns2
            kc = N - k
            xhold = X(k)
            X(k) = X(kc+1)
            X(kc+1) = xhold
         ENDDO
         GOTO 99999
      ENDIF
      X(1) = 4.0D0*X(1)
      RETURN
99999 END
!*==DSINQF.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      SUBROUTINE DSINQF(N,X,Wsave)
      USE FFTPACK_KIND
      IMPLICIT NONE
!*--DSINQF496
!*** Start of declarations inserted by SPAG
      REAL FFTPACK_KIND , rk , Wsave , X , xhold
      INTEGER k , kc , N , ns2
!*** End of declarations inserted by SPAG
      DIMENSION X(1) , Wsave(1)
      IF ( N==1 ) RETURN
      ns2 = N/2
      DO k = 1 , ns2
         kc = N - k
         xhold = X(k)
         X(k) = X(kc+1)
         X(kc+1) = xhold
      ENDDO
      CALL DCOSQF(N,X,Wsave)
      DO k = 2 , N , 2
         X(k) = -X(k)
      ENDDO
      END
!*==DSINQI.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      SUBROUTINE DSINQI(N,Wsave)
      USE FFTPACK_KIND
      IMPLICIT NONE
!*--DSINQI519
!*** Start of declarations inserted by SPAG
      REAL FFTPACK_KIND , rk , Wsave
      INTEGER N
!*** End of declarations inserted by SPAG
      DIMENSION Wsave(1)
      CALL DCOSQI(N,Wsave)
      END
!*==DSINT.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      SUBROUTINE DSINT(N,X,Wsave)
      USE FFTPACK_KIND
      IMPLICIT NONE
!*--DSINT531
!*** Start of declarations inserted by SPAG
      REAL FFTPACK_KIND , rk , Wsave , X
      INTEGER iw1 , iw2 , iw3 , N , np1
!*** End of declarations inserted by SPAG
      DIMENSION X(1) , Wsave(1)
      np1 = N + 1
      iw1 = N/2 + 1
      iw2 = iw1 + np1
      iw3 = iw2 + np1
      CALL SINT1(N,X,Wsave,Wsave(iw1),Wsave(iw2),Wsave(iw3))
      END
!*==DSINTI.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      SUBROUTINE DSINTI(N,Wsave)
      USE FFTPACK_KIND
      IMPLICIT NONE
!*--DSINTI547
!*** Start of declarations inserted by SPAG
      REAL dt , FFTPACK_KIND , pi , rk , Wsave
      INTEGER k , N , np1 , ns2
!*** End of declarations inserted by SPAG
      DIMENSION Wsave(1)
      DATA pi/3.14159265358979323846D0/
      IF ( N<=1 ) RETURN
      ns2 = N/2
      np1 = N + 1
      dt = pi/REAL(np1,rk)
      DO k = 1 , ns2
         Wsave(k) = 2.0D0*SIN(k*dt)
      ENDDO
      CALL DFFTI(np1,Wsave(ns2+1))
      END
!*==DZFFTB.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      SUBROUTINE DZFFTB(N,R,Azero,A,B,Wsave)
      USE FFTPACK_KIND
      IMPLICIT NONE
!*--DZFFTB567
!*** Start of declarations inserted by SPAG
      REAL A , Azero , B , FFTPACK_KIND , R , rk , Wsave
      INTEGER i , N , ns2
!*** End of declarations inserted by SPAG
      DIMENSION R(*) , A(*) , B(*) , Wsave(*)
      IF ( N<2 ) THEN
         R(1) = Azero
         RETURN
      ELSEIF ( N==2 ) THEN
         R(1) = Azero + A(1)
         R(2) = Azero - A(1)
         RETURN
      ELSE
         ns2 = (N-1)/2
         DO i = 1 , ns2
            R(2*i) = 0.5D0*A(i)
            R(2*i+1) = -0.5D0*B(i)
         ENDDO
         R(1) = Azero
         IF ( MOD(N,2)==0 ) R(N) = A(ns2+1)
         CALL DFFTB(N,R,Wsave(N+1))
      ENDIF
      END
!*==DZFFTF.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      SUBROUTINE DZFFTF(N,R,Azero,A,B,Wsave)
!
!                       VERSION 3  JUNE 1979
!
      USE FFTPACK_KIND
      IMPLICIT NONE
!*--DZFFTF598
!*** Start of declarations inserted by SPAG
      REAL A , Azero , B , cf , cfm , FFTPACK_KIND , R , rk , Wsave
      INTEGER i , N , ns2 , ns2m
!*** End of declarations inserted by SPAG
      DIMENSION R(*) , A(*) , B(*) , Wsave(*)
      IF ( N<2 ) THEN
         Azero = R(1)
         RETURN
      ELSEIF ( N==2 ) THEN
         Azero = 0.5D0*(R(1)+R(2))
         A(1) = 0.5D0*(R(1)-R(2))
         RETURN
      ELSE
         DO i = 1 , N
            Wsave(i) = R(i)
         ENDDO
         CALL DFFTF(N,Wsave,Wsave(N+1))
         cf = 2.0D0/REAL(N,rk)
         cfm = -cf
         Azero = 0.5D0*cf*Wsave(1)
         ns2 = (N+1)/2
         ns2m = ns2 - 1
         DO i = 1 , ns2m
            A(i) = cf*Wsave(2*i)
            B(i) = cfm*Wsave(2*i+1)
         ENDDO
         IF ( MOD(N,2)==1 ) RETURN
         A(ns2) = 0.5D0*cf*Wsave(N)
         B(ns2) = 0.0D0
      ENDIF
      END
!*==DZFFTI.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      SUBROUTINE DZFFTI(N,Wsave)
      USE FFTPACK_KIND
      IMPLICIT NONE
!*--DZFFTI634
!*** Start of declarations inserted by SPAG
      REAL FFTPACK_KIND , rk , Wsave
      INTEGER N
!*** End of declarations inserted by SPAG
      DIMENSION Wsave(1)
      IF ( N==1 ) RETURN
      CALL EZFFT1(N,Wsave(2*N+1),Wsave(3*N+1))
      END
!*==EZFFT1.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      SUBROUTINE EZFFT1(N,Wa,Ifac)
      USE FFTPACK_KIND
      IMPLICIT NONE
!*--EZFFT1647
!*** Start of declarations inserted by SPAG
      REAL arg1 , argh , ch1 , ch1h , dch1 , dsh1 , FFTPACK_KIND , rk , &
         & sh1 , tpi , Wa
      INTEGER i , ib , ido , Ifac , ii , ip , ipm , is , j , k1 , l1 ,  &
            & l2 , N , nf , nfm1 , nl , nq , nr , ntry , ntryh
!*** End of declarations inserted by SPAG
      DIMENSION Wa(*) , Ifac(*) , ntryh(4)
      DATA ntryh(1) , ntryh(2) , ntryh(3) , ntryh(4)/4 , 2 , 3 , 5/ ,   &
         & tpi/6.28318530717958647692D0/
      nl = N
      nf = 0
      j = 0
 100  j = j + 1
      IF ( j<=4 ) THEN
         ntry = ntryh(j)
      ELSE
         ntry = ntry + 2
      ENDIF
 200  nq = nl/ntry
      nr = nl - ntry*nq
      IF ( nr/=0 ) GOTO 100
      nf = nf + 1
      Ifac(nf+2) = ntry
      nl = nq
      IF ( ntry==2 ) THEN
         IF ( nf/=1 ) THEN
            DO i = 2 , nf
               ib = nf - i + 2
               Ifac(ib+2) = Ifac(ib+1)
            ENDDO
            Ifac(3) = 2
         ENDIF
      ENDIF
      IF ( nl/=1 ) GOTO 200
      Ifac(1) = N
      Ifac(2) = nf
      argh = tpi/REAL(N,rk)
      is = 0
      nfm1 = nf - 1
      l1 = 1
      IF ( nfm1==0 ) RETURN
      DO k1 = 1 , nfm1
         ip = Ifac(k1+2)
         l2 = l1*ip
         ido = N/l2
         ipm = ip - 1
         arg1 = REAL(l1,rk)*argh
         ch1 = 1.0D0
         sh1 = 0.0D0
         dch1 = COS(arg1)
         dsh1 = SIN(arg1)
         DO j = 1 , ipm
            ch1h = dch1*ch1 - dsh1*sh1
            sh1 = dch1*sh1 + dsh1*ch1
            ch1 = ch1h
            i = is + 2
            Wa(i-1) = ch1
            Wa(i) = sh1
            IF ( ido>=5 ) THEN
               DO ii = 5 , ido , 2
                  i = i + 2
                  Wa(i-1) = ch1*Wa(i-3) - sh1*Wa(i-2)
                  Wa(i) = ch1*Wa(i-2) + sh1*Wa(i-3)
               ENDDO
            ENDIF
            is = is + ido
         ENDDO
         l1 = l2
      ENDDO
      END
!*==PASSB.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      SUBROUTINE PASSB(Nac,Ido,Ip,L1,Idl1,Cc,C1,C2,Ch,Ch2,Wa)
      USE FFTPACK_KIND
      IMPLICIT NONE
!*--PASSB722
!*** Start of declarations inserted by SPAG
      REAL C1 , C2 , Cc , Ch , Ch2 , FFTPACK_KIND , rk , Wa , wai , war
      INTEGER i , idij , idj , idl , Idl1 , idlj , Ido , idot , idp ,   &
            & ik , inc , Ip , ipp2 , ipph , j , jc , k , l , L1 , lc
      INTEGER Nac , nt
!*** End of declarations inserted by SPAG
      DIMENSION Ch(Ido,L1,Ip) , Cc(Ido,Ip,L1) , C1(Ido,L1,Ip) , Wa(1) , &
              & C2(Idl1,Ip) , Ch2(Idl1,Ip)
      idot = Ido/2
      nt = Ip*Idl1
      ipp2 = Ip + 2
      ipph = (Ip+1)/2
      idp = Ip*Ido
!
      IF ( Ido<L1 ) THEN
         DO j = 2 , ipph
            jc = ipp2 - j
            DO i = 1 , Ido
               DO k = 1 , L1
                  Ch(i,k,j) = Cc(i,j,k) + Cc(i,jc,k)
                  Ch(i,k,jc) = Cc(i,j,k) - Cc(i,jc,k)
               ENDDO
            ENDDO
         ENDDO
         DO i = 1 , Ido
            DO k = 1 , L1
               Ch(i,k,1) = Cc(i,1,k)
            ENDDO
         ENDDO
      ELSE
         DO j = 2 , ipph
            jc = ipp2 - j
            DO k = 1 , L1
               DO i = 1 , Ido
                  Ch(i,k,j) = Cc(i,j,k) + Cc(i,jc,k)
                  Ch(i,k,jc) = Cc(i,j,k) - Cc(i,jc,k)
               ENDDO
            ENDDO
         ENDDO
         DO k = 1 , L1
            DO i = 1 , Ido
               Ch(i,k,1) = Cc(i,1,k)
            ENDDO
         ENDDO
      ENDIF
      idl = 2 - Ido
      inc = 0
      DO l = 2 , ipph
         lc = ipp2 - l
         idl = idl + Ido
         DO ik = 1 , Idl1
            C2(ik,l) = Ch2(ik,1) + Wa(idl-1)*Ch2(ik,2)
            C2(ik,lc) = Wa(idl)*Ch2(ik,Ip)
         ENDDO
         idlj = idl
         inc = inc + Ido
         DO j = 3 , ipph
            jc = ipp2 - j
            idlj = idlj + inc
            IF ( idlj>idp ) idlj = idlj - idp
            war = Wa(idlj-1)
            wai = Wa(idlj)
            DO ik = 1 , Idl1
               C2(ik,l) = C2(ik,l) + war*Ch2(ik,j)
               C2(ik,lc) = C2(ik,lc) + wai*Ch2(ik,jc)
            ENDDO
         ENDDO
      ENDDO
      DO j = 2 , ipph
         DO ik = 1 , Idl1
            Ch2(ik,1) = Ch2(ik,1) + Ch2(ik,j)
         ENDDO
      ENDDO
      DO j = 2 , ipph
         jc = ipp2 - j
         DO ik = 2 , Idl1 , 2
            Ch2(ik-1,j) = C2(ik-1,j) - C2(ik,jc)
            Ch2(ik-1,jc) = C2(ik-1,j) + C2(ik,jc)
            Ch2(ik,j) = C2(ik,j) + C2(ik-1,jc)
            Ch2(ik,jc) = C2(ik,j) - C2(ik-1,jc)
         ENDDO
      ENDDO
      Nac = 1
      IF ( Ido==2 ) RETURN
      Nac = 0
      DO ik = 1 , Idl1
         C2(ik,1) = Ch2(ik,1)
      ENDDO
      DO j = 2 , Ip
         DO k = 1 , L1
            C1(1,k,j) = Ch(1,k,j)
            C1(2,k,j) = Ch(2,k,j)
         ENDDO
      ENDDO
      IF ( idot>L1 ) THEN
         idj = 2 - Ido
         DO j = 2 , Ip
            idj = idj + Ido
            DO k = 1 , L1
               idij = idj
               DO i = 4 , Ido , 2
                  idij = idij + 2
                  C1(i-1,k,j) = Wa(idij-1)*Ch(i-1,k,j) - Wa(idij)       &
                              & *Ch(i,k,j)
                  C1(i,k,j) = Wa(idij-1)*Ch(i,k,j) + Wa(idij)           &
                            & *Ch(i-1,k,j)
               ENDDO
            ENDDO
         ENDDO
         GOTO 99999
      ENDIF
      idij = 0
      DO j = 2 , Ip
         idij = idij + 2
         DO i = 4 , Ido , 2
            idij = idij + 2
            DO k = 1 , L1
               C1(i-1,k,j) = Wa(idij-1)*Ch(i-1,k,j) - Wa(idij)*Ch(i,k,j)
               C1(i,k,j) = Wa(idij-1)*Ch(i,k,j) + Wa(idij)*Ch(i-1,k,j)
            ENDDO
         ENDDO
      ENDDO
      RETURN
99999 END
!*==PASSB2.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      SUBROUTINE PASSB2(Ido,L1,Cc,Ch,Wa1)
      USE FFTPACK_KIND
      IMPLICIT NONE
!*--PASSB2851
!*** Start of declarations inserted by SPAG
      REAL Cc , Ch , FFTPACK_KIND , rk , ti2 , tr2 , Wa1
      INTEGER i , Ido , k , L1
!*** End of declarations inserted by SPAG
      DIMENSION Cc(Ido,2,L1) , Ch(Ido,L1,2) , Wa1(1)
      IF ( Ido>2 ) THEN
         DO k = 1 , L1
            DO i = 2 , Ido , 2
               Ch(i-1,k,1) = Cc(i-1,1,k) + Cc(i-1,2,k)
               tr2 = Cc(i-1,1,k) - Cc(i-1,2,k)
               Ch(i,k,1) = Cc(i,1,k) + Cc(i,2,k)
               ti2 = Cc(i,1,k) - Cc(i,2,k)
               Ch(i,k,2) = Wa1(i-1)*ti2 + Wa1(i)*tr2
               Ch(i-1,k,2) = Wa1(i-1)*tr2 - Wa1(i)*ti2
            ENDDO
         ENDDO
         GOTO 99999
      ENDIF
      DO k = 1 , L1
         Ch(1,k,1) = Cc(1,1,k) + Cc(1,2,k)
         Ch(1,k,2) = Cc(1,1,k) - Cc(1,2,k)
         Ch(2,k,1) = Cc(2,1,k) + Cc(2,2,k)
         Ch(2,k,2) = Cc(2,1,k) - Cc(2,2,k)
      ENDDO
      RETURN
99999 END
!*==PASSB3.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      SUBROUTINE PASSB3(Ido,L1,Cc,Ch,Wa1,Wa2)
      USE FFTPACK_KIND
      IMPLICIT NONE
!*--PASSB3882
!*** Start of declarations inserted by SPAG
      REAL Cc , Ch , ci2 , ci3 , cr2 , cr3 , di2 , di3 , dr2 , dr3 ,    &
         & FFTPACK_KIND , rk , taui , taur , ti2 , tr2 , Wa1 , Wa2
      INTEGER i , Ido , k , L1
!*** End of declarations inserted by SPAG
      DIMENSION Cc(Ido,3,L1) , Ch(Ido,L1,3) , Wa1(1) , Wa2(1)
!     *** TAUI IS SQRT(3)/2 ***
      DATA taur , taui/ - 0.5D0 , 0.86602540378443864676D0/
      IF ( Ido/=2 ) THEN
         DO k = 1 , L1
            DO i = 2 , Ido , 2
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
            ENDDO
         ENDDO
         GOTO 99999
      ENDIF
      DO k = 1 , L1
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
      ENDDO
      RETURN
99999 END
!*==PASSB4.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      SUBROUTINE PASSB4(Ido,L1,Cc,Ch,Wa1,Wa2,Wa3)
      USE FFTPACK_KIND
      IMPLICIT NONE
!*--PASSB4934
!*** Start of declarations inserted by SPAG
      REAL Cc , Ch , ci2 , ci3 , ci4 , cr2 , cr3 , cr4 , FFTPACK_KIND , &
         & rk , ti1 , ti2 , ti3 , ti4 , tr1 , tr2 , tr3 , tr4 , Wa1 ,   &
         & Wa2
      REAL Wa3
      INTEGER i , Ido , k , L1
!*** End of declarations inserted by SPAG
      DIMENSION Cc(Ido,4,L1) , Ch(Ido,L1,4) , Wa1(1) , Wa2(1) , Wa3(1)
      IF ( Ido/=2 ) THEN
         DO k = 1 , L1
            DO i = 2 , Ido , 2
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
            ENDDO
         ENDDO
         GOTO 99999
      ENDIF
      DO k = 1 , L1
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
      ENDDO
      RETURN
99999 END
!*==PASSB5.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      SUBROUTINE PASSB5(Ido,L1,Cc,Ch,Wa1,Wa2,Wa3,Wa4)
      USE FFTPACK_KIND
      IMPLICIT NONE
!*--PASSB5996
!*** Start of declarations inserted by SPAG
      REAL Cc , Ch , ci2 , ci3 , ci4 , ci5 , cr2 , cr3 , cr4 , cr5 ,    &
         & di2 , di3 , di4 , di5 , dr2 , dr3 , dr4 , dr5 ,              &
         & FFTPACK_KIND , rk
      REAL ti11 , ti12 , ti2 , ti3 , ti4 , ti5 , tr11 , tr12 , tr2 ,    &
         & tr3 , tr4 , tr5 , Wa1 , Wa2 , Wa3 , Wa4
      INTEGER i , Ido , k , L1
!*** End of declarations inserted by SPAG
      DIMENSION Cc(Ido,5,L1) , Ch(Ido,L1,5) , Wa1(1) , Wa2(1) , Wa3(1) ,&
              & Wa4(1)
!     *** TR11=COS(2*PI/5), TI11=SIN(2*PI/5)
!     *** TR12=COS(4*PI/5), TI12=SIN(4*PI/5)
      DATA tr11 , ti11 , tr12 , ti12/0.3090169943749474241D0 ,          &
         & 0.95105651629515357212D0 , -0.8090169943749474241D0 ,        &
         & 0.58778525229247312917D0/
      IF ( Ido/=2 ) THEN
         DO k = 1 , L1
            DO i = 2 , Ido , 2
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
            ENDDO
         ENDDO
         GOTO 99999
      ENDIF
      DO k = 1 , L1
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
      ENDDO
      RETURN
99999 END
!*==PASSF.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      SUBROUTINE PASSF(Nac,Ido,Ip,L1,Idl1,Cc,C1,C2,Ch,Ch2,Wa)
      USE FFTPACK_KIND
      IMPLICIT NONE
!*--PASSF1087
!*** Start of declarations inserted by SPAG
      REAL C1 , C2 , Cc , Ch , Ch2 , FFTPACK_KIND , rk , Wa , wai , war
      INTEGER i , idij , idj , idl , Idl1 , idlj , Ido , idot , idp ,   &
            & ik , inc , Ip , ipp2 , ipph , j , jc , k , l , L1 , lc
      INTEGER Nac , nt
!*** End of declarations inserted by SPAG
      DIMENSION Ch(Ido,L1,Ip) , Cc(Ido,Ip,L1) , C1(Ido,L1,Ip) , Wa(1) , &
              & C2(Idl1,Ip) , Ch2(Idl1,Ip)
      idot = Ido/2
      nt = Ip*Idl1
      ipp2 = Ip + 2
      ipph = (Ip+1)/2
      idp = Ip*Ido
!
      IF ( Ido<L1 ) THEN
         DO j = 2 , ipph
            jc = ipp2 - j
            DO i = 1 , Ido
               DO k = 1 , L1
                  Ch(i,k,j) = Cc(i,j,k) + Cc(i,jc,k)
                  Ch(i,k,jc) = Cc(i,j,k) - Cc(i,jc,k)
               ENDDO
            ENDDO
         ENDDO
         DO i = 1 , Ido
            DO k = 1 , L1
               Ch(i,k,1) = Cc(i,1,k)
            ENDDO
         ENDDO
      ELSE
         DO j = 2 , ipph
            jc = ipp2 - j
            DO k = 1 , L1
               DO i = 1 , Ido
                  Ch(i,k,j) = Cc(i,j,k) + Cc(i,jc,k)
                  Ch(i,k,jc) = Cc(i,j,k) - Cc(i,jc,k)
               ENDDO
            ENDDO
         ENDDO
         DO k = 1 , L1
            DO i = 1 , Ido
               Ch(i,k,1) = Cc(i,1,k)
            ENDDO
         ENDDO
      ENDIF
      idl = 2 - Ido
      inc = 0
      DO l = 2 , ipph
         lc = ipp2 - l
         idl = idl + Ido
         DO ik = 1 , Idl1
            C2(ik,l) = Ch2(ik,1) + Wa(idl-1)*Ch2(ik,2)
            C2(ik,lc) = -Wa(idl)*Ch2(ik,Ip)
         ENDDO
         idlj = idl
         inc = inc + Ido
         DO j = 3 , ipph
            jc = ipp2 - j
            idlj = idlj + inc
            IF ( idlj>idp ) idlj = idlj - idp
            war = Wa(idlj-1)
            wai = Wa(idlj)
            DO ik = 1 , Idl1
               C2(ik,l) = C2(ik,l) + war*Ch2(ik,j)
               C2(ik,lc) = C2(ik,lc) - wai*Ch2(ik,jc)
            ENDDO
         ENDDO
      ENDDO
      DO j = 2 , ipph
         DO ik = 1 , Idl1
            Ch2(ik,1) = Ch2(ik,1) + Ch2(ik,j)
         ENDDO
      ENDDO
      DO j = 2 , ipph
         jc = ipp2 - j
         DO ik = 2 , Idl1 , 2
            Ch2(ik-1,j) = C2(ik-1,j) - C2(ik,jc)
            Ch2(ik-1,jc) = C2(ik-1,j) + C2(ik,jc)
            Ch2(ik,j) = C2(ik,j) + C2(ik-1,jc)
            Ch2(ik,jc) = C2(ik,j) - C2(ik-1,jc)
         ENDDO
      ENDDO
      Nac = 1
      IF ( Ido==2 ) RETURN
      Nac = 0
      DO ik = 1 , Idl1
         C2(ik,1) = Ch2(ik,1)
      ENDDO
      DO j = 2 , Ip
         DO k = 1 , L1
            C1(1,k,j) = Ch(1,k,j)
            C1(2,k,j) = Ch(2,k,j)
         ENDDO
      ENDDO
      IF ( idot>L1 ) THEN
         idj = 2 - Ido
         DO j = 2 , Ip
            idj = idj + Ido
            DO k = 1 , L1
               idij = idj
               DO i = 4 , Ido , 2
                  idij = idij + 2
                  C1(i-1,k,j) = Wa(idij-1)*Ch(i-1,k,j) + Wa(idij)       &
                              & *Ch(i,k,j)
                  C1(i,k,j) = Wa(idij-1)*Ch(i,k,j) - Wa(idij)           &
                            & *Ch(i-1,k,j)
               ENDDO
            ENDDO
         ENDDO
         GOTO 99999
      ENDIF
      idij = 0
      DO j = 2 , Ip
         idij = idij + 2
         DO i = 4 , Ido , 2
            idij = idij + 2
            DO k = 1 , L1
               C1(i-1,k,j) = Wa(idij-1)*Ch(i-1,k,j) + Wa(idij)*Ch(i,k,j)
               C1(i,k,j) = Wa(idij-1)*Ch(i,k,j) - Wa(idij)*Ch(i-1,k,j)
            ENDDO
         ENDDO
      ENDDO
      RETURN
99999 END
!*==PASSF2.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      SUBROUTINE PASSF2(Ido,L1,Cc,Ch,Wa1)
      USE FFTPACK_KIND
      IMPLICIT NONE
!*--PASSF21216
!*** Start of declarations inserted by SPAG
      REAL Cc , Ch , FFTPACK_KIND , rk , ti2 , tr2 , Wa1
      INTEGER i , Ido , k , L1
!*** End of declarations inserted by SPAG
      DIMENSION Cc(Ido,2,L1) , Ch(Ido,L1,2) , Wa1(1)
      IF ( Ido>2 ) THEN
         DO k = 1 , L1
            DO i = 2 , Ido , 2
               Ch(i-1,k,1) = Cc(i-1,1,k) + Cc(i-1,2,k)
               tr2 = Cc(i-1,1,k) - Cc(i-1,2,k)
               Ch(i,k,1) = Cc(i,1,k) + Cc(i,2,k)
               ti2 = Cc(i,1,k) - Cc(i,2,k)
               Ch(i,k,2) = Wa1(i-1)*ti2 - Wa1(i)*tr2
               Ch(i-1,k,2) = Wa1(i-1)*tr2 + Wa1(i)*ti2
            ENDDO
         ENDDO
         GOTO 99999
      ENDIF
      DO k = 1 , L1
         Ch(1,k,1) = Cc(1,1,k) + Cc(1,2,k)
         Ch(1,k,2) = Cc(1,1,k) - Cc(1,2,k)
         Ch(2,k,1) = Cc(2,1,k) + Cc(2,2,k)
         Ch(2,k,2) = Cc(2,1,k) - Cc(2,2,k)
      ENDDO
      RETURN
99999 END
!*==PASSF3.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      SUBROUTINE PASSF3(Ido,L1,Cc,Ch,Wa1,Wa2)
      USE FFTPACK_KIND
      IMPLICIT NONE
!*--PASSF31247
!*** Start of declarations inserted by SPAG
      REAL Cc , Ch , ci2 , ci3 , cr2 , cr3 , di2 , di3 , dr2 , dr3 ,    &
         & FFTPACK_KIND , rk , taui , taur , ti2 , tr2 , Wa1 , Wa2
      INTEGER i , Ido , k , L1
!*** End of declarations inserted by SPAG
      DIMENSION Cc(Ido,3,L1) , Ch(Ido,L1,3) , Wa1(1) , Wa2(1)
!     *** TAUI IS -SQRT(3)/2 ***
      DATA taur , taui/ - 0.5D0 , -0.86602540378443864676D0/
      IF ( Ido/=2 ) THEN
         DO k = 1 , L1
            DO i = 2 , Ido , 2
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
            ENDDO
         ENDDO
         GOTO 99999
      ENDIF
      DO k = 1 , L1
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
      ENDDO
      RETURN
99999 END
!*==PASSF4.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      SUBROUTINE PASSF4(Ido,L1,Cc,Ch,Wa1,Wa2,Wa3)
      USE FFTPACK_KIND
      IMPLICIT NONE
!*--PASSF41299
!*** Start of declarations inserted by SPAG
      REAL Cc , Ch , ci2 , ci3 , ci4 , cr2 , cr3 , cr4 , FFTPACK_KIND , &
         & rk , ti1 , ti2 , ti3 , ti4 , tr1 , tr2 , tr3 , tr4 , Wa1 ,   &
         & Wa2
      REAL Wa3
      INTEGER i , Ido , k , L1
!*** End of declarations inserted by SPAG
      DIMENSION Cc(Ido,4,L1) , Ch(Ido,L1,4) , Wa1(1) , Wa2(1) , Wa3(1)
      IF ( Ido/=2 ) THEN
         DO k = 1 , L1
            DO i = 2 , Ido , 2
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
            ENDDO
         ENDDO
         GOTO 99999
      ENDIF
      DO k = 1 , L1
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
      ENDDO
      RETURN
99999 END
!*==PASSF5.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      SUBROUTINE PASSF5(Ido,L1,Cc,Ch,Wa1,Wa2,Wa3,Wa4)
      USE FFTPACK_KIND
      IMPLICIT NONE
!*--PASSF51361
!*** Start of declarations inserted by SPAG
      REAL Cc , Ch , ci2 , ci3 , ci4 , ci5 , cr2 , cr3 , cr4 , cr5 ,    &
         & di2 , di3 , di4 , di5 , dr2 , dr3 , dr4 , dr5 ,              &
         & FFTPACK_KIND , rk
      REAL ti11 , ti12 , ti2 , ti3 , ti4 , ti5 , tr11 , tr12 , tr2 ,    &
         & tr3 , tr4 , tr5 , Wa1 , Wa2 , Wa3 , Wa4
      INTEGER i , Ido , k , L1
!*** End of declarations inserted by SPAG
      DIMENSION Cc(Ido,5,L1) , Ch(Ido,L1,5) , Wa1(1) , Wa2(1) , Wa3(1) ,&
              & Wa4(1)
!     *** TR11=COS(2*PI/5), TI11=-SIN(2*PI/5)
!     *** TR12=-COS(4*PI/5), TI12=-SIN(4*PI/5)
      DATA tr11 , ti11 , tr12 , ti12/0.3090169943749474241D0 ,          &
         & -0.95105651629515357212D0 , -0.8090169943749474241D0 ,       &
         & -0.58778525229247312917D0/
      IF ( Ido/=2 ) THEN
         DO k = 1 , L1
            DO i = 2 , Ido , 2
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
            ENDDO
         ENDDO
         GOTO 99999
      ENDIF
      DO k = 1 , L1
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
      ENDDO
      RETURN
99999 END
!*==RADB2.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      SUBROUTINE RADB2(Ido,L1,Cc,Ch,Wa1)
      USE FFTPACK_KIND
      IMPLICIT NONE
!*--RADB21452
!*** Start of declarations inserted by SPAG
      REAL Cc , Ch , FFTPACK_KIND , rk , ti2 , tr2 , Wa1
      INTEGER i , ic , Ido , idp2 , k , L1
!*** End of declarations inserted by SPAG
      DIMENSION Cc(Ido,2,L1) , Ch(Ido,L1,2) , Wa1(1)
      DO k = 1 , L1
         Ch(1,k,1) = Cc(1,1,k) + Cc(Ido,2,k)
         Ch(1,k,2) = Cc(1,1,k) - Cc(Ido,2,k)
      ENDDO
      IF ( Ido<2 ) GOTO 99999
      IF ( Ido/=2 ) THEN
         idp2 = Ido + 2
         DO k = 1 , L1
            DO i = 3 , Ido , 2
               ic = idp2 - i
               Ch(i-1,k,1) = Cc(i-1,1,k) + Cc(ic-1,2,k)
               tr2 = Cc(i-1,1,k) - Cc(ic-1,2,k)
               Ch(i,k,1) = Cc(i,1,k) - Cc(ic,2,k)
               ti2 = Cc(i,1,k) + Cc(ic,2,k)
               Ch(i-1,k,2) = Wa1(i-2)*tr2 - Wa1(i-1)*ti2
               Ch(i,k,2) = Wa1(i-2)*ti2 + Wa1(i-1)*tr2
            ENDDO
         ENDDO
         IF ( MOD(Ido,2)==1 ) RETURN
      ENDIF
      DO k = 1 , L1
         Ch(Ido,k,1) = Cc(Ido,1,k) + Cc(Ido,1,k)
         Ch(Ido,k,2) = -(Cc(1,2,k)+Cc(1,2,k))
      ENDDO
99999 END
!*==RADB3.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      SUBROUTINE RADB3(Ido,L1,Cc,Ch,Wa1,Wa2)
      USE FFTPACK_KIND
      IMPLICIT NONE
!*--RADB31487
!*** Start of declarations inserted by SPAG
      REAL Cc , Ch , ci2 , ci3 , cr2 , cr3 , di2 , di3 , dr2 , dr3 ,    &
         & FFTPACK_KIND , rk , taui , taur , ti2 , tr2 , Wa1 , Wa2
      INTEGER i , ic , Ido , idp2 , k , L1
!*** End of declarations inserted by SPAG
      DIMENSION Cc(Ido,3,L1) , Ch(Ido,L1,3) , Wa1(1) , Wa2(1)
!     *** TAUI IS SQRT(3)/2 ***
      DATA taur , taui/ - 0.5D0 , 0.86602540378443864676D0/
      DO k = 1 , L1
         tr2 = Cc(Ido,2,k) + Cc(Ido,2,k)
         cr2 = Cc(1,1,k) + taur*tr2
         Ch(1,k,1) = Cc(1,1,k) + tr2
         ci3 = taui*(Cc(1,3,k)+Cc(1,3,k))
         Ch(1,k,2) = cr2 - ci3
         Ch(1,k,3) = cr2 + ci3
      ENDDO
      IF ( Ido==1 ) RETURN
      idp2 = Ido + 2
      DO k = 1 , L1
         DO i = 3 , Ido , 2
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
         ENDDO
      ENDDO
      END
!*==RADB4.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      SUBROUTINE RADB4(Ido,L1,Cc,Ch,Wa1,Wa2,Wa3)
      USE FFTPACK_KIND
      IMPLICIT NONE
!*--RADB41532
!*** Start of declarations inserted by SPAG
      REAL Cc , Ch , ci2 , ci3 , ci4 , cr2 , cr3 , cr4 , FFTPACK_KIND , &
         & rk , sqrt2 , ti1 , ti2 , ti3 , ti4 , tr1 , tr2 , tr3 , tr4 , &
         & Wa1
      REAL Wa2 , Wa3
      INTEGER i , ic , Ido , idp2 , k , L1
!*** End of declarations inserted by SPAG
      DIMENSION Cc(Ido,4,L1) , Ch(Ido,L1,4) , Wa1(1) , Wa2(1) , Wa3(1)
      DATA sqrt2/1.41421356237309504880D0/
      DO k = 1 , L1
         tr1 = Cc(1,1,k) - Cc(Ido,4,k)
         tr2 = Cc(1,1,k) + Cc(Ido,4,k)
         tr3 = Cc(Ido,2,k) + Cc(Ido,2,k)
         tr4 = Cc(1,3,k) + Cc(1,3,k)
         Ch(1,k,1) = tr2 + tr3
         Ch(1,k,2) = tr1 - tr4
         Ch(1,k,3) = tr2 - tr3
         Ch(1,k,4) = tr1 + tr4
      ENDDO
      IF ( Ido<2 ) GOTO 99999
      IF ( Ido/=2 ) THEN
         idp2 = Ido + 2
         DO k = 1 , L1
            DO i = 3 , Ido , 2
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
            ENDDO
         ENDDO
         IF ( MOD(Ido,2)==1 ) RETURN
      ENDIF
      DO k = 1 , L1
         ti1 = Cc(1,2,k) + Cc(1,4,k)
         ti2 = Cc(1,4,k) - Cc(1,2,k)
         tr1 = Cc(Ido,1,k) - Cc(Ido,3,k)
         tr2 = Cc(Ido,1,k) + Cc(Ido,3,k)
         Ch(Ido,k,1) = tr2 + tr2
         Ch(Ido,k,2) = sqrt2*(tr1-ti1)
         Ch(Ido,k,3) = ti2 + ti2
         Ch(Ido,k,4) = -sqrt2*(tr1+ti1)
      ENDDO
99999 END
!*==RADB5.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      SUBROUTINE RADB5(Ido,L1,Cc,Ch,Wa1,Wa2,Wa3,Wa4)
      USE FFTPACK_KIND
      IMPLICIT NONE
!*--RADB51599
!*** Start of declarations inserted by SPAG
      REAL Cc , Ch , ci2 , ci3 , ci4 , ci5 , cr2 , cr3 , cr4 , cr5 ,    &
         & di2 , di3 , di4 , di5 , dr2 , dr3 , dr4 , dr5 ,              &
         & FFTPACK_KIND , rk
      REAL ti11 , ti12 , ti2 , ti3 , ti4 , ti5 , tr11 , tr12 , tr2 ,    &
         & tr3 , tr4 , tr5 , Wa1 , Wa2 , Wa3 , Wa4
      INTEGER i , ic , Ido , idp2 , k , L1
!*** End of declarations inserted by SPAG
      DIMENSION Cc(Ido,5,L1) , Ch(Ido,L1,5) , Wa1(1) , Wa2(1) , Wa3(1) ,&
              & Wa4(1)
!     *** TR11=COS(2*PI/5), TI11=SIN(2*PI/5)
!     *** TR12=COS(4*PI/5), TI12=SIN(4*PI/5)
      DATA tr11 , ti11 , tr12 , ti12/0.3090169943749474241D0 ,          &
         & 0.95105651629515357212D0 , -0.8090169943749474241D0 ,        &
         & 0.58778525229247312917D0/
      DO k = 1 , L1
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
      ENDDO
      IF ( Ido==1 ) RETURN
      idp2 = Ido + 2
      DO k = 1 , L1
         DO i = 3 , Ido , 2
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
         ENDDO
      ENDDO
      END
!*==RADBG.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      SUBROUTINE RADBG(Ido,Ip,L1,Idl1,Cc,C1,C2,Ch,Ch2,Wa)
      USE FFTPACK_KIND
      IMPLICIT NONE
!*--RADBG1676
!*** Start of declarations inserted by SPAG
      REAL ai1 , ai2 , ar1 , ar1h , ar2 , ar2h , arg , C1 , C2 , Cc ,   &
         & Ch , Ch2 , dc2 , dcp , ds2 , dsp , FFTPACK_KIND , rk , tpi , &
         & Wa
      INTEGER i , ic , idij , Idl1 , Ido , idp2 , ik , Ip , ipp2 ,      &
            & ipph , is , j , j2 , jc , k , l , L1 , lc , nbd
!*** End of declarations inserted by SPAG
      DIMENSION Ch(Ido,L1,Ip) , Cc(Ido,Ip,L1) , C1(Ido,L1,Ip) ,         &
              & C2(Idl1,Ip) , Ch2(Idl1,Ip) , Wa(1)
      DATA tpi/6.28318530717958647692D0/
      arg = tpi/REAL(Ip,rk)
      dcp = COS(arg)
      dsp = SIN(arg)
      idp2 = Ido + 2
      nbd = (Ido-1)/2
      ipp2 = Ip + 2
      ipph = (Ip+1)/2
      IF ( Ido<L1 ) THEN
         DO i = 1 , Ido
            DO k = 1 , L1
               Ch(i,k,1) = Cc(i,1,k)
            ENDDO
         ENDDO
      ELSE
         DO k = 1 , L1
            DO i = 1 , Ido
               Ch(i,k,1) = Cc(i,1,k)
            ENDDO
         ENDDO
      ENDIF
      DO j = 2 , ipph
         jc = ipp2 - j
         j2 = j + j
         DO k = 1 , L1
            Ch(1,k,j) = Cc(Ido,j2-2,k) + Cc(Ido,j2-2,k)
            Ch(1,k,jc) = Cc(1,j2-1,k) + Cc(1,j2-1,k)
         ENDDO
      ENDDO
      IF ( Ido/=1 ) THEN
         IF ( nbd<L1 ) THEN
            DO j = 2 , ipph
               jc = ipp2 - j
               DO i = 3 , Ido , 2
                  ic = idp2 - i
                  DO k = 1 , L1
                     Ch(i-1,k,j) = Cc(i-1,2*j-1,k) + Cc(ic-1,2*j-2,k)
                     Ch(i-1,k,jc) = Cc(i-1,2*j-1,k) - Cc(ic-1,2*j-2,k)
                     Ch(i,k,j) = Cc(i,2*j-1,k) - Cc(ic,2*j-2,k)
                     Ch(i,k,jc) = Cc(i,2*j-1,k) + Cc(ic,2*j-2,k)
                  ENDDO
               ENDDO
            ENDDO
         ELSE
            DO j = 2 , ipph
               jc = ipp2 - j
               DO k = 1 , L1
                  DO i = 3 , Ido , 2
                     ic = idp2 - i
                     Ch(i-1,k,j) = Cc(i-1,2*j-1,k) + Cc(ic-1,2*j-2,k)
                     Ch(i-1,k,jc) = Cc(i-1,2*j-1,k) - Cc(ic-1,2*j-2,k)
                     Ch(i,k,j) = Cc(i,2*j-1,k) - Cc(ic,2*j-2,k)
                     Ch(i,k,jc) = Cc(i,2*j-1,k) + Cc(ic,2*j-2,k)
                  ENDDO
               ENDDO
            ENDDO
         ENDIF
      ENDIF
      ar1 = 1.0D0
      ai1 = 0.0D0
      DO l = 2 , ipph
         lc = ipp2 - l
         ar1h = dcp*ar1 - dsp*ai1
         ai1 = dcp*ai1 + dsp*ar1
         ar1 = ar1h
         DO ik = 1 , Idl1
            C2(ik,l) = Ch2(ik,1) + ar1*Ch2(ik,2)
            C2(ik,lc) = ai1*Ch2(ik,Ip)
         ENDDO
         dc2 = ar1
         ds2 = ai1
         ar2 = ar1
         ai2 = ai1
         DO j = 3 , ipph
            jc = ipp2 - j
            ar2h = dc2*ar2 - ds2*ai2
            ai2 = dc2*ai2 + ds2*ar2
            ar2 = ar2h
            DO ik = 1 , Idl1
               C2(ik,l) = C2(ik,l) + ar2*Ch2(ik,j)
               C2(ik,lc) = C2(ik,lc) + ai2*Ch2(ik,jc)
            ENDDO
         ENDDO
      ENDDO
      DO j = 2 , ipph
         DO ik = 1 , Idl1
            Ch2(ik,1) = Ch2(ik,1) + Ch2(ik,j)
         ENDDO
      ENDDO
      DO j = 2 , ipph
         jc = ipp2 - j
         DO k = 1 , L1
            Ch(1,k,j) = C1(1,k,j) - C1(1,k,jc)
            Ch(1,k,jc) = C1(1,k,j) + C1(1,k,jc)
         ENDDO
      ENDDO
      IF ( Ido/=1 ) THEN
         IF ( nbd<L1 ) THEN
            DO j = 2 , ipph
               jc = ipp2 - j
               DO i = 3 , Ido , 2
                  DO k = 1 , L1
                     Ch(i-1,k,j) = C1(i-1,k,j) - C1(i,k,jc)
                     Ch(i-1,k,jc) = C1(i-1,k,j) + C1(i,k,jc)
                     Ch(i,k,j) = C1(i,k,j) + C1(i-1,k,jc)
                     Ch(i,k,jc) = C1(i,k,j) - C1(i-1,k,jc)
                  ENDDO
               ENDDO
            ENDDO
         ELSE
            DO j = 2 , ipph
               jc = ipp2 - j
               DO k = 1 , L1
                  DO i = 3 , Ido , 2
                     Ch(i-1,k,j) = C1(i-1,k,j) - C1(i,k,jc)
                     Ch(i-1,k,jc) = C1(i-1,k,j) + C1(i,k,jc)
                     Ch(i,k,j) = C1(i,k,j) + C1(i-1,k,jc)
                     Ch(i,k,jc) = C1(i,k,j) - C1(i-1,k,jc)
                  ENDDO
               ENDDO
            ENDDO
         ENDIF
      ENDIF
      IF ( Ido==1 ) RETURN
      DO ik = 1 , Idl1
         C2(ik,1) = Ch2(ik,1)
      ENDDO
      DO j = 2 , Ip
         DO k = 1 , L1
            C1(1,k,j) = Ch(1,k,j)
         ENDDO
      ENDDO
      IF ( nbd>L1 ) THEN
         is = -Ido
         DO j = 2 , Ip
            is = is + Ido
            DO k = 1 , L1
               idij = is
               DO i = 3 , Ido , 2
                  idij = idij + 2
                  C1(i-1,k,j) = Wa(idij-1)*Ch(i-1,k,j) - Wa(idij)       &
                              & *Ch(i,k,j)
                  C1(i,k,j) = Wa(idij-1)*Ch(i,k,j) + Wa(idij)           &
                            & *Ch(i-1,k,j)
               ENDDO
            ENDDO
         ENDDO
      ELSE
         is = -Ido
         DO j = 2 , Ip
            is = is + Ido
            idij = is
            DO i = 3 , Ido , 2
               idij = idij + 2
               DO k = 1 , L1
                  C1(i-1,k,j) = Wa(idij-1)*Ch(i-1,k,j) - Wa(idij)       &
                              & *Ch(i,k,j)
                  C1(i,k,j) = Wa(idij-1)*Ch(i,k,j) + Wa(idij)           &
                            & *Ch(i-1,k,j)
               ENDDO
            ENDDO
         ENDDO
      ENDIF
      END
!*==RADF2.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      SUBROUTINE RADF2(Ido,L1,Cc,Ch,Wa1)
      USE FFTPACK_KIND
      IMPLICIT NONE
!*--RADF21854
!*** Start of declarations inserted by SPAG
      REAL Cc , Ch , FFTPACK_KIND , rk , ti2 , tr2 , Wa1
      INTEGER i , ic , Ido , idp2 , k , L1
!*** End of declarations inserted by SPAG
      DIMENSION Ch(Ido,2,L1) , Cc(Ido,L1,2) , Wa1(1)
      DO k = 1 , L1
         Ch(1,1,k) = Cc(1,k,1) + Cc(1,k,2)
         Ch(Ido,2,k) = Cc(1,k,1) - Cc(1,k,2)
      ENDDO
      IF ( Ido<2 ) GOTO 99999
      IF ( Ido/=2 ) THEN
         idp2 = Ido + 2
         DO k = 1 , L1
            DO i = 3 , Ido , 2
               ic = idp2 - i
               tr2 = Wa1(i-2)*Cc(i-1,k,2) + Wa1(i-1)*Cc(i,k,2)
               ti2 = Wa1(i-2)*Cc(i,k,2) - Wa1(i-1)*Cc(i-1,k,2)
               Ch(i,1,k) = Cc(i,k,1) + ti2
               Ch(ic,2,k) = ti2 - Cc(i,k,1)
               Ch(i-1,1,k) = Cc(i-1,k,1) + tr2
               Ch(ic-1,2,k) = Cc(i-1,k,1) - tr2
            ENDDO
         ENDDO
         IF ( MOD(Ido,2)==1 ) RETURN
      ENDIF
      DO k = 1 , L1
         Ch(1,2,k) = -Cc(Ido,k,2)
         Ch(Ido,1,k) = Cc(Ido,k,1)
      ENDDO
99999 END
!*==RADF3.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      SUBROUTINE RADF3(Ido,L1,Cc,Ch,Wa1,Wa2)
      USE FFTPACK_KIND
      IMPLICIT NONE
!*--RADF31889
!*** Start of declarations inserted by SPAG
      REAL Cc , Ch , ci2 , cr2 , di2 , di3 , dr2 , dr3 , FFTPACK_KIND , &
         & rk , taui , taur , ti2 , ti3 , tr2 , tr3 , Wa1 , Wa2
      INTEGER i , ic , Ido , idp2 , k , L1
!*** End of declarations inserted by SPAG
      DIMENSION Ch(Ido,3,L1) , Cc(Ido,L1,3) , Wa1(1) , Wa2(1)
!     *** TAUI IS -SQRT(3)/2 ***
      DATA taur , taui/ - 0.5D0 , 0.86602540378443864676D0/
      DO k = 1 , L1
         cr2 = Cc(1,k,2) + Cc(1,k,3)
         Ch(1,1,k) = Cc(1,k,1) + cr2
         Ch(1,3,k) = taui*(Cc(1,k,3)-Cc(1,k,2))
         Ch(Ido,2,k) = Cc(1,k,1) + taur*cr2
      ENDDO
      IF ( Ido==1 ) RETURN
      idp2 = Ido + 2
      DO k = 1 , L1
         DO i = 3 , Ido , 2
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
         ENDDO
      ENDDO
      END
!*==RADF4.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      SUBROUTINE RADF4(Ido,L1,Cc,Ch,Wa1,Wa2,Wa3)
      USE FFTPACK_KIND
      IMPLICIT NONE
!*--RADF41932
!*** Start of declarations inserted by SPAG
      REAL Cc , Ch , ci2 , ci3 , ci4 , cr2 , cr3 , cr4 , FFTPACK_KIND , &
         & hsqt2 , rk , ti1 , ti2 , ti3 , ti4 , tr1 , tr2 , tr3 , tr4 , &
         & Wa1
      REAL Wa2 , Wa3
      INTEGER i , ic , Ido , idp2 , k , L1
!*** End of declarations inserted by SPAG
      DIMENSION Cc(Ido,L1,4) , Ch(Ido,4,L1) , Wa1(1) , Wa2(1) , Wa3(1)
      DATA hsqt2/0.70710678118654752440D0/
      DO k = 1 , L1
         tr1 = Cc(1,k,2) + Cc(1,k,4)
         tr2 = Cc(1,k,1) + Cc(1,k,3)
         Ch(1,1,k) = tr1 + tr2
         Ch(Ido,4,k) = tr2 - tr1
         Ch(Ido,2,k) = Cc(1,k,1) - Cc(1,k,3)
         Ch(1,3,k) = Cc(1,k,4) - Cc(1,k,2)
      ENDDO
      IF ( Ido<2 ) GOTO 99999
      IF ( Ido/=2 ) THEN
         idp2 = Ido + 2
         DO k = 1 , L1
            DO i = 3 , Ido , 2
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
            ENDDO
         ENDDO
         IF ( MOD(Ido,2)==1 ) RETURN
      ENDIF
      DO k = 1 , L1
         ti1 = -hsqt2*(Cc(Ido,k,2)+Cc(Ido,k,4))
         tr1 = hsqt2*(Cc(Ido,k,2)-Cc(Ido,k,4))
         Ch(Ido,1,k) = tr1 + Cc(Ido,k,1)
         Ch(Ido,3,k) = Cc(Ido,k,1) - tr1
         Ch(1,2,k) = ti1 - Cc(Ido,k,3)
         Ch(1,4,k) = ti1 + Cc(Ido,k,3)
      ENDDO
99999 END
!*==RADF5.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      SUBROUTINE RADF5(Ido,L1,Cc,Ch,Wa1,Wa2,Wa3,Wa4)
      USE FFTPACK_KIND
      IMPLICIT NONE
!*--RADF51995
!*** Start of declarations inserted by SPAG
      REAL Cc , Ch , ci2 , ci3 , ci4 , ci5 , cr2 , cr3 , cr4 , cr5 ,    &
         & di2 , di3 , di4 , di5 , dr2 , dr3 , dr4 , dr5 ,              &
         & FFTPACK_KIND , rk
      REAL ti11 , ti12 , ti2 , ti3 , ti4 , ti5 , tr11 , tr12 , tr2 ,    &
         & tr3 , tr4 , tr5 , Wa1 , Wa2 , Wa3 , Wa4
      INTEGER i , ic , Ido , idp2 , k , L1
!*** End of declarations inserted by SPAG
      DIMENSION Cc(Ido,L1,5) , Ch(Ido,5,L1) , Wa1(1) , Wa2(1) , Wa3(1) ,&
              & Wa4(1)
      DATA tr11 , ti11 , tr12 , ti12/0.3090169943749474241D0 ,          &
         & 0.95105651629515357212D0 , -0.8090169943749474241D0 ,        &
         & 0.58778525229247312917D0/
      DO k = 1 , L1
         cr2 = Cc(1,k,5) + Cc(1,k,2)
         ci5 = Cc(1,k,5) - Cc(1,k,2)
         cr3 = Cc(1,k,4) + Cc(1,k,3)
         ci4 = Cc(1,k,4) - Cc(1,k,3)
         Ch(1,1,k) = Cc(1,k,1) + cr2 + cr3
         Ch(Ido,2,k) = Cc(1,k,1) + tr11*cr2 + tr12*cr3
         Ch(1,3,k) = ti11*ci5 + ti12*ci4
         Ch(Ido,4,k) = Cc(1,k,1) + tr12*cr2 + tr11*cr3
         Ch(1,5,k) = ti12*ci5 - ti11*ci4
      ENDDO
      IF ( Ido==1 ) RETURN
      idp2 = Ido + 2
      DO k = 1 , L1
         DO i = 3 , Ido , 2
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
         ENDDO
      ENDDO
      END
!*==RADFG.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      SUBROUTINE RADFG(Ido,Ip,L1,Idl1,Cc,C1,C2,Ch,Ch2,Wa)
      USE FFTPACK_KIND
      IMPLICIT NONE
!*--RADFG2066
!*** Start of declarations inserted by SPAG
      REAL ai1 , ai2 , ar1 , ar1h , ar2 , ar2h , arg , C1 , C2 , Cc ,   &
         & Ch , Ch2 , dc2 , dcp , ds2 , dsp , FFTPACK_KIND , rk , tpi , &
         & Wa
      INTEGER i , ic , idij , Idl1 , Ido , idp2 , ik , Ip , ipp2 ,      &
            & ipph , is , j , j2 , jc , k , l , L1 , lc , nbd
!*** End of declarations inserted by SPAG
      DIMENSION Ch(Ido,L1,Ip) , Cc(Ido,Ip,L1) , C1(Ido,L1,Ip) ,         &
              & C2(Idl1,Ip) , Ch2(Idl1,Ip) , Wa(1)
      DATA tpi/6.28318530717958647692D0/
      arg = tpi/REAL(Ip,rk)
      dcp = COS(arg)
      dsp = SIN(arg)
      ipph = (Ip+1)/2
      ipp2 = Ip + 2
      idp2 = Ido + 2
      nbd = (Ido-1)/2
      IF ( Ido==1 ) THEN
         DO ik = 1 , Idl1
            C2(ik,1) = Ch2(ik,1)
         ENDDO
      ELSE
         DO ik = 1 , Idl1
            Ch2(ik,1) = C2(ik,1)
         ENDDO
         DO j = 2 , Ip
            DO k = 1 , L1
               Ch(1,k,j) = C1(1,k,j)
            ENDDO
         ENDDO
         IF ( nbd>L1 ) THEN
            is = -Ido
            DO j = 2 , Ip
               is = is + Ido
               DO k = 1 , L1
                  idij = is
                  DO i = 3 , Ido , 2
                     idij = idij + 2
                     Ch(i-1,k,j) = Wa(idij-1)*C1(i-1,k,j) + Wa(idij)    &
                                 & *C1(i,k,j)
                     Ch(i,k,j) = Wa(idij-1)*C1(i,k,j) - Wa(idij)        &
                               & *C1(i-1,k,j)
                  ENDDO
               ENDDO
            ENDDO
         ELSE
            is = -Ido
            DO j = 2 , Ip
               is = is + Ido
               idij = is
               DO i = 3 , Ido , 2
                  idij = idij + 2
                  DO k = 1 , L1
                     Ch(i-1,k,j) = Wa(idij-1)*C1(i-1,k,j) + Wa(idij)    &
                                 & *C1(i,k,j)
                     Ch(i,k,j) = Wa(idij-1)*C1(i,k,j) - Wa(idij)        &
                               & *C1(i-1,k,j)
                  ENDDO
               ENDDO
            ENDDO
         ENDIF
         IF ( nbd<L1 ) THEN
            DO j = 2 , ipph
               jc = ipp2 - j
               DO i = 3 , Ido , 2
                  DO k = 1 , L1
                     C1(i-1,k,j) = Ch(i-1,k,j) + Ch(i-1,k,jc)
                     C1(i-1,k,jc) = Ch(i,k,j) - Ch(i,k,jc)
                     C1(i,k,j) = Ch(i,k,j) + Ch(i,k,jc)
                     C1(i,k,jc) = Ch(i-1,k,jc) - Ch(i-1,k,j)
                  ENDDO
               ENDDO
            ENDDO
         ELSE
            DO j = 2 , ipph
               jc = ipp2 - j
               DO k = 1 , L1
                  DO i = 3 , Ido , 2
                     C1(i-1,k,j) = Ch(i-1,k,j) + Ch(i-1,k,jc)
                     C1(i-1,k,jc) = Ch(i,k,j) - Ch(i,k,jc)
                     C1(i,k,j) = Ch(i,k,j) + Ch(i,k,jc)
                     C1(i,k,jc) = Ch(i-1,k,jc) - Ch(i-1,k,j)
                  ENDDO
               ENDDO
            ENDDO
         ENDIF
      ENDIF
      DO j = 2 , ipph
         jc = ipp2 - j
         DO k = 1 , L1
            C1(1,k,j) = Ch(1,k,j) + Ch(1,k,jc)
            C1(1,k,jc) = Ch(1,k,jc) - Ch(1,k,j)
         ENDDO
      ENDDO
!
      ar1 = 1.0D0
      ai1 = 0.0D0
      DO l = 2 , ipph
         lc = ipp2 - l
         ar1h = dcp*ar1 - dsp*ai1
         ai1 = dcp*ai1 + dsp*ar1
         ar1 = ar1h
         DO ik = 1 , Idl1
            Ch2(ik,l) = C2(ik,1) + ar1*C2(ik,2)
            Ch2(ik,lc) = ai1*C2(ik,Ip)
         ENDDO
         dc2 = ar1
         ds2 = ai1
         ar2 = ar1
         ai2 = ai1
         DO j = 3 , ipph
            jc = ipp2 - j
            ar2h = dc2*ar2 - ds2*ai2
            ai2 = dc2*ai2 + ds2*ar2
            ar2 = ar2h
            DO ik = 1 , Idl1
               Ch2(ik,l) = Ch2(ik,l) + ar2*C2(ik,j)
               Ch2(ik,lc) = Ch2(ik,lc) + ai2*C2(ik,jc)
            ENDDO
         ENDDO
      ENDDO
      DO j = 2 , ipph
         DO ik = 1 , Idl1
            Ch2(ik,1) = Ch2(ik,1) + C2(ik,j)
         ENDDO
      ENDDO
!
      IF ( Ido<L1 ) THEN
         DO i = 1 , Ido
            DO k = 1 , L1
               Cc(i,1,k) = Ch(i,k,1)
            ENDDO
         ENDDO
      ELSE
         DO k = 1 , L1
            DO i = 1 , Ido
               Cc(i,1,k) = Ch(i,k,1)
            ENDDO
         ENDDO
      ENDIF
      DO j = 2 , ipph
         jc = ipp2 - j
         j2 = j + j
         DO k = 1 , L1
            Cc(Ido,j2-2,k) = Ch(1,k,j)
            Cc(1,j2-1,k) = Ch(1,k,jc)
         ENDDO
      ENDDO
      IF ( Ido==1 ) RETURN
      IF ( nbd<L1 ) THEN
         DO j = 2 , ipph
            jc = ipp2 - j
            j2 = j + j
            DO i = 3 , Ido , 2
               ic = idp2 - i
               DO k = 1 , L1
                  Cc(i-1,j2-1,k) = Ch(i-1,k,j) + Ch(i-1,k,jc)
                  Cc(ic-1,j2-2,k) = Ch(i-1,k,j) - Ch(i-1,k,jc)
                  Cc(i,j2-1,k) = Ch(i,k,j) + Ch(i,k,jc)
                  Cc(ic,j2-2,k) = Ch(i,k,jc) - Ch(i,k,j)
               ENDDO
            ENDDO
         ENDDO
         GOTO 99999
      ENDIF
      DO j = 2 , ipph
         jc = ipp2 - j
         j2 = j + j
         DO k = 1 , L1
            DO i = 3 , Ido , 2
               ic = idp2 - i
               Cc(i-1,j2-1,k) = Ch(i-1,k,j) + Ch(i-1,k,jc)
               Cc(ic-1,j2-2,k) = Ch(i-1,k,j) - Ch(i-1,k,jc)
               Cc(i,j2-1,k) = Ch(i,k,j) + Ch(i,k,jc)
               Cc(ic,j2-2,k) = Ch(i,k,jc) - Ch(i,k,j)
            ENDDO
         ENDDO
      ENDDO
      RETURN
99999 END
!*==RFFTB1.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      SUBROUTINE RFFTB1(N,C,Ch,Wa,Ifac)
      USE FFTPACK_KIND
      IMPLICIT NONE
!*--RFFTB12251
!*** Start of declarations inserted by SPAG
      REAL C , Ch , FFTPACK_KIND , rk , Wa
      INTEGER i , idl1 , ido , Ifac , ip , iw , ix2 , ix3 , ix4 , k1 ,  &
            & l1 , l2 , N , na , nf
!*** End of declarations inserted by SPAG
      DIMENSION Ch(*) , C(*) , Wa(*) , Ifac(*)
      nf = Ifac(2)
      na = 0
      l1 = 1
      iw = 1
      DO k1 = 1 , nf
         ip = Ifac(k1+2)
         l2 = ip*l1
         ido = N/l2
         idl1 = ido*l1
         IF ( ip==4 ) THEN
            ix2 = iw + ido
            ix3 = ix2 + ido
            IF ( na/=0 ) THEN
               CALL RADB4(ido,l1,Ch,C,Wa(iw),Wa(ix2),Wa(ix3))
            ELSE
               CALL RADB4(ido,l1,C,Ch,Wa(iw),Wa(ix2),Wa(ix3))
            ENDIF
            na = 1 - na
         ELSEIF ( ip==2 ) THEN
            IF ( na/=0 ) THEN
               CALL RADB2(ido,l1,Ch,C,Wa(iw))
            ELSE
               CALL RADB2(ido,l1,C,Ch,Wa(iw))
            ENDIF
            na = 1 - na
         ELSEIF ( ip==3 ) THEN
            ix2 = iw + ido
            IF ( na/=0 ) THEN
               CALL RADB3(ido,l1,Ch,C,Wa(iw),Wa(ix2))
            ELSE
               CALL RADB3(ido,l1,C,Ch,Wa(iw),Wa(ix2))
            ENDIF
            na = 1 - na
         ELSEIF ( ip/=5 ) THEN
            IF ( na/=0 ) THEN
               CALL RADBG(ido,ip,l1,idl1,Ch,Ch,Ch,C,C,Wa(iw))
            ELSE
               CALL RADBG(ido,ip,l1,idl1,C,C,C,Ch,Ch,Wa(iw))
            ENDIF
            IF ( ido==1 ) na = 1 - na
         ELSE
            ix2 = iw + ido
            ix3 = ix2 + ido
            ix4 = ix3 + ido
            IF ( na/=0 ) THEN
               CALL RADB5(ido,l1,Ch,C,Wa(iw),Wa(ix2),Wa(ix3),Wa(ix4))
            ELSE
               CALL RADB5(ido,l1,C,Ch,Wa(iw),Wa(ix2),Wa(ix3),Wa(ix4))
            ENDIF
            na = 1 - na
         ENDIF
         l1 = l2
         iw = iw + (ip-1)*ido
      ENDDO
      IF ( na==0 ) RETURN
      DO i = 1 , N
         C(i) = Ch(i)
      ENDDO
      END
!*==RFFTF1.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      SUBROUTINE RFFTF1(N,C,Ch,Wa,Ifac)
      USE FFTPACK_KIND
      IMPLICIT NONE
!*--RFFTF12321
!*** Start of declarations inserted by SPAG
      REAL C , Ch , FFTPACK_KIND , rk , Wa
      INTEGER i , idl1 , ido , Ifac , ip , iw , ix2 , ix3 , ix4 , k1 ,  &
            & kh , l1 , l2 , N , na , nf
!*** End of declarations inserted by SPAG
      DIMENSION Ch(*) , C(*) , Wa(*) , Ifac(*)
      nf = Ifac(2)
      na = 1
      l2 = N
      iw = N
      DO k1 = 1 , nf
         kh = nf - k1
         ip = Ifac(kh+3)
         l1 = l2/ip
         ido = N/l2
         idl1 = ido*l1
         iw = iw - (ip-1)*ido
         na = 1 - na
         IF ( ip==4 ) THEN
            ix2 = iw + ido
            ix3 = ix2 + ido
            IF ( na/=0 ) THEN
               CALL RADF4(ido,l1,Ch,C,Wa(iw),Wa(ix2),Wa(ix3))
            ELSE
               CALL RADF4(ido,l1,C,Ch,Wa(iw),Wa(ix2),Wa(ix3))
            ENDIF
         ELSEIF ( ip/=2 ) THEN
            IF ( ip==3 ) THEN
               ix2 = iw + ido
               IF ( na/=0 ) THEN
                  CALL RADF3(ido,l1,Ch,C,Wa(iw),Wa(ix2))
               ELSE
                  CALL RADF3(ido,l1,C,Ch,Wa(iw),Wa(ix2))
               ENDIF
            ELSEIF ( ip/=5 ) THEN
               IF ( ido==1 ) na = 1 - na
               IF ( na/=0 ) THEN
                  CALL RADFG(ido,ip,l1,idl1,Ch,Ch,Ch,C,C,Wa(iw))
                  na = 0
               ELSE
                  CALL RADFG(ido,ip,l1,idl1,C,C,C,Ch,Ch,Wa(iw))
                  na = 1
               ENDIF
            ELSE
               ix2 = iw + ido
               ix3 = ix2 + ido
               ix4 = ix3 + ido
               IF ( na/=0 ) THEN
                  CALL RADF5(ido,l1,Ch,C,Wa(iw),Wa(ix2),Wa(ix3),Wa(ix4))
               ELSE
                  CALL RADF5(ido,l1,C,Ch,Wa(iw),Wa(ix2),Wa(ix3),Wa(ix4))
               ENDIF
            ENDIF
         ELSEIF ( na/=0 ) THEN
            CALL RADF2(ido,l1,Ch,C,Wa(iw))
         ELSE
            CALL RADF2(ido,l1,C,Ch,Wa(iw))
         ENDIF
         l2 = l1
      ENDDO
      IF ( na==1 ) RETURN
      DO i = 1 , N
         C(i) = Ch(i)
      ENDDO
      END
!*==RFFTI1.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      SUBROUTINE RFFTI1(N,Wa,Ifac)
      USE FFTPACK_KIND
      IMPLICIT NONE
!*--RFFTI12391
!*** Start of declarations inserted by SPAG
      REAL arg , argh , argld , FFTPACK_KIND , fi , rk , tpi , Wa
      INTEGER i , ib , ido , Ifac , ii , ip , ipm , is , j , k1 , l1 ,  &
            & l2 , ld , N , nf , nfm1 , nl , nq , nr , ntry
      INTEGER ntryh
!*** End of declarations inserted by SPAG
      DIMENSION Wa(*) , Ifac(*) , ntryh(4)
      DATA ntryh(1) , ntryh(2) , ntryh(3) , ntryh(4)/4 , 2 , 3 , 5/
      nl = N
      nf = 0
      j = 0
 100  j = j + 1
      IF ( j<=4 ) THEN
         ntry = ntryh(j)
      ELSE
         ntry = ntry + 2
      ENDIF
 200  nq = nl/ntry
      nr = nl - ntry*nq
      IF ( nr/=0 ) GOTO 100
      nf = nf + 1
      Ifac(nf+2) = ntry
      nl = nq
      IF ( ntry==2 ) THEN
         IF ( nf/=1 ) THEN
            DO i = 2 , nf
               ib = nf - i + 2
               Ifac(ib+2) = Ifac(ib+1)
            ENDDO
            Ifac(3) = 2
         ENDIF
      ENDIF
      IF ( nl/=1 ) GOTO 200
      Ifac(1) = N
      Ifac(2) = nf
      tpi = 6.28318530717958647692D0
      argh = tpi/REAL(N,rk)
      is = 0
      nfm1 = nf - 1
      l1 = 1
      IF ( nfm1==0 ) RETURN
      DO k1 = 1 , nfm1
         ip = Ifac(k1+2)
         ld = 0
         l2 = l1*ip
         ido = N/l2
         ipm = ip - 1
         DO j = 1 , ipm
            ld = ld + l1
            i = is
            argld = REAL(ld,rk)*argh
            fi = 0.0D0
            DO ii = 3 , ido , 2
               i = i + 2
               fi = fi + 1.0D0
               arg = fi*argld
               Wa(i-1) = COS(arg)
               Wa(i) = SIN(arg)
            ENDDO
            is = is + ido
         ENDDO
         l1 = l2
      ENDDO
      END
!*==FFTPACK_KIND.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      MODULE FFTPACK_KIND
      IMPLICIT NONE
!*--FFTPACK_KIND2459
      INTEGER , PARAMETER :: RK = KIND(1.0D0)
      END
!*==SINT1.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      SUBROUTINE SINT1(N,War,Was,Xh,X,Ifac)
      USE FFTPACK_KIND
      IMPLICIT NONE
!*--SINT12466
!*** Start of declarations inserted by SPAG
      INTEGER i , Ifac , k , kc , modn , N , np1 , ns2
      REAL sqrt3 , t1 , t2 , War , Was , X , Xh , xhold
!*** End of declarations inserted by SPAG
      DIMENSION War(*) , Was(*) , X(*) , Xh(*) , Ifac(*)
      DATA sqrt3/1.73205080756887729352D0/
      DO i = 1 , N
         Xh(i) = War(i)
         War(i) = X(i)
      ENDDO
      IF ( N<2 ) THEN
         Xh(1) = Xh(1) + Xh(1)
      ELSEIF ( N==2 ) THEN
         xhold = sqrt3*(Xh(1)+Xh(2))
         Xh(2) = sqrt3*(Xh(1)-Xh(2))
         Xh(1) = xhold
      ELSE
         np1 = N + 1
         ns2 = N/2
         X(1) = 0.0D0
         DO k = 1 , ns2
            kc = np1 - k
            t1 = Xh(k) - Xh(kc)
            t2 = Was(k)*(Xh(k)+Xh(kc))
            X(k+1) = t1 + t2
            X(kc+1) = t2 - t1
         ENDDO
         modn = MOD(N,2)
         IF ( modn/=0 ) X(ns2+2) = 4.0D0*Xh(ns2+1)
         CALL RFFTF1(np1,X,Xh,War,Ifac)
         Xh(1) = 0.5D0*X(1)
         DO i = 3 , N , 2
            Xh(i-1) = -X(i)
            Xh(i) = Xh(i-2) + X(i-1)
         ENDDO
         IF ( modn==0 ) Xh(N) = -X(N+1)
      ENDIF
      DO i = 1 , N
         X(i) = War(i)
         War(i) = Xh(i)
      ENDDO
      END
!*==ZFFTB.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      SUBROUTINE ZFFTB(N,C,Wsave)
      USE FFTPACK_KIND
      IMPLICIT NONE
!*--ZFFTB2513
!*** Start of declarations inserted by SPAG
      REAL C , Wsave
      INTEGER iw1 , iw2 , N
!*** End of declarations inserted by SPAG
      DIMENSION C(1) , Wsave(1)
      IF ( N==1 ) RETURN
      iw1 = N + N + 1
      iw2 = iw1 + N + N
      CALL CFFTB1(N,C,Wsave,Wsave(iw1),Wsave(iw2))
      END
!*==ZFFTF.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      SUBROUTINE ZFFTF(N,C,Wsave)
      USE FFTPACK_KIND
      IMPLICIT NONE
!*--ZFFTF2528
!*** Start of declarations inserted by SPAG
      REAL C , Wsave
      INTEGER iw1 , iw2 , N
!*** End of declarations inserted by SPAG
      DIMENSION C(1) , Wsave(1)
      IF ( N==1 ) RETURN
      iw1 = N + N + 1
      iw2 = iw1 + N + N
      CALL CFFTF1(N,C,Wsave,Wsave(iw1),Wsave(iw2))
      END
!*==ZFFTI.spg  processed by SPAG 6.72Dc at 19:17 on 14 Sep 2021
      SUBROUTINE ZFFTI(N,Wsave)
      USE FFTPACK_KIND
      IMPLICIT NONE
!*--ZFFTI2543
!*** Start of declarations inserted by SPAG
      INTEGER iw1 , iw2 , N
      REAL Wsave
!*** End of declarations inserted by SPAG
      DIMENSION Wsave(1)
      IF ( N==1 ) RETURN
      iw1 = N + N + 1
      iw2 = iw1 + N + N
      CALL CFFTI1(N,Wsave(iw1),Wsave(iw2))
      END
